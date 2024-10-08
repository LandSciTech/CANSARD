
library(dplyr)
library(stringr)
library(tidyr)
# load the database
dat_pth <- "data-raw"
# fls <- list.files(paste0(dat_pth, "data-out/"), full.names = TRUE)
# fls <- fls[which(file.mtime(fls) == max(file.mtime(fls)))]

db <- read.csv(here::here(dat_pth, "data-out/CAN-SAR_database.csv"), stringsAsFactors = FALSE)

# #Create db_expected and check vals are correct
# db_expected <-  data.frame(colnms = colnames(db)) %>%
#   filter(str_detect(colnms, "^X", negate = TRUE)) %>%
#   group_by(colnms) %>%
#   mutate(vals = colnms %>% {pull(db, .)} %>% str_split(", ") %>% unlist() %>%
#            as.character() %>%
#            ifelse(is.na(.), "NA", .) %>% unique() %>% paste0(collapse = ", ")) %>%
#   mutate(vals = vals %>% str_trunc(width = 200, side = "right", ellipsis = ""))

# write.csv(db_expected, "data/interim/expectedValues2.csv", quote = TRUE, row.names = FALSE)

db_expected <- read.csv(here::here(dat_pth, "data-out/expectedValues.csv"))

test_that("all values are in expected for that column", {
  # see db_expected for vars that have a specific list of possible values
  pass1 <- db_expected %>% filter(expectation == "vals",
                                  !colnms %in% c("CC_action_subtype", "action_subtype")) %>%
    group_by(colnms) %>%
    mutate(vals_in = pull(db, colnms)%>% str_split(", ") %>% unlist() %>%
             as.character() %>% ifelse(is.na(.), "NA", .) %>% unique() %>%
             list(),
           pass = vals_in[[1]] %in% (vals %>% str_split(", ") %>%
                                       unlist()) %>%
             all(),
           wch_fail = vals_in[[1]][which(!vals_in[[1]] %in%
                                           (vals %>% str_split(", ") %>%
                                              unlist()))] %>%
             list())

  # action subtypes are a bit different
  pass2 <- db_expected %>% filter(expectation == "vals",
                                  colnms %in% c("CC_action_subtype", "action_subtype")) %>%
    group_by(colnms) %>%
    mutate(vals_in = pull(db, colnms)%>% str_split(", ") %>% unlist() %>%
             as.character() %>%  str_trim() %>%
             str_remove(",") %>% tolower() %>% ifelse(is.na(.), "NA", .) %>%
             str_subset(".") %>%
             unique() %>% list(),
           pass = vals_in[[1]] %in% (vals %>% str_split(", ") %>%
                                       unlist()) %>%
             all(),
           wch_fail = vals_in[[1]][which(!vals_in[[1]] %in%
                                           (vals %>% str_split(", ") %>%
                                              unlist()))] %>%
             list())

  # for threats values can be 0, 1, 2 for identified or -2-5 for iucn cols
  pass_th <-  data.frame(colnms = colnames(db)) %>%
    filter(str_detect(colnms, "^X"),
           str_detect(colnms, "comments|note", negate = TRUE)) %>%
    group_by(colnms) %>%
    mutate(vals_in = colnms %>% {pull(db, .)} %>% unique() %>% list()) %>%
    mutate(vals = case_when(str_detect(colnms, "identified") ~ list(c(0, 1, 2, NA, "NE")),
                            TRUE ~ list(c(NA, -4:9 * 0.5, 5))),
           pass = vals_in[[1]] %in% vals[[1]] %>%
             all(),
           wch_fail = vals_in[[1]][which(!vals_in[[1]] %in% vals[[1]])] %>%
             list())

  pass <- bind_rows(pass1, pass2, pass_th %>% select(-vals))

  fail_mess <- filter(pass, !pass) %>%
    mutate(fail_mess = paste0(colnms, ": ", wch_fail %>% unlist() %>%
                                paste0(collapse = ", ")))

  expect_true(all(pass$pass),
              label = paste0("Values not matching expected were found:\n",
                             paste0(fail_mess$fail_mess, collapse = " and \n")))

  # other variables have a test they should pass in db_expected
  chr <- db_expected %>% filter(str_detect(expectation, "is.character")) %>%
    group_by(colnms) %>%
    mutate(pass = pull(db, colnms) %>% is.character(),
           fail = "Not is.character")

  num <- db_expected %>% filter(str_detect(expectation, "is.numeric")) %>%
    group_by(colnms) %>%
    mutate(pass = pull(db, colnms) %>% is.numeric(),
           fail = "Not is.numeric")

  uni <- db_expected %>% filter(str_detect(expectation, "unique")) %>%
    group_by(colnms) %>%
    mutate(pass = pull(db, colnms) %>% {length(.) == length(unique(.))},
           fail = "Not unique")

  datechr <- db_expected %>% filter(str_detect(expectation, "date")) %>%
    group_by(colnms) %>%
    mutate(pass1 = pull(db, colnms) %>% lubridate::ymd() %>%
             {lubridate::is.Date(.) & . > lubridate::ymd("1980-01-01")} %>%
             all(na.rm = TRUE),
           pass2 = setequal(pull(db, colnms) %>% lubridate::ymd() %>%
                              is.na() %>% which(),
                            pull(db, colnms) %>% is.na() %>% which()),
           pass = all(pass1, pass2),
           fail = "Not likely date")

  yr <- db_expected %>% filter(str_detect(expectation, "year")) %>%
    group_by(colnms) %>%
    mutate(pass = pull(db, colnms) %>% {. > 1980 & . < 2100} %>%
             all(na.rm = TRUE),
           fail = "Not likely year")

  pass4 <- bind_rows(chr, num, datechr, yr, uni)

  fail_mess2 <- filter(pass4, !pass) %>%
    mutate(fail_mess = paste0(colnms, ": ", fail))

  expect_true(all(pass4$pass),
              label = paste0(fail_mess2$fail_mess, collapse = " and \n\r"))
})

test_that("Data is not missing in required fields",{
  # Fields that are never NA
  missing_data <- db_expected %>% filter(na_allowed == 0) %>%
    group_by(colnms) %>%
    mutate(pass = pull(db, colnms)%>% is.na() %>% any() %>% `!`,
           nmissing = pull(db, colnms)%>% is.na() %>% sum())

  fail_mess <- filter(missing_data, !pass) %>%
    mutate(fail_mess = paste0("Column ", colnms," has ", nmissing, " missing values"))

  expect_true(all(missing_data$pass),
              label = paste0(fail_mess$fail_mess, collapse = " and \n\r"))

  # Fields not NA if SR
  col_sr <- c('report_writers', 'EOO', 'IAO', 'endemic_NA',
              'endemic_canada', 'continuous_USA', 'cosewic_status', 'ranges',
              'cosewic_examined_date')

  # Note locations can be NA because sometimes it is not determined in the sr

  missing_data_SR <- db_expected %>% filter(colnms %in% col_sr) %>%
    group_by(colnms) %>%
    mutate(pass = db %>% filter(doc_type == "COSEWIC Status Reports") %>%
             pull(colnms) %>% is.na() %>% any() %>% `!`,
           nmissing = db %>% filter(doc_type == "COSEWIC Status Reports") %>%
             pull(colnms) %>% is.na() %>% sum())

  fail_mess_SR <- filter(missing_data_SR, !pass) %>%
    mutate(fail_mess = paste0("Column ", colnms," has ", nmissing, " missing values"))

  expect_true(all(missing_data_SR$pass),
              label = paste0("Missing in Status reports:\n\r",
                             paste0(fail_mess_SR$fail_mess, collapse = " and \n\r")))

  # Fields not NA if RS
  col_rs <- c('Critical_habitat', 'action_subtype', 'CC_action', 'action_type')
  col_rs_cc <- c('CC_action_subtype', 'CC_action_type')

  missing_data_RS <- db_expected %>% filter(colnms %in% col_rs) %>%
    group_by(colnms) %>%
    mutate(pass = db %>% filter(doc_type == "Recovery Strategies") %>%
             pull(colnms) %>% is.na() %>% any() %>% `!`,
           nmissing = db %>% filter(doc_type == "Recovery Strategies") %>%
             pull(colnms) %>% is.na() %>% sum())

  missing_data_RS2 <- db_expected %>% filter(colnms %in% col_rs_cc) %>%
    group_by(colnms) %>%
    mutate(pass = db %>% filter(doc_type == "Recovery Strategies", CC_action == 1) %>%
             pull(colnms) %>% is.na() %>% any() %>% `!`,
           nmissing = db %>% filter(doc_type == "Recovery Strategies", CC_action == 1) %>%
             pull(colnms) %>% is.na() %>% sum())

  missing_data_RS <- bind_rows(missing_data_RS, missing_data_RS2)

  fail_mess_RS <- filter(missing_data_RS, !pass) %>%
    mutate(fail_mess = paste0("Column ", colnms," has ", nmissing, " missing values"))

  expect_true(all(missing_data_RS$pass),
              label = paste0("Missing in Recovery Strategies:\n\r",
                             paste0(fail_mess_RS$fail_mess, collapse = " and \n\r")))

  # Fields not NA if MP
  col_mp <- c('action_subtype', 'CC_action', 'action_type')
  col_mp_cc <- c('CC_action_subtype', 'CC_action_type')

  missing_data_MP <- db_expected %>% filter(colnms %in% col_mp) %>%
    group_by(colnms) %>%
    mutate(pass = db %>% filter(doc_type == "Management Plans") %>%
             pull(colnms) %>% is.na() %>% any() %>% `!`,
           nmissing = db %>% filter(doc_type == "Management Plans") %>%
             pull(colnms) %>% is.na() %>% sum())

  missing_data_MP2 <- db_expected %>% filter(colnms %in% col_mp_cc) %>%
    group_by(colnms) %>%
    mutate(pass = db %>% filter(doc_type == "Management Plans", CC_action == 1) %>%
             pull(colnms) %>% is.na() %>% any() %>% `!`,
           nmissing = db %>% filter(doc_type == "Management Plans", CC_action == 1) %>%
             pull(colnms) %>% is.na() %>% sum())

  missing_data_MP <- bind_rows(missing_data_MP, missing_data_MP2)

  fail_mess_MP <- filter(missing_data_MP, !pass) %>%
    mutate(fail_mess = paste0("Column ", colnms," has ", nmissing, " missing values"))

  expect_true(all(missing_data_MP$pass),
              label = paste0("Missing in Management Plans:\n\r",
                             paste0(fail_mess_MP$fail_mess, collapse = " and \n\r")))

  # threat ided NA, should always be 0 or 1
  th_l1_miss <- db %>% summarise(across(matches("^X\\d\\d?_threat_identified"),
                          ~sum(is.na(.x)))) %>%
    pivot_longer(everything()) %>%
    filter(value > 0)
  th_l2_miss <- db %>% summarise(across(c(matches("^X\\d\\d?\\..*_threat_identified"),
                          -c(X8.4_threat_identified, X8.5_threat_identified,
                             X8.6_threat_identified, X11.5_threat_identified)),
                          ~sum(is.na(.x)))) %>%
    pivot_longer(everything()) %>%
    filter(value > 0)

  expect_true(nrow(th_l1_miss) == 0,
              label = paste0("Level 1 threat data is missing: \n\r",
                             paste0(th_l1_miss$name, " has ", th_l1_miss$value,
                                    " missing values", collapse = " and \n\r")))

  expect_true(nrow(th_l2_miss) == 0,
              label = paste0("Level 2 threat data is missing: \n\r",
                             paste0(th_l2_miss$name, " has ", th_l2_miss$value,
                                    " missing values", collapse = " and \n\r")))

  # db %>% filter(if_all(matches("^X\\d\\d?\\..*_threat_identified"), is.na)) %>%
  #   View
  # db %>% filter(is.na(X7.2_threat_identified), !is.na(X1.1_threat_identified)) %>%
  #   View

  # tc = 1 then tc version not NA
  tc_v_miss <- db %>% filter(threat_calculator == 1, is.na(TC_version)) %>%
    pull(rowID)
  expect_true(length(tc_v_miss) == 0,
              label = paste0("TC_version is missing for rowID ",
                     paste0(tc_v_miss, collapse = ", "), "\n"))
})

test_that("Data is internally consistent",{
  id <- db %>% filter(CC_not_mentioned == 1, CC_threat == 1) %>% pull(rowID)

  expect_true(length(id) == 0,
              label = paste0("CC_not_mentioned and CC_threat are both 1 for rowID ",
                             paste0(id, collapse = ", "), "\n"))

  id <- db %>% filter(CC_not_mentioned == 1, CC_unknown == 1) %>% pull(rowID)

  expect_true(length(id) == 0,
              label = paste0("CC_not_mentioned and CC_unknown are both 1 for rowID ",
                             paste0(id, collapse = ", "), "\n"))

  id <- db %>%
    filter(CC_threat == 1, CC_relative_impact == 0 |is.na(CC_relative_impact)) %>%
    pull(rowID)

  expect_true(length(id) == 0,
              label = paste0("CC_threat is 1 and relative impact is not > 0 for rowID ",
                             paste0(id, collapse = ", "), "\n"))

  id <- db %>%
    filter(CC_threat == 0, CC_relative_impact > 0) %>%
    pull(rowID)

  expect_true(length(id) == 0,
              label = paste0("CC_threat is 0 and relative impact is > 0 for rowID ",
                             paste0(id, collapse = ", "), "\n"))

  th_nums <- db %>% select(contains("identified")) %>% colnames() %>%
    str_extract("\\d\\d?\\.?\\d?") %>%
    as.numeric() %>% sort()

  notidedscpsev <- function(th_num){
    col_th <- db %>% select(starts_with("X")) %>% colnames() %>%
      str_remove("X\\d\\d?\\.?\\d?") %>% unique()

    col_th <- purrr::map(th_num, ~paste0("X", .x, col_th)) %>% unlist() %>%
      str_subset("notes|comments", negate = TRUE)

    tim <- db %>% select(all_of(col_th)) %>% pull(contains("timing"))
    scp <- db %>% select(all_of(col_th)) %>% pull(contains("scope"))
    sev <- db %>% select(all_of(col_th)) %>% pull(contains("severity"))
    imp <- db %>% select(all_of(col_th)) %>% pull(contains("impact"))
    ided <- db %>% select(all_of(col_th)) %>% pull(contains("identified"))

    fail <- ifelse(ided == 0 & (scp != 0 & sev != 0 & tim > 2) &
                     (!is.na(scp) & !is.na(sev)),
                   TRUE, FALSE)
    id <- db %>% filter(fail) %>% select(rowID, common_name) %>% mutate(th_num = th_num)

    if(length(id) > 0){
      return(id)
    } else {
      return(NULL)
    }

  }
  fail <- purrr::map_dfr(th_nums, notidedscpsev)

  expect_true(nrow(fail) == 0,
              label = "Threats that should be identified based on scope, severity and timing are not")
  if(nrow(fail) > 0){
    print(fail)
  }
})

