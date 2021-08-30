context("testing the final database")
library(dplyr)
library(stringr)
# load the database
fls <- list.files("data/data-out/", full.names = TRUE)
fls <- fls[which(file.mtime(fls) == max(file.mtime(fls)))]
db <- read.csv(fls, stringsAsFactors = FALSE)

# #Create db_expected and check vals are correct
# db_expected <-  data.frame(colnms = colnames(db)) %>%
#   filter(str_detect(colnms, "^X", negate = TRUE)) %>%
#   group_by(colnms) %>%
#   mutate(vals = colnms %>% {pull(db, .)} %>% str_split(", ") %>% unlist() %>%
#            as.character() %>%
#            ifelse(is.na(.), "NA", .) %>% unique() %>% paste0(collapse = ", ")) %>%
#   mutate(vals = vals %>% str_trunc(width = 200, side = "right", ellipsis = ""))

# write.csv(db_expected, "data/interim/expectedValues2.csv", quote = TRUE, row.names = FALSE)

db_expected <- read.csv("data/interim/expectedValues.csv")

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
             str_subset("") %>%
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
    mutate(vals = case_when(str_detect(colnms, "identified") ~ list(c(0, 1, 2, NA)),
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
    mutate(pass = pull(db, colnms) %>% as.Date() %>%
             {lubridate::is.Date(.) & . > as.Date("1980-01-01")} %>%
             all(na.rm = TRUE),
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
  missing_data <- db_expected %>% filter(na_allowed == 0) %>%
    group_by(colnms) %>%
    mutate(pass = pull(db, colnms)%>% is.na() %>% any() %>% `!`,
           nmissing = pull(db, colnms)%>% is.na() %>% sum())

  fail_mess <- filter(missing_data, !pass) %>%
    mutate(fail_mess = paste0("Column ", colnms," has ", nmissing, " missing values"))

  expect_true(all(missing_data$pass),
              label = paste0(fail_mess$fail_mess, collapse = " and \n\r"))
})

test_that("Data is internally consistent",{
  id <- db %>% filter(CC_not_mentioned == 1, CC_threat == 1) %>% pull(docID)

  expect_true(length(id) == 0,
              label = paste0("CC_not_mentioned and CC_threat are both 1 for docID ",
                             paste0(id, collapse = ", "), "\n"))

  id <- db %>% filter(CC_not_mentioned == 1, CC_unknown == 1) %>% pull(docID)

  expect_true(length(id) == 0,
              label = paste0("CC_not_mentioned and CC_unknown are both 1 for docID ",
                             paste0(id, collapse = ", "), "\n"))

  th_nums <- db %>% select(contains("identified")) %>% colnames() %>%
    str_extract("\\d\\d?\\.?\\d?") %>%
    as.numeric() %>% sort()

  notidedscpsev <- function(th_num){
    col_th <- db %>% select(starts_with("X")) %>% colnames() %>%
      str_remove("X\\d\\d?\\.?\\d?") %>% unique()

    col_th <- map(th_num, ~paste0("X", .x, col_th)) %>% unlist() %>%
      str_subset("notes|comments", negate = TRUE)

    tim <- db %>% select(all_of(col_th)) %>% select(contains("timing"))
    scp <- db %>% select(all_of(col_th)) %>% select(contains("scope"))
    sev <- db %>% select(all_of(col_th)) %>% select(contains("severity"))
    imp <- db %>% select(all_of(col_th)) %>% select(contains("impact"))
    ided <- db %>% select(all_of(col_th)) %>% select(contains("identified"))

    fail <- ifelse(ided == 0 & (scp != 0 & sev != 0 & tim > 2) &
                     (!is.na(scp) & !is.na(sev)),
                   TRUE, FALSE)
    id <- db %>% filter(fail) %>% select(docID, common_name) %>% mutate(th_num = th_num)

    if(length(id) > 0){
      return(id)
    } else {
      return(NULL)
    }

  }

  pass <- purrr::map_dfr(th_nums, notidedscpsev)

  expect_true(nrow(pass) == 0)
})


