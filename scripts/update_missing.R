library(dplyr)

# Make a csv with all rows with missing data #============================================
if(interactive()){
  has_missing <- function(df){
    col_all <- db_expected %>% filter(na_allowed == 0) %>% pull(colnms) %>%
      str_subset("url|rowID", TRUE)

    col_sr <- c('author', 'EOO', 'IAO', 'endemic_NA',
                'endemic_canada', 'continuous_USA', 'cosewic_status', 'ranges',
                'cosewic_examined_date')

    col_rs <- c('Critical_habitat', 'action_subtype', 'CC_action', 'action_type')
    col_rs_cc <- c('CC_action_subtype', 'CC_action_type')

    col_mp <- c('action_subtype', 'CC_action', 'action_type')
    col_mp_cc <- c('CC_action_subtype', 'CC_action_type')

    col_th1 <- db %>% select(matches("^X\\d\\d?_threat_identified")) %>%
      colnames()

    col_th2 <- db %>% select(matches("^X\\d\\d?\\..*_threat_identified"),
                             -c(X8.4_threat_identified, X8.5_threat_identified,
                                X8.6_threat_identified, X11.5_threat_identified)) %>%
      colnames()

    col_tc <- "TC_version"

    meta <- df %>% mutate(across(all_of(col_all), is.na)) %>%
      rowwise() %>%
      mutate(miss_all = col_all[which(c_across(all_of(col_all)))] %>%
               paste0(collapse = ", ")) %>%
      select(rowID, contains("miss"))

    sr <- df %>%
      filter(doc_type == "COSEWIC Status Reports") %>%
      mutate(across(all_of(col_sr), is.na)) %>%
      rowwise() %>%
      mutate(miss_sr = col_sr[which(c_across(all_of(col_sr)))] %>%
               paste0(collapse = ", ")) %>%
      select(rowID, contains("miss"))

    rs <- df %>%
      filter(doc_type == "Recovery Strategies") %>%
      mutate(across(all_of(col_rs), is.na)) %>%
      rowwise() %>%
      mutate(miss_rs = col_rs[which(c_across(all_of(col_rs)))] %>%
               paste0(collapse = ", ")) %>%
      select(rowID, contains("miss"))

    mp <- df %>%
      filter(doc_type == "Management Plans") %>%
      mutate(across(all_of(col_mp), is.na)) %>%
      rowwise() %>%
      mutate(miss_mp = col_mp[which(c_across(all_of(col_mp)))] %>%
               paste0(collapse = ", ")) %>%
      select(rowID, contains("miss"))

    rsmp_cc <- df %>%
      filter(doc_type %in% c("Management Plans", "Recovery Strategies"),
             CC_action == 1) %>%
      mutate(across(all_of(col_mp_cc), is.na)) %>%
      rowwise() %>%
      mutate(miss_rsmp_cc = col_mp_cc[which(c_across(all_of(col_mp_cc)))] %>%
               paste0(collapse = ", ")) %>%
      select(rowID, contains("miss"))

    tc <- df %>%
      filter(threat_calculator == 1) %>%
      mutate(across(all_of(col_tc), is.na)) %>%
      rowwise() %>%
      mutate(miss_tcv = col_tc[which(c_across(all_of(col_tc)))] %>%
               paste0(collapse = ", ")) %>%
      select(rowID, contains("miss"))

    th1 <- df %>% mutate(across(all_of(col_th1), is.na)) %>%
      rowwise() %>%
      mutate(miss_th1 = ifelse(any(c_across(all_of(col_th1))),
                               "Level 1 threats", "")) %>%
      select(rowID, contains("miss"))

    th2 <- df %>% mutate(across(all_of(col_th2), is.na)) %>%
      rowwise() %>%
      mutate(miss_th2 = ifelse(any(c_across(all_of(col_th2))),
                               "Level 2 threats", "")) %>%
      select(rowID, contains("miss"))

    df_miss <- purrr::reduce(lst(meta, sr, rs, mp, rsmp_cc, tc, th1, th2),
                             left_join, .init = df, by = "rowID") %>%
      mutate(across(contains("miss"), ~replace_na(.x, ""))) %>%
      rowwise() %>%
      filter(!all(purrr::map_lgl(c_across(contains("miss")),
                                 ~is.na(.x)|.x == ""))) %>%
      relocate(contains("miss"), .before = rowID)

  }
  db_miss <- has_missing(db)

  write.csv(db_miss, paste0(dat_pth, "interim/CAN_SARD_missing_data.csv"),
            row.names = FALSE)

}

# Bring in missing data that has been updated #=============================================
miss <- read.csv("data-raw/data-raw/CAN_SARD_missing_data_updates_2021_09_16.csv")

db <- read.csv("data-raw/data-out/CAN_SARD.csv", stringsAsFactors = FALSE)

# TODO on reading in when they have been used: determine action_types and
# CC_action_type from action_subtypes, process tc data

# filter miss to just data that has been updated and rename ID cols to match
miss2 <- miss %>% rename(rowID = docID, speciesID = uID) %>%
  select(rowID, speciesID, everything()) %>%
  filter(change_made != "") %>%
  select(-change_made, -matches("^miss"), -url)

db <- rows_patch(db, miss2, by = "rowID")

# run test-database with this db object to check that updates worked as expected

# save a new version of db
write.csv(db, paste0(dat_pth, "data-out/CAN_SARD.csv"), row.names = FALSE)
