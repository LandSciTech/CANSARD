# Combine the existing database with the updated versions for species after
# 2018 and with level 2 threats
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(purrr)
# Load all functions in the R directory
devtools::load_all(".")

# Download the data from google drive
# https://drive.google.com/drive/folders/1p3SYAWkgI4GX8wsaw3UgXuf_twmuedHK?usp=sharing

# unzip the file into data-raw/data-raw Only do once
# unzip("~/../Downloads/drive-download-20210901T194224Z-001.zip",
#       exdir = "data-raw/data-raw")

# Get the current version of the db from SAR_Climate_Change folder
db_2018 <- read.csv("../SAR_climate_change/SE_version/Threats_and_recovery/Analyses/data/data_wkg/Can_SAR_Database.csv")

# Get the data extracted from docs published after 2018 or species listed after
# 2018
# Have to specify some col types that are being read wrong
nms <- read_xlsx("data-raw/data-raw/Can_SAR_data_extraction v.current.xlsx",
                 sheet = "Extracted Data", skip = 2,
                 na = "NA",
                 n_max = 0) %>% names()
ct <- case_when(str_detect(nms, "Date|DATE|date") ~ "date",
                str_detect(nms, "year|ID") ~ "numeric",
                TRUE ~ "guess")
# TC_date needs to be dealt with separately
ct[which(nms == "TC_date")] <- "guess"

db_2021 <- read_excel("data-raw/data-raw/Can_SAR_data_extraction v.current.xlsx",
                      sheet = "Extracted Data", skip = 2,
                      na = c("NA",  "-"),
                      col_types = ct)

# Fix TC_date Comes in as character with mixture of excel numeric
# dates and years
db_2021 <- mutate(
  db_2021,
  TC_date = case_when(nchar(TC_date) == 6 ~ str_replace(TC_date, "\\.0", ""),
                      TRUE ~ TC_date) %>%
    janitor::convert_to_date(
      character_fun = function(x){
        lubridate::parse_date_time(x, orders = c("Y", "Ydm"), truncated = 3) %>%
          as.Date()
      })
)

# Action table from google drive
action_table <- read_excel("data-raw/data-raw/Can_SAR_data_extraction v.current.xlsx",
                           sheet = "Action Types", skip = 1) %>%
  select(`Type of action`, `Action sub-types`)

# Threats data for the 2021 db
db_2021_thFJ <- read_excel("data-raw/data-raw/Can_SAR_data_extraction v.current.xlsx",
                         sheet = "Threats Calculator Data",
                         range = "A1:I7828",
                         #col_types = c(rep("guess", 9), "date", rep("guess", 7)),
                         na = "NA") %>%
  mutate(threat_num = str_replace(threat_num, ",", ".") %>% as.numeric(),
         source = "Hand Extraction")

# get TC_date and docID from the main spreadsheet
db_2021_tolink <- db_2021 %>% filter(threat_calculator == 1) %>%
  select(uID, TC_date, docID) %>%
  distinct() %>%
  filter(!docID %in% c(232, 101, 276, 269, 122, 344, 256))

db_2021_thFJ2 <- left_join(db_2021_thFJ,
                           db_2021_tolink,
                           by = "uID")

db_2021_thmissing <-read_excel("data-raw/data-raw/Can_SAR_data_extraction v.current.xlsx",
                               sheet = "TC to Add",
                               #col_types = c(rep("guess", 9), "date", rep("guess", 7)),
                               na = "NA") %>%
  mutate(source = "Hand Extraction")

db_2021_thFJ3 <- bind_rows(db_2021_thFJ2, db_2021_thmissing)

# Threats data automatically extracted from threats calculator spreadsheets
# provided by CWS with uIDs added by hand by FJ
db_2021_thCWS <- read_excel("data-raw/data-raw/Can_SAR_data_extraction v.current.xlsx",
                            sheet = "TC Spreadsheet Data",
                            range = "A1:Q8358",
                            na = "NA") %>%
  mutate(source = "Spreadsheet Extraction") %>%
  filter(!is.na(uID)) %>%
  rename_with(~paste0("TC_", .x),
              c(references, assessors, calculated_overall_impact,
                assigned_overall_impact, impact_adjustment_reasons,
                overall_comments))

# ones with missing uID are the Athabasca Endemics bundle but this TC
# was extracted by Florence so not needed here

# TCs from spreadsheet to add to specific docs
db_2021_addCWS <- db_2021_thCWS %>% filter(uID %in% c(316, 827, 829)) %>%
  mutate(docID = case_when(uID == 316 ~ 232,
                           uID == 827 ~ 101,
                           uID == 829 ~ 276)) %>%
  left_join(db_2021 %>% select(uID, common_name) %>% distinct(), by = "uID") %>%
  select(-species, -threat_name)

db_2021_thFJ4 <- bind_rows(db_2021_thFJ3, db_2021_addCWS)


# confirm by hand that docIDs are correct for anything with dup uID
# db_2021_thFJ2 %>%
#   group_by(uID, docID) %>%
#   slice(1) %>%
#   group_by(uID) %>%
#   filter(n() > 1) %>%
#   arrange(uID) %>%
#   write.csv("data-raw/interim/check_dub_tcs.csv")

# filter the noTC and TC spreadsheets out of db_2021 before join. Need to
# consider how to connect TC spreadsheet with docID (should work with TC_date)
# add missings to a new sheet
# TC data and same are fine as they are

# Master list of uID, common name and species
uID_list <- read_excel("data-raw/data-raw/list uID.xlsx")

# check that each uID threat_num is unique
db_2021_thCWS %>%
  group_by(uID, threat_num) %>%
  summarise(N = n()) %>%
  filter(N > 1) %>% nrow() == 0

# Different Docs can use the same TC
db_2021_thFJ4 %>%
  group_by(uID, threat_num, TC_date, docID) %>%
  summarise(N = n()) %>%
  filter(N > 1) %>% nrow() == 0

# Fixed duplicates in source in uID 1001 in FJ there were 2 1.1s and one should
# be 1.0. For uID 117 it was incorrectly assigned. Should have been 116.
# Ignore NAs in uID they are correct in the FJ version


# combine the two data sets by adding in rows of CWS data that are not in FJ
db_2021_thCWS_notFJ <- anti_join(db_2021_thCWS, db_2021_thFJ4,
                                 by = c("uID", "threat_num", "TC_date")) %>%
  select(-threat_name, -species)

db_2021_th <- bind_rows(db_2021_thFJ4,
                          db_2021_thCWS_notFJ)

# 2021 Threats Data -------------------------------------------------------

# some threat levels do not match look at non-matching entries and fix. Column
# wch is which column has the problem
check_threats(db_2021_th)

db_2021_th <- db_2021_th %>%
  mutate(across(c(impact, scope, severity, timing),
                ~.x %>% str_replace("Hig$", "High") %>%
                  str_replace("Ongoing", "High") %>%
                  str_replace("Smal$", "Small") %>%
                  str_replace("Neutral or potentially beneficial",
                              "Neutral or potential benefit") %>%
                  str_replace("Unkown", "Unknown") %>%
                  str_replace("Sight", "Slight") %>%
                  str_replace("Neglibible", "Negligible"))) %>%
  mutate(scope = str_replace(scope, "Medium", "Restricted") %>%
           str_replace("Serious - Moderate", "Large - Restricted"))

# format the threats to fit with the 2018 database
db_2021_th_2 <- format_threats(db_2021_th)

db_2021_th_3 <- filter(db_2021_th_2, if_any(matches("X.*iucn"), ~!is.na(.x)))

# # investigate duplicates
# db_2021_th_3 %>%
#   group_by(uID) %>%
#   filter(n() > 1) %>% View()
# # some are the same but just have TC_date NA in hand extracted version
# db_2021_th_3dups <- db_2021_th_3 %>%
#   group_by(uID) %>%
#   filter(n() > 1)
# db_2021_th_3dups <- db_2021_th_3dups %>%
#   distinct(across(-c(TC_date, source)), .keep_all = TRUE) %>%
#   filter(n() > 1)
# # removes 64 duplicates but still 24
# showDifs <- function(df, id){
#   df %>% filter(uID == id) %>% mutate(across(everything(), as.character)) %>%
#     pivot_longer(c(-uID, -source)) %>%
#     pivot_wider(names_from = source, values_from = value) %>%
#     filter(`Hand Extraction` != `Spreadsheet Extraction`)
# }
#
# dupDifs <- purrr::map_dfr(unique(db_2021_th_3dups$uID), showDifs,
#                           df = db_2021_th_3dups)
#
# write.csv(dupDifs, "data-raw/data-raw/reconcileDuplicates.csv", row.names = FALSE)
#
# # those with keep_both == 0 are the same with mostly changes from NA to not
# # applicable. Those where there was a real difference I checked manually and
# # recorded the docID
# dupDifs <- read.csv("data-raw/data-raw/reconcileDuplicates.csv")
#
# true_dups <- filter(dupDifs, keep_both == 0) %>% pull(uID) %>% unique()
#
# # remove true_dups and those where only the TC_date and source differ
# db_2021_th_4 <- db_2021_th_3 %>%
#   distinct(across(-c(TC_date, source)), .keep_all = TRUE) %>%
#   slice(-which(.$uID %in% true_dups &
#                 .$source == "Hand Extraction"))

# In the Extracted Data spreadsheet adjusted the TC_date so that it will be
# different for the ones that are from different docs


# Check that uid/docID is unique
db_2021_th_3 %>%
  group_by(uID, docID) %>%
  filter(n() > 1) %>% {nrow(.) == 0} %>% stopifnot()

# Prepare the 2021 database -----------------------------------------------

# Filter empty rows that were either docs that were not available or species
# that were not listed
db_2021_2 <- db_2021 %>% filter(!(is.na(web_pub_date)|is.na(doc_citation)|
                                    is.na(date_last_access))) %>%
  # remove columns that were just for data extraction
  select(-year_rs_mp_data)
# TODO: check what CC_unknown variables should be for Not Listed species?
# Some Not Listed species had data extracted but most did not. Only keep the
# ones with data extracted

# Check that uID, and TC_date and docID uniquely identify *This ought to be docID but we
# didn't include it in TC datasheet. Problem is that sometimes the RS and SR
# used same TC I added docID into threats above so should work
db_2021_2 %>% group_by(uID, TC_date, docID) %>%
  filter(n() > 1) %>% {nrow(.) == 0} %>% stopifnot()

db_2021_3 <- coalesce_join(db_2021_2, db_2021_th_3, by = c("uID", "docID"))

db_2021_4 <- coalesce_join(db_2021_3,
                           uID_list %>% select(uID, species, common_name),
                           by = "uID",
                           join = left_join)

# add action_type and CC_action_type based on subtypes.
action_table2 <- action_table %>%
  mutate(`Action sub-types` = tolower(`Action sub-types`)) %>%
  group_by(`Type of action`) %>%
  nest() %>%
  mutate(data = map(data, ~paste0(.x$`Action sub-types`,
                                  collapse = "|")))

db_2021_5 <- db_2021_4 %>%
  mutate(CC_action_subtype = str_replace(CC_action_subtype,
                                         regex("manage human threats",
                                               ignore_case = TRUE),
                                         "regulate human activities") %>%
           str_replace(regex("manage predation", ignore_case = TRUE),
                       "Manage native species negatively impacting species at risk"),
         action_subtype = str_replace(action_subtype,
                                      regex("manage human threats",
                                            ignore_case = TRUE),
                                      "regulate human activities") %>%
           str_replace(",,|, ,", ",") %>%
           str_replace(regex("manage predation", ignore_case = TRUE),
                       "Manage native species negatively impacting species at risk")) %>%
  mutate(action_type = map2_dfc(action_table2$data,
                                action_table2$`Type of action`,
                               ~ifelse(str_detect(action_subtype %>% tolower(),
                                  .x), .y, NA)),
         CC_action_type = map2_dfc(action_table2$data,
                                   action_table2$`Type of action`,
                                   ~ifelse(str_detect(CC_action_subtype %>% tolower(),
                                                      .x), .y, NA))) %>%
  mutate(action_type = paste(action_type$...1, action_type$...2,
                             action_type$...3, action_type$...4,
                             sep = ", ") %>%
           str_replace("^NA, NA, NA, NA$", NA_character_) %>%
           str_replace_all("NA, |, NA|NA", ""),
         CC_action_type = paste(CC_action_type$...1, CC_action_type$...2,
                                CC_action_type$...3, CC_action_type$...4,
                                sep = ", ") %>%
           str_replace("^NA, NA, NA, NA$", NA_character_) %>%
           str_remove_all("NA, |, NA|NA"))


rm(db_2021, db_2021_2, db_2021_3, db_2021_4, db_2021_th, db_2021_th_2,
   db_2021_th_3, db_2021_thCWS, db_2021_thCWS_notFJ, db_2021_thFJ,
   db_2021_thFJ2)
# Reformat 2018 database --------------------------------------------------

# Split into chunks for each document, SR, RS, MP, AP rename and filter columns
# and then bind together
rs_cols <- c("RS", "RS_final", "RS_amended", "RS_year", "Critical_habitat",
             "CC_action",	"action_type", "CC_action_type", "action_subtype",
             "action_subtype_CC")
mp_cols <- c("MP", "MP_final", "MP_year",
             "CC_action",	"action_type", "CC_action_type", "action_subtype",
             "action_subtype_CC")
ap_cols <- c("AP", "AP_year")

sr_cols <- setdiff(colnames(db_2018), c(rs_cols, mp_cols, ap_cols))

db_2018_sr <- select(db_2018, all_of(sr_cols)) %>%
  mutate(docID = seq(500, length.out = n()),
         doc_type = "COSEWIC Status Reports",
         doc_citation = NA_character_,
         url = NA_character_,
         web_pub_date = as.Date(NA),
         year_published = year_assessment,
         date_last_access = as.Date("2018-02-22"),
         status_appraisal_rapid_review = ifelse(is.na(status_appraisal), 0, 1),
         final = 1,
         amendment = 0) %>%
  # All species with out a report in old db are in new db so remove old version
  filter(COSEWIC_report == 1) %>%
  #remove columns that are no longer in use
  select(-all_of(c("COSEWIC_report", "genus", "taxonomic_group", "year_assessment",
            "cosewic_designation", "designation_change", "year_order_council",
            "status_appraisal", "former_cosewic_designation",
            "year_former_cosewic_assessment", "cosewic_designation_change",
            "former_sara_status", "sara_status_change", "canadian_range",
            "AB", "BC", "MB", "NB", "NL", "NS", "NU", "NWT", "ON", "PEI", "QC",
            "SK", "YT", "arctic", "atlantic", "pacific"))) %>%
  #reorder cols to match 2021
  select(intersect(colnames(db_2021_5), colnames(.)))

db_2018_rs <- select(db_2018, uID, common_name, species, large_taxonomic_group,
                     all_of(rs_cols)) %>%
  filter(!is.na(RS_year)) %>%
  mutate(year_published = ifelse(is.na(RS_amended), RS_year, RS_amended),
         final = RS_final,
         amendment = ifelse(is.na(RS_amended), 0, 1),
         CC_action_subtype = action_subtype_CC,
         docID = seq(1500, length.out = n()),
         doc_type = "Recovery Strategies",
         doc_citation = NA_character_,
         url = NA_character_,
         web_pub_date = as.Date(NA),
         date_last_access = as.Date("2018-02-22"),
         status_appraisal_rapid_review = 0) %>%
  select(-c(RS, RS_final, RS_amended, RS_year, action_subtype_CC))


db_2018_mp <- select(db_2018, uID, common_name, species, large_taxonomic_group,
                     all_of(mp_cols))%>%
  # remove sp that don't have an MP
  filter(!is.na(MP_year)) %>%
  mutate(year_published = MP_year,
         final = MP_final,
         amendment = 0,
         CC_action_subtype = action_subtype_CC,
         docID = seq(2000, length.out = n()),
         doc_type = "Management Plans",
         doc_citation = NA_character_,
         url = NA_character_,
         web_pub_date = as.Date(NA),
         date_last_access = as.Date("2018-02-22"),
         status_appraisal_rapid_review = 0) %>%
  select(-c(MP, MP_final, MP_year, action_subtype_CC))

db_2018_refor <- bind_rows(db_2018_sr, db_2018_rs, db_2018_mp) %>%
  arrange(uID)

rm(db_2018, db_2018_mp, db_2018_rs, db_2018_sr)

# Add level 2 threats for 2018 database -----------------------------------
# level 2 threats identified in docs when no TC
db_2018_l2 <- read_excel("data-raw/data-raw/Level 2 threats for 2018 database.xlsx",
                         sheet = "Threats data (2018)",
                         col_types = c(rep("guess", 9), "text", rep("guess", 73)),
                         skip = 1) %>%
  filter(`new data` == 0,
         threat_calculator == 0,
         !is.na(doc_citation)) %>%
  mutate(
    web_pub_date = str_replace(web_pub_date, "\\.0", "") %>%
      janitor::convert_to_date(
        character_fun = function(x){
          lubridate::parse_date_time(x, orders = c("Y", "Ydm"), truncated = 3) %>%
            as.Date()
        }),
    Doc_type = case_when(Doc_type == "Recovery Strategy" ~ "Recovery Strategies",
                         Doc_type == "COSEWIC Status Report" ~ "COSEWIC Status Reports",
                         Doc_type == "Management Plan" ~ "Management Plans")
  ) %>%
  rename(doc_type = Doc_type) %>%
  rename_with(~str_replace(.x, "_threat_notes", " notes")) %>%
  select(-docID) %>%
  mutate(across(contains("identified"), as.numeric))

# meta data from l2 sheet for ones with tc
db_2018_l2_tc <- read_excel("data-raw/data-raw/Level 2 threats for 2018 database.xlsx",
                         sheet = "Threats data (2018)",
                         col_types = c(rep("guess", 9), "text", rep("guess", 73)),
                         skip = 1) %>%
  filter(`new data` == 0, threat_calculator == 1, !is.na(doc_citation)) %>%
  mutate(
    web_pub_date = str_replace(web_pub_date, "\\.0", "") %>%
      janitor::convert_to_date(
        character_fun = function(x){
          lubridate::parse_date_time(x, orders = c("Y", "Ydm"), truncated = 3) %>%
            as.Date()
        }),
    Doc_type = case_when(Doc_type == "Recovery Strategy" ~ "Recovery Strategies",
                         Doc_type == "COSEWIC Status Report" ~ "COSEWIC Status Reports",
                         Doc_type == "Management Plan" ~ "Management Plans")
  ) %>%
  rename(doc_type = Doc_type) %>%
  rename_with(~str_replace(.x, "_threat_notes", " notes"))%>%
  mutate(across(contains("identified"), as.numeric))

# setdiff(colnames(db_2018_l2), colnames(db_2018_refor))

#uniquely ided by uID and doc_type?
db_2018_l2 %>% group_by(uID, doc_type) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()

db_2018_refor %>% group_by(uID, doc_type) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()

# level 2 threats in recovery docs when tc used
db_2018_th <- read_excel("data-raw/data-raw/Level 2 threats for 2018 database.xlsx",
                         sheet = "TC data (RS-MP 2018)",
                         na = "NA") %>%
  mutate(source = "Hand Extraction")

db_2018_th %>% group_by(uID, threat_num) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()

# updated in source TC data uID 515 had 1.1 twice instead of 1.1 and 11.1. Also
# had to change format of threat_num col so it wasn't a date

# format threats for database
db_2018_th_2 <- format_threats(db_2018_th)

# need to add new columns to db_2018_l2_tc
cols_to_add <- setdiff(colnames(db_2018_th_2), colnames(db_2018_l2_tc))[-1:-2]

alt_df <- as.data.frame(matrix(ncol = length(cols_to_add)))
colnames(alt_df) <- cols_to_add

db_2018_l2_tc2 <- bind_cols(db_2018_l2_tc, alt_df) %>%
  # update threats into db_2018_l2_tc
  rows_update(db_2018_th_2 %>% select(-c(Year, source)),
              by = c("uID", "docID", "common_name"))

db_2018_l2_tc2 %>% group_by(uID, doc_type) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()
# changed in source uID 398 Year was 2016 instead of 2017 for 1 threat_num

db_2018_refor %>% group_by(uID, doc_type) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()

db_2018_l2 %>% group_by(uID, doc_type) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()
# combine all three into one. For l2 threats FJ made some changes to level 1 threats as well so
# should update. Should not be any uIDs that are not in db_2018

# need to add new columns to db_2018_refor
cols_to_add <- c(setdiff(setdiff(colnames(db_2018_l2_tc2),
                                 colnames(db_2018_l2))[-1],
                 colnames(db_2018_refor)),
                 setdiff(colnames(db_2018_l2), colnames(db_2018_refor))[-1:-2])

alt_df <- as.data.frame(matrix(ncol = length(cols_to_add)))
colnames(alt_df) <- cols_to_add
db_2018_refor_2 <- bind_cols(db_2018_refor, alt_df)

db_2018_refor_3 <- db_2018_refor_2 %>%
  rows_update(db_2018_l2 %>% select(-`new data`, -`author SR`) %>%
                # select rows with match in db2018
                semi_join(db_2018_refor_2, by = c("uID", "doc_type")),
              by = c("uID", "doc_type"))

# use patch becasue some already have tc data that was not in l2
db_2018_refor_4 <- db_2018_refor_3 %>%
  rows_patch(db_2018_l2_tc2 %>% select(-`new data`, -`author SR`, -docID) %>%
                # select rows with match in db2018
                semi_join(db_2018_refor_3, by = c("uID", "doc_type")),
              by = c("uID", "doc_type"))

# Add in Rs and Mp that are missing from 2018
# looks like some are situations where there is an RS for SC species or MP for a
# Th or En species which were initially ignored. Some RS and MP seem to have
# been missed will add now but CC_action etc will be missing. This is the case
# for many 2018 sp though

# need to get species, and some other data before adding.
toinsert <- db_2018_l2_tc %>% select(-`new data`, -`author SR`) %>%
  anti_join(db_2018_refor_4,
            by = c("uID", "doc_type")) %>%
  mutate(docID = seq(2500, length.out = n()))

db_2018_refor_5 <- db_2018_refor_4 %>%
  rows_insert(toinsert,
              by = c("uID", "doc_type")) %>%
  mutate(across(matches("X\\d\\d?_.*identified"), as.numeric))

# check docIDs are unique
db_2018_refor_5 %>% group_by(docID) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()

# Combine 2018 and 2021 databases -----------------------------------------

setdiff(colnames(db_2018_refor_5), colnames(db_2021_5))
setdiff(colnames(db_2021_5), colnames(db_2018_refor_5))

# Threats calculators that did not match a species in 2021 db don't have a docID
# might match one in 2018 db
no_doc_2021 <- db_2021_5 %>% filter(is.na(docID))

# there is only 1 of each uID and they would have come from SRs only
no_doc_2021 %>% group_by(uID) %>% filter(n() > 1)

db_2018_tc_to_add <- db_2018_refor_5 %>%
  filter(doc_type == "COSEWIC Status Reports", threat_calculator == 1)
# also only 1 uID
db_2018_tc_to_add %>% group_by(uID) %>% filter(n() > 1)

semi_join(no_doc_2021, db_2018_tc_to_add, by = "uID")

semi_join(db_2018_tc_to_add, no_doc_2021, by = "uID")

inner_join(db_2018_tc_to_add, no_doc_2021, by = "uID") %>%
  filter(as.numeric(year_published.x) >= lubridate::year(TC_date)) %>%
  select(year_published.x, TC_date)
# year_published is greater than TC_date for all so seems likely it was used

filter(semi_join(db_2018_tc_to_add, no_doc_2021, by = "uID"),
       if_all(matches("X.*iucn"), ~is.na(.x)))
# all the ones that overlap have data in iucn fields

# So if bind_rows 2021 with doc and 2018 that will add the TC_cols as NA and
# then use patch to update them
db_final <- db_2021_5 %>% filter(!is.na(docID)) %>%
  bind_rows(db_2018_refor_5)

db_final_tc_to_add <- db_final %>%
  filter(doc_type == "COSEWIC Status Reports",
         threat_calculator == 1,
         is.na(TC_date),
         is.na(doc_citation))

db_final_tc_to_add <- rows_patch(db_final_tc_to_add,
             semi_join(no_doc_2021, db_final_tc_to_add, by = "uID"),
             by = "uID")

db_final_2 <- rows_patch(db_final, db_final_tc_to_add,
                         by = c("uID", "docID"))

# look for duplicates based on year published and uID
# db_final_2 %>% group_by(uID, year_published, doc_type) %>%
#   filter(n() > 1) %>%
#   View()
# These were re-published online after the 2018 data but were not actually new
# we will keep the newly extracted versions since they have more data

db_final_3 <- db_final_2 %>% group_by(uID, year_published, doc_type) %>%
  filter(!(n() > 1 & is.na(web_pub_date)))

# Prairie Skink old RS has no threats data. Shouldn't it have been added with level2?
 # Threats were not extracted from old versions if the new one was included in 2021 db

# filter out old docs that are not final when there is a newer version
db_final_4 <- db_final_3 %>% group_by(uID, doc_type) %>%
  filter(!(n() > 1 & final == 0 & is.na(web_pub_date))) %>%
  ungroup()

# there are still some RS that do not have threats. Less clear sometimes what
# constitutes a new document. Ie round pigtoe had ammended RS in 2016 and then
# another amendment in 2019. No threat data for the 2016 one. Also seem FJ did
# not extract threats for RS if species was in new data even if doc in new data
# was just SR

# # find missing TC data captured in test-database now
# filter(db_final_4, if_all(matches("X\\d\\..*identified"), ~is.na(.x)),
#        !sara_status %in% c("Not listed"), is.na(doc_citation)) %>%
#   write.csv("missingLevel2.csv", row.names = FALSE)

# connect to Sar DMS data
leg_list_data <- read_xlsx("data-raw/data-raw/SARB_legal_list_2021_04_15.xlsx")

col_ll <- c('COS_ID', 'LEG_ID', 'LEGAL NAME POPULATION (E)',
            'SCIENTIFIC NAME (E)', 'COSEWIC COMMON NAME POPULATION (E)',
            'TAXONMIC GROUP (E)', 'COSEWIC STATUS (E)', 'SARA STATUS (E)',
            'RANGES (E)', 'COSEWIC EXAMINED DATE {E}', 'DATE OF LISTING (E)',
            'GIC DECISION (E)')

leg_list_data <- leg_list_data %>%
  select(all_of(col_ll)) %>%
  rename_all(~str_replace(.x, " .E.$", "_sardms") %>%
               str_replace_all(" ", "_") %>% str_to_lower()) %>%
  mutate(cosewic_common_name_population_sardms =
           str_remove_all(cosewic_common_name_population_sardms,
                          "\\(|\\)") %>%
           str_replace_all("’", "'") %>%
           str_replace_all(" \\-| \\- |\\- ", "-") %>%
           str_replace_all(" \\-| \\- |\\- ", "-") %>%
           str_replace_all(" – ", "-") %>%
           str_replace_all(" \\/| \\/ |\\/ ", "/")%>%
           str_replace_all(" \\/| \\/ |\\/ ", "/") %>%
           tolower()) %>%
  mutate(across(where(~is(.x,"POSIXct")), ~as.character(as.Date(.x))))

# Fix spelling of names to match SAR DMS
db_final_5 <- db_final_4 %>%
  mutate(common_name = common_name %>%
           str_replace_all(" -| - |- ", "-") %>%
           str_replace_all(" \\-| \\- |\\- ", "-") %>%
           str_replace_all(" \\/| \\/ |\\/ ", "/") %>%
           str_replace_all(" /| / |/ ", "/") %>%
           str_replace_all("’", "'") %>%
           str_replace_all("Coastrange Sculpin Cultus population",
                           "Coastrange Sculpin Cultus Lake population") %>%
           str_replace_all("Dolphin and Union Caribou",
                           "Caribou Dolphin and Union population") %>%
           tolower() %>%
           str_replace("c stickleback",
                       "c threespine stickleback") %>%
           str_replace("grey fox", "gray fox") %>%
           str_squish() %>%
           str_replace("northern resident killer whales",
                       "killer whale northeast pacific northern resident population") %>%
           str_replace("offshore killer whale",
                       "killer whale northeast pacific offshore population") %>%
           str_replace("southern resident killer whales",
                       "killer whale northeast pacific southern resident population") %>%
           str_replace("northern saw-whet owl subspecies",
                       "northern saw-whet owl brooksi subspecies") %>%
           str_replace_all("rocky mountain sculpin eastslope \\(missouri river\\) populations",
                       "rocky mountain sculpin missouri river populations") %>%
           str_replace_all("rocky mountain sculpin eastslope \\(saskatchewan-nelson river\\) populations",
                       "rocky mountain sculpin saskatchewan-nelson river populations") %>%
           str_replace_all("rocky mountain sculpin westslope \\(pacific\\) populations",
                       "rocky mountain sculpin pacific populations") %>%
           str_replace("wavyrayed lampmussel", "wavy-rayed lampmussel") %>%
           str_replace("western toad non.calling population",
                       "western toad non-calling population") %>%
           str_replace("westslope cutthroat trout alberta population \\(or saskatchewan-nelson river populations 2019\\)",
                       "westslope cutthroat trout saskatchewan-nelson rivers populations") %>%
           str_replace("island blue insulanus subspecies", "island blue") %>%
           str_replace("rocky mountain sculpin westslope populations",
                       "rocky mountain sculpin pacific populations") %>%
           str_replace("red knot rufa subspecies",
                       "red knot rufa subspecies tierra del fuego/patagonia wintering population")) %>%
  left_join(leg_list_data,
            by = c(common_name = "cosewic_common_name_population_sardms")) %>%
  #combine the columns from each
  mutate(large_taxonomic_group = case_when(
    taxonmic_group_sardms %in% c("Reptiles", "Amphibians") ~ "Herpetofauna",
    taxonmic_group_sardms %in% c("Lichens", "Mosses", "Vascular Plants") ~ "Plants",
    taxonmic_group_sardms %in% c("Arthropods", "Molluscs") ~ "Invertebrates",
    taxonmic_group_sardms %in% c("Fishes (marine)") ~ "Marine Fishes",
    taxonmic_group_sardms %in% c("Fishes (freshwater)") ~ "Freshwater Fishes",
    taxonmic_group_sardms %in% c("Mammals (terrestrial)") ~ "Terrestrial Mammals",
    taxonmic_group_sardms %in% c("Mammals (marine)") ~ "Marine Mammals",
    TRUE ~ taxonmic_group_sardms
  )) %>%
  mutate(across(c(contains("sardms"), -taxonmic_group_sardms),
                ~ifelse(lubridate::year(cosewic_examined_date_sardms) ==
                          year_published &
                          doc_type == "COSEWIC Status Reports",
                        .x,
                        NA))) %>%
  select(-c(legal_name_population_sardms, scientific_name_sardms,
            #taxonmic_group_sardms,
            sara_status_sardms, cos_id,
            leg_id)) %>%
  rename_with(~str_remove(.x, "_sardms"))

# reorder columns

# shouldn't be NA for any doc
col_meta <- c('uID', 'common_name', 'species', 'docID', 'doc_type', 'sara_status',
              'doc_citation', 'url', 'web_pub_date', 'year_published',
              'date_last_access', 'status_appraisal_rapid_review', 'final',
              'amendment', 'taxonmic_group')

# should be NA unless sr
col_sr <- c('author', 'EOO', 'IAO', 'locations', 'endemic_NA',
            'endemic_canada', 'continuous_USA', 'cosewic_status', 'ranges',
            'cosewic_examined_date', 'date_of_listing',
            'gic_decision')

# should be NA unless RS or MP CHab is NA for MP also
col_rs <- c('Critical_habitat', 'action_subtype', 'notes_action_subtype',
            'CC_action', 'CC_action_subtype', 'notes_CC_action_subtype',
            'action_type', 'CC_action_type')

# in all docs
col_CC <- c('CC_not_mentioned', 'CC_unknown', 'CC_in_knowledge_gap',
            'CC_unknown_impact', 'CC_unknown_scope', 'CC_unknown_severity',
            'CC_unknown_timing', 'CC_threat', 'CC_relative_impact')

# in all docs
col_tc <- c('threat_calculator', 'TC_version', 'TC_date', 'TC_assessors',
            'TC_references', 'TC_calculated_overall_impact',
            'TC_assigned_overall_impact', 'TC_impact_adjustment_reasons',
            'TC_overall_comments')

# in all docs
th_nums <- db_final_4 %>% select(contains("identified")) %>% colnames() %>%
  str_extract("\\d\\d?\\.?\\d?") %>%
  as.numeric() %>% sort()

col_th <- db_final_4 %>% select(starts_with("X")) %>% colnames() %>%
  str_remove("X\\d\\d?\\.?\\d?") %>% unique()

col_th <- map(th_nums, ~paste0("X", .x, col_th)) %>% unlist() %>%
  str_subset("X\\d\\d?\\..* notes", negate = TRUE)

# reorder cols
db_final_6 <- db_final_5 %>%
  select(all_of(col_meta), all_of(col_sr), all_of(col_rs), all_of(col_CC),
         all_of(col_tc), all_of(col_th)) %>%
  # fix values that don't match expected
  mutate(CC_action_subtype = str_replace(CC_action_subtype,
                                         regex("manage human threats",
                                               ignore_case = TRUE),
                                         "regulate human activities") %>%
           str_replace(regex("manage predation", ignore_case = TRUE),
                       "Manage native species negatively impacting species at risk"),
         action_subtype = str_replace(action_subtype,
                                      regex("manage human threats",
                                            ignore_case = TRUE),
                                      "regulate human activities") %>%
           str_replace(",,|, ,", ",") %>%
           str_replace(regex("manage predation", ignore_case = TRUE),
                       "Manage native species negatively impacting species at risk"),
         CC_unknown_scope = str_replace(CC_unknown_scope, "Nr", "NR"),
         # assign tc_version based on which lev 2 threats are used
         TC_version = case_when(threat_calculator == 1 &
                                  is.na(TC_version) &
                                  !is.na(X8.4_threat_identified) ~ 2.3,
                                threat_calculator == 1 &
                                  is.na(TC_version) &
                                  is.na(X8.4_threat_identified) ~ 1.1,
                                TRUE ~ TC_version)
         ) %>%
  rename_with(~str_replace(.x, " ", "_"))

# Do something about when the writers did not fill in level 1 but only level 2.
# I will update threat identified but not the impact, severity, scope, timing
# because they are supposed to be rolled up to level 2 based on whether the
# threats overlap

# look at rows with no level 1
# db_final_6 %>% filter(X4_threat_identified == 0,
# X4.1_threat_identified == 1|X4.2_threat_identified == 1|X4.3_threat_identified == 1)

# easier to do with base R
db_final_7 <- db_final_6

for (i in 1:11) {
  lev1 <- paste0("X", i, "_threat_identified")
  lev2 <- paste0("X", th_nums[which(floor(th_nums) == i)],
                 "_threat_identified") %>%
  setdiff(lev1)

  anylev2 <- rowSums(db_final_7[lev2], na.rm = TRUE) > 0

  db_final_7[[lev1]] <- case_when(
    db_final_7[[lev1]] == 0 & anylev2 ~ 1,
    TRUE ~ db_final_7[[lev1]])
}


write.csv(db_final_7, paste0("data-raw/interim/CAN_SARD_", Sys.Date(), ".csv"),
          row.names = FALSE)

# missing data #============================

# There are 139 docs with data missing in important fields sara_status,
# CC_not_mentioned, CC_threat, and CC_unknown.

# 290 with missing doc_citation and web_pub_date and 880 with missing url. Do we
# want to keep url

# 51 with missing cosewic_status, ranges and cosewic_examinined_date because
# they are older than the cosewic_examined_date so the data does not necessarily
# still apply

# 172 RS  and 56 MP are missing action type and subtype probably because CC was
# not a threat in the status report so it was not extracted in the 2018 db

# 139 docs with no level 1 threats. These are RS and MP from 2018 database

# 266 docs with no level 2 threats. These include the above and old SR where l2
# threats were not extracted because there was a new SR in the 2021 data



# Additional formatting after review #======================

db_final_7 <- read.csv(paste0("data-raw/interim/CAN_SARD_", Sys.Date(), ".csv"),
                       stringsAsFactors = FALSE)

# change url to the species page rather than docs
base_url <- "https://species-registry.canada.ca/index-en.html#/species?sortBy=commonNameSort&sortDirection=asc&pageSize=10&keywords="

db_final_8 <- db_final_7 %>% rename(rowID = docID, speciesID = uID,
                             taxonomic_group = taxonmic_group) %>%
  select(rowID, speciesID, everything()) %>%
  mutate(url = common_name %>% str_replace_all("-", " - ") %>%
                       stringi::stri_trans_general("latin-ascii") %>%
                       str_replace_all("/", "%20%2F%20") %>%
                       str_replace_all("'", "%27") %>%
                       str_replace_all("\\s", "%20") %>%
                       {paste0(base_url, .)})

write.csv(db_final_8, paste0("data-raw/interim/CAN_SARD_", Sys.Date(), ".csv"),
          row.names = FALSE)
