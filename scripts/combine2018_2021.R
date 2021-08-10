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

# unzip the file into data/data-raw Only do once
# unzip("~/../Downloads/drive-download-20210806T150949Z-001.zip",
#       exdir = "data/data-raw")

# Get the current version of the db from SAR_Climate_Change folder
db_2018 <- read.csv("../SAR_climate_change/SE_version/Threats_and_recovery/Analyses/data/data_wkg/Can_SAR_Database.csv")

# Get the data extracted from docs published after 2018 or species listed after
# 2018
# Have to specify some col types that are being read wrong
nms <- read_xlsx("data/data-raw/Can_SAR_data_extraction v.current.xlsx",
                 sheet = "Extracted Data", skip = 2,
                 na = "NA",
                 n_max = 0) %>% names()
ct <- case_when(str_detect(nms, "Date|DATE|date") ~ "date",
                str_detect(nms, "year|ID") ~ "numeric",
                TRUE ~ "guess")
# TC_date needs to be dealt with separately
ct[which(nms == "TC_date")] <- "guess"

db_2021 <- read_excel("data/data-raw/Can_SAR_data_extraction v.current.xlsx",
                      sheet = "Extracted Data", skip = 2,
                      na = c("NA", "N", "-"),
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

# Threats data for the 2021 db
db_2021_thFJ <- read_excel("data/data-raw/Can_SAR_data_extraction v.current.xlsx",
                         sheet = "Threats Calculator Data",
                         range = "A1:I7828",
                         #col_types = c(rep("guess", 9), "date", rep("guess", 7)),
                         na = "NA") %>%
  mutate(threat_num = str_replace(threat_num, ",", ".") %>% as.numeric(),
         source = "Hand Extraction")

# get TC_date and docID from the main spreadsheet
db_2021_thFJ2 <- left_join(db_2021_thFJ,
                           db_2021 %>% filter(threat_calculator == 1) %>%
                             select(uID, TC_date, docID) %>%
                             distinct(),
                           by = "uID")

# Threats data automatically extracted from threats calculator spreadsheets
# provided by CWS with uIDs added by hand by FJ
db_2021_thCWS <- read_excel("data/data-raw/Can_SAR_data_extraction v.current.xlsx",
                            sheet = "TC Spreadsheet Data",
                            range = "A1:Q8358",
                            na = "NA") %>%
  mutate(source = "Spreadsheet Extraction") %>%
  filter(!is.na(uID))

# Master list of uID, common name and species
uID_list <- read_excel("data/data-raw/list uID.xlsx")

# check that each uID threat_num is unique
db_2021_thCWS %>%
  group_by(uID, threat_num) %>%
  summarise(N = n()) %>%
  filter(N > 1) %>% nrow() == 0

# Different Docs can use the same TC
db_2021_thFJ2 %>%
  group_by(uID, threat_num, TC_date) %>%
  summarise(N = n()) %>%
  filter(N > 1) %>% nrow() == 0

# Fixed duplicates in source in uID 1001 in FJ there were 2 1.1s and one should
# be 1.0. For uID 117 it was incorrectly assigned. Should have been 116.
# Ignore NAs in uID they are correct in the FJ version


# combine the two data sets by taking adding in rows of CWS data that are not in
# FJ
db_2021_thCWS_notFJ <- anti_join(db_2021_thCWS, db_2021_thFJ2,
                                 by = c("uID", "threat_num", "TC_date")) %>%
  select(intersect(colnames(db_2021_thFJ2),
                   colnames(db_2021_thCWS)))

db_2021_th <- bind_rows(db_2021_thFJ2,
                          db_2021_thCWS_notFJ)

# 2021 Threats Data -------------------------------------------------------

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
# write.csv(dupDifs, "data/data-raw/reconcileDuplicates.csv", row.names = FALSE)
#
# # those with keep_both == 0 are the same with mostly changes from NA to not
# # applicable. Those where there was a real difference I checked manually and
# # recorded the docID
# dupDifs <- read.csv("data/data-raw/reconcileDuplicates.csv")
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
db_2021_2 %>% group_by(uID, TC_date, docID) %>% summarise(N = n()) %>%
  filter(N > 1) %>% {nrow(.) == 0} %>% stopifnot()

db_2021_3 <- coalesce_join(db_2021_2, db_2021_th_3, by = c("uID", "docID"))

db_2021_4 <- coalesce_join(db_2021_3,
                           uID_list %>% select(uID, species, common_name),
                           by = "uID",
                           join = left_join)

# TODO: add action_type and CC_action_type based on subtypes

write.csv(db_2021_4, paste0("data/interim/DB2021_", Sys.Date(), ".csv"),
          row.names = TRUE)


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
  #remove columns that are no longer in use
  select(-all_of(c("COSEWIC_report", "genus", "taxonomic_group", "year_assessment",
            "cosewic_designation", "designation_change", "year_order_council",
            "status_appraisal", "former_cosewic_designation",
            "year_former_cosewic_assessment", "cosewic_designation_change",
            "former_sara_status", "sara_status_change", "canadian_range",
            "AB", "BC", "MB", "NB", "NL", "NS", "NU", "NWT", "ON", "PEI", "QC",
            "SK", "YT", "arctic", "atlantic", "pacific"))) %>%
  #reorder cols to match 2021
  select(intersect(colnames(db_2021_4), colnames(.)))

db_2018_rs <- select(db_2018, uID, common_name, species, large_taxonomic_group,
                     all_of(rs_cols)) %>%
  mutate(year_published = ifelse(is.na(RS_amended), RS_year, RS_amended),
         final = RS_final,
         amendment = ifelse(is.na(RS_amended), 0, 1),
         CC_action_subtype = action_subtype_CC,
         docID = seq(1000, length.out = n()),
         doc_type = "Recovery Strategies",
         doc_citation = NA_character_,
         url = NA_character_,
         web_pub_date = as.Date(NA),
         date_last_access = as.Date("2018-02-22"),
         status_appraisal_rapid_review = 0) %>%
  select(-c(RS, RS_final, RS_amended, RS_year, action_subtype_CC))%>%
  filter(!is.na(year_published))


db_2018_mp <- select(db_2018, uID, common_name, species, large_taxonomic_group,
                     all_of(mp_cols)) %>%
  mutate(year_published = MP_year,
         final = MP_final,
         amendment = 0,
         CC_action_subtype = action_subtype_CC,
         docID = seq(1000, length.out = n()),
         doc_type = "Management Plans",
         doc_citation = NA_character_,
         url = NA_character_,
         web_pub_date = as.Date(NA),
         date_last_access = as.Date("2018-02-22"),
         status_appraisal_rapid_review = 0) %>%
  select(-c(MP, MP_final, MP_year, action_subtype_CC)) %>%
  # remove sp that don't have an MP
  filter(!is.na(year_published))

db_2018_refor <- bind_rows(db_2018_sr, db_2018_rs, db_2018_mp) %>%
  arrange(uID)

rm(db_2018, db_2018_mp, db_2018_rs, db_2018_sr)

# Add level 2 threats for 2018 database -----------------------------------
# level 2 threats identified in docs when no TC
db_2018_l2 <- read_excel("data/data-raw/Level 2 threats for 2018 database.xlsx",
                         sheet = "Threats data (2018)",
                         col_types = c(rep("guess", 8), "text", rep("guess", 73)),
                         skip = 1) %>%
  filter(`new data` == 0, threat_calculator == 0, !is.na(doc_citation)) %>%
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
  rename(doc_type = Doc_type)

# setdiff(colnames(db_2018_l2), colnames(db_2018_refor))

#uniquely ided by uID and doc_type?
db_2018_l2 %>% group_by(uID, Doc_type) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()

db_2018_refor %>% group_by(uID, doc_type) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()

# level 2 threats in recovery docs when tc used
db_2018_th <- read_excel("data/data-raw/Level 2 threats for 2018 database.xlsx",
                         sheet = "TC data (RS-MP 2018)",
                         na = "NA") %>%
  mutate(source = "Hand Extraction")

db_2018_th %>% group_by(uID, threat_num) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()

# updated in source TC data uID 515 had 1.1 twice instead of 1.1 and 11.1. Also
# had to change format of threat_num col so it wasn't a date

# format threats for database
db_2018_th_2 <- format_threats(db_2018_th)

db_2018_th_3 <- db_2018_th_2 %>%
  mutate(Doc_type = ifelse(Doc_type == "RS", "Recovery Strategies",
                           "Management Plans")) %>%
  rename(doc_type = Doc_type)

db_2018_th_3 %>% group_by(uID, doc_type) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()
# changed in source uID 398 Year was 2016 instead of 2017 for 1 threat_num

db_2018_refor %>% group_by(uID, doc_type) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()

db_2018_l2 %>% group_by(uID, doc_type) %>% filter(n() > 1) %>%
  {nrow(.) == 0} %>% stopifnot()
# combine all three into one. for Rs mp threats can use patch since should be NA
# in db_2018. For l2 threats FJ made some changes to level 1 threats as well so
# should update. Should not be any uIDs that are not in db_2018

# need to add new columns to db_2018_refor
cols_to_add <- c(setdiff(colnames(db_2018_th_3), colnames(db_2018_refor))[-1],
                 setdiff(colnames(db_2018_l2), colnames(db_2018_refor))[-1:-2])

alt_df <- as.data.frame(matrix(ncol = length(cols_to_add)))
colnames(alt_df) <- cols_to_add
db_2018_refor_2 <- bind_cols(db_2018_refor, alt_df)

db_2018_refor_3 <- db_2018_refor_2 %>%
  rows_patch(db_2018_th_3 %>% select(-Year), by = c("uID", "doc_type"))

db_2018_refor_4 <- db_2018_refor_3 %>%
  rows_upsert(db_2018_l2 %>% select(-`new data`, -`author SR`),
              by = c("uID", "doc_type"))

# which uID doc_type combos are in l2 but not db_2018
anti_join(db_2018_l2, db_2018_refor_3,
          by = c("uID", "doc_type")) %>%
  View()
# looks like some are situations where there is an RS for SC species or MP for a
# Th or En species which were initially ignored. Some seem to have been missed
# will add now but CC_action etc will be missing. This is the case for many 2018
# sp though
