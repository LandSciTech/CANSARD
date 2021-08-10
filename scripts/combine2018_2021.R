# Combine the existing database with the updated versions for species after
# 2018 and with level 2 threats
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

# Load all functions in the R directory
devtools::load_all(".")

# Download the data from google drive
# https://drive.google.com/drive/folders/1p3SYAWkgI4GX8wsaw3UgXuf_twmuedHK?usp=sharing

# unzip the file into data-raw Only do once
# unzip("~/../Downloads/drive-download-20210806T150949Z-001.zip",
#       exdir = "data-raw")

# Get the current version of the db from SAR_Climate_Change folder
db_2018 <- read.csv("../SAR_climate_change/SE_version/Threats_and_recovery/Analyses/data/data_wkg/Can_SAR_Database.csv")

# Get the data extracted from docs published after 2018 or species listed after
# 2018
# Have to specify some col types that are being read wrong
nms <- read_xlsx("data-raw/Can_SAR_data_extraction v.current.xlsx",
                 sheet = "Extracted Data", skip = 2,
                 na = "NA",
                 n_max = 0) %>% names()
ct <- case_when(str_detect(nms, "Date|DATE|date") ~ "date",
                str_detect(nms, "year|ID") ~ "numeric",
                TRUE ~ "guess")
# TC_date needs to be dealt with separately
ct[which(nms == "TC_date")] <- "guess"

db_2021 <- read_excel("data-raw/Can_SAR_data_extraction v.current.xlsx",
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
db_2021_thFJ <- read_excel("data-raw/Can_SAR_data_extraction v.current.xlsx",
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
db_2021_thCWS <- read_excel("data-raw/Can_SAR_data_extraction v.current.xlsx",
                            sheet = "TC Spreadsheet Data",
                            range = "A1:Q8358",
                            na = "NA") %>%
  mutate(source = "Spreadsheet Extraction") %>%
  filter(!is.na(uID))

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
# Deal with date fields. Comes in as character with mixture of excel numeric
# dates and years
# db_2021_th <- mutate(
#   db_2021_th,
#   TC_date = case_when(nchar(TC_date) == 6 ~ str_replace(TC_date, "\\.0", ""),
#                       TRUE ~ TC_date) %>%
#     janitor::convert_to_date(
#       character_fun = function(x){
#         lubridate::parse_date_time(x, orders = c("Y", "Ydm"), truncated = 3) %>%
#           as.Date()
#       })
# )

# seems some dates are a sequence from draging in excel
# db_2021_th %>% group_by(uID) %>%
#   summarise(ndist_date = n_distinct(TC_date)) %>%
#   filter(ndist_date > 1)

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
# write.csv(dupDifs, "data-raw/reconcileDuplicates.csv", row.names = FALSE)
#
# # those with keep_both == 0 are the same with mostly changes from NA to not
# # applicable. Those where there was a real difference I checked manually and
# # recorded the docID
# dupDifs <- read.csv("data-raw/reconcileDuplicates.csv")
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
# used same TC
db_2021_2 %>% group_by(uID, TC_date, docID) %>% summarise(N = n()) %>% filter(N > 1) %>%
  View("NotUnique")


#Use rows_upsert to either insert a new row or replace values in x with y if
#match the key. Need to figure out which cols should be upserted other option is
#rows_patch which will only replace if x is NA but will not create new rows
# first need to join to uID docID and TC_date so that rows_upsert will have
# uniquely ided rows
db_2021_th_toupdate <- db_2021_th_3 %>%
  select(uID, common_name, contains("TC_"), matches("X\\d.*identified"),
         -c(X8.4_threat_identified, X8.5_threat_identified,
            X8.6_threat_identified,
            X11.5_threat_identified)) %>%
  left_join(db_2021_2 %>% select(uID, docID, TC_date), by = c("uID", "TC_date"))

# check columns match
setdiff(colnames(db_2021_th_toupdate), colnames(db_2021_2))

# update rows that already exist and create new rows if they don't
db_2021_3 <- rows_upsert(db_2021_2, db_2021_th_toupdate,
                         by = c("uID", "common_name", "TC_date"))





