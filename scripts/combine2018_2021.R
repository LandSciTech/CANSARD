# Combine the existing database with the updated versions for species after
# 2018 and with level 2 threats
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

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

# Threats data for the 2021 db merged with threats data automatically extracted
# from threats calculator spreadsheets provided by CWS
db_2021_th <- read_excel("data-raw/Can_SAR_data_extraction v.current.xlsx",
                         sheet = "TC Data Merged",
                         #col_types = c(rep("guess", 9), "date", rep("guess", 7)),
                         na = "NA")

# 2021 Threats Data -------------------------------------------------------
# Deal with date fields. Comes in as character with mixture of excel numeric
# dates and years
db_2021_th <- mutate(
  db_2021_th,
  TC_date = case_when(nchar(TC_date) == 6 ~ str_replace(TC_date, "\\.0", ""),
                      TRUE ~ TC_date) %>%
    janitor::convert_to_date(
      character_fun = function(x){
        lubridate::parse_date_time(x, orders = c("Y", "Ydm"), truncated = 3) %>%
          as.Date()
      })
)


#check that each uID threat_num is unique
db_2021_th %>% mutate(threat_num = str_replace(threat_num, ",", ".")) %>%
  group_by(uID, threat_num) %>%
  summarise(N = n()) %>%
  filter(N > 1)

# looks like two species have duplicates 1001 and 1023
db_2021_th %>% mutate(rownum = 1:n()) %>% filter(uID == 1001) %>%
  arrange(threat_num)
# just 1.1 is repeated and all the values are the same

db_2021_th %>% mutate(rownum = 1:n()) %>% filter(uID == 1023) %>%
  #mutate(threat_num = str_replace(threat_num, ",", ".")) %>%
  #distinct(impact, scope, severity, timing, threat_num) %>%
  arrange(threat_num)

# there are no differences in impact, scope, severity, timing only difference
# appears to be in capitalization of one field

# All duplicated threats are the same so can just remove duplicates based on
# rownums
db_2021_th <- db_2021_th %>% slice(-630, -8032:-8082) %>%
  # Change threat num so no .0 to match db
  mutate(threat_num = str_replace(threat_num, "\\.0|,0", ""))

# format the threats to fit with the 2018 database
db_2021_th_2 <- format_threats(db_2021_th)

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

# Join threats data to 2021 db
db_2021_3 <- db_2021_2 %>% select(-contains("TC"), TC_date)

#Use rows_upsert to either insert a new row or replace values in x with y if
#match the key. Need to figure out which cols should be upserted other option is
#rows_patch which will only replace if x is NA but will not create new rows

db_2021_th_toupdate <- db_2021_th_2 %>%
  select(uID,contains("TC_"), matches("X\\d.*"))

db_2021_3 <- rows_upsert(db_2021_2, db_2021_th_toupdate)

setdiff(colnames(db_2021_th_toupdate), colnames(db_2021_2))



