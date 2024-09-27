# Combine SAR DMTS data with existing database

# This will fix species missing because they have no status report on the web

# Could additionally use this as a way to start adding new species


library(dplyr)
library(stringr)
library(tidyr)
# load the database
dat_pth <- "data-raw"

db <- readr::read_csv(here::here(dat_pth, "data-out/CAN-SAR_database.csv"),
                      locale = readr::locale(encoding = "ISO-8859-1"))
readr::write_csv(db, here::here(dat_pth, "data-out/CAN-SAR_database.csv"))
# exported from SAR DMTS listing search page filtered by SARA Schedule = Schedule 1
dmts <- readr::read_csv(here::here(dat_pth, "data-raw/SAR_DMTS_FullExport-20240926-154900.csv"))

# link to species ids in db first

db_link <- db %>%
  select(speciesID, common_name, species, sara_status, cosewic_status,
         web_pub_date, year_published, doc_type, date_of_listing, taxonomic_group) %>%
  mutate(common_name = common_name %>% str_replace("populations", "population") %>%
           str_replace_all("�", "é") %>%
           str_replace_all("tope shark", "tope"))


dmts_link <- dmts %>%
  select(`COSEWIC ID`, `Listing ID`, `COSEWIC English common name`,
         `COSEWIC Population (E)`, `COSEWIC Scientific Name`, `COSEWIC WS status`,
         `COSEWIC WS status date`,
         `Listing Common Name (English)`, `Listing Scientific Name`,
         `Listing Taxonomic group (E)`, `Listing Population (E)`, `Schedule 1 status`,
         `On Schedule 1 date`, `GIC decision date`, `GIC decision (E)`,
         Endemic, `Ranges (E)`, `Type of Report`) %>%
  mutate(common_name = paste(`COSEWIC English common name`,
                             ifelse(is.na(`COSEWIC Population (E)`), "",
                                    `COSEWIC Population (E)`)) %>%
           str_to_lower() %>% str_trim() %>%
           str_replace("populations", "population") %>%
           str_replace_all("common five", "five") %>%
           str_replace_all("eastern false", "false") %>%
           str_remove_all("\\(|\\)") %>%
           str_replace_all("’", "'") %>%
           str_replace_all(" \\-| \\- |\\- ", "-") %>%
           str_replace_all(" \\-| \\- |\\- ", "-") %>%
           str_replace_all(" – ", "-") %>%
           str_replace_all(" \\/| \\/ |\\/ ", "/")%>%
           str_replace_all(" \\/| \\/ |\\/ ", "/")) %>%
  filter(`On Schedule 1 date` < as.Date("2021-03-01")) %>%
  full_join(db_link, by = "common_name")

# some confusion due to species on Schedule 1 under one DU but new DU exists and
# has COSEWIC status but hasn't been added to schedule 1 yet. Looks like in
# CANSAR the old SARA status was applied to all three of the new DUs although
# probably that is incorrect since only two were included in the previous DU and
# the third was previously not assessed by COSEWIC and isn't really on SARA
# registry.
# CANSAR includes all species that were ever on schedule 1 it seems eg Sonora skipper



## Missing species

# Badger jeffersoni subspecies was not included in initial data extraction

# Loggerhead shrike Eastern subspecies is missing was not included in initial data
# extraction

# Channel darter dmts says it was listed in 2006 but it was only on Schedule
# 2 until 2019 so it was not included in the initial database and it was not
# included in the update to the database because we filtered based on new
# documents added after 2018 or species listed after 2018 and the database
# incorrectly lists it as on schedule 1 since 2018

# White sturgeon very confusing DU listing history, but some should have been
# included as on schedule 1 in 2018 but were not in the initial data extraction

# Silver chub should have been in database because was on Schedule 1 in 2018, may
# have been missed because COSEWIC changed DU in 2012 and was updated on
# Schedule 1 in 2019

# Toothcup should have been in database because was on Schedule 1 in 2018, may
# have been missed because COSEWIC changed DU in 2014 and was updated on
# Schedule 1 in 2019

# Northern and Southern mountain caribou not included in initial data
# extraction. These are on Schedule 1 as woodland caribou NM and SM pops but
# COSEWIC changed the names and geography of subpopulations included in each in
# DU in 2014, so the COSEWIC status of the old DUs is now Non-active but the new
# DUs are not yet on Schedule 1

## Not really missing species but not matched

# Grey whale eastern north pacific population is on schedule one but the most
# recent status report is two child DUs so those are the ones included in the
# database even though they are not actually on schedule one yet. Technically the
# 2004 SR for this pop is missing from our database though if the goal was to
# include all documents, but I think we only included the most recent document.


# Species in database but missing Status Report
sr_to_add <- dmts_link %>% filter(!is.na(`COSEWIC ID`), !is.na(speciesID)) %>%
  group_by(common_name) %>%
  filter(!any(doc_type == "COSEWIC Status Reports")) %>%
  ungroup() %>%
  mutate(rowID = (max(db$rowID)+1):(max(db$rowID)+n()),
         speciesID, common_name,
         species = `COSEWIC Scientific Name`,
         doc_type = "COSEWIC Status Reports",
         sara_status = `Schedule 1 status`,
         doc_citation = "NE",
         url = "NE",
         web_pub_date = NA,
         year_published = lubridate::year(lubridate::ymd(`COSEWIC WS status date`)),
         date_last_access = NA,
         status_appraisal_rapid_review = NA,
         final = 1,
         amendment = NA,
         taxonomic_group,
         report_writers = "NE",
         EOO = "NE",
         IAO = "NE",
         locations = "NE",
         endemic_NA = "NE",
         endemic_canada = "NE",
         continuous_USA = "NE",
         cosewic_status = "NE",
         ranges = "NE",
         cosewic_examined_date = "NE",
         date_of_listing = `On Schedule 1 date`,
         gic_decision = "NE",
         Critical_habitat = NA,
         action_subtype = NA,
         notes_action_subtype = NA,
         CC_action = NA,
         CC_action_subtype = NA,
         notes_CC_action_subtype = NA,
         action_type = NA,
         CC_action_type = NA,
         .keep = "none"
         # the rest are CC and TC and should all be NE, will add separately
         ) %>%
  distinct(speciesID, .keep_all = TRUE)

nms_to_add <- setdiff(names(db), names(sr_to_add))

sr_to_add[, nms_to_add] <- "NE"


sp_to_add <- dmts_link %>%
  # changing to listing name because some are not on COSEWIC due to name changes
  mutate(common_name = paste(`Listing Common Name (English)`,
                             ifelse(is.na(`Listing Population (E)`), "",
                                    `Listing Population (E)`)) %>%
           str_to_lower() %>% str_trim() %>%
           str_replace("populations", "population") %>%
           str_replace_all("common five", "five") %>%
           str_replace_all("eastern false", "false") %>%
           str_remove_all("\\(|\\)") %>%
           str_replace_all("’", "'") %>%
           str_replace_all(" \\-| \\- |\\- ", "-") %>%
           str_replace_all(" \\-| \\- |\\- ", "-") %>%
           str_replace_all(" – ", "-") %>%
           str_replace_all(" \\/| \\/ |\\/ ", "/")%>%
           str_replace_all(" \\/| \\/ |\\/ ", "/")) %>%
  filter(is.na(speciesID), `Schedule 1 status` != "Extirpated",
         # Grey whale is included just with more recent DU names
         `Listing Common Name (English)` != "Grey Whale") %>%
  mutate(rowID = (max(sr_to_add$rowID)+1):(max(sr_to_add$rowID)+n()),
         speciesID = (max(db$speciesID)+1):(max(db$speciesID)+n()),
         common_name,
         species = `COSEWIC Scientific Name`,
         doc_type = "COSEWIC Status Reports",
         sara_status = `Schedule 1 status`,
         doc_citation = "NE",
         url = "NE",
         web_pub_date = NA,
         year_published = lubridate::year(lubridate::ymd(`COSEWIC WS status date`)),
         date_last_access = NA,
         status_appraisal_rapid_review = NA,
         final = 1,
         amendment = NA,
         taxonomic_group,
         report_writers = "NE",
         EOO = "NE",
         IAO = "NE",
         locations = "NE",
         endemic_NA = "NE",
         endemic_canada = "NE",
         continuous_USA = "NE",
         cosewic_status = "NE",
         ranges = "NE",
         cosewic_examined_date = "NE",
         date_of_listing = `On Schedule 1 date`,
         gic_decision = "NE",
         Critical_habitat = NA,
         action_subtype = NA,
         notes_action_subtype = NA,
         CC_action = NA,
         CC_action_subtype = NA,
         notes_CC_action_subtype = NA,
         action_type = NA,
         CC_action_type = NA,
         .keep = "none"
         # the rest are CC and TC and should all be NE, will add separately
  )

nms_to_add <- setdiff(names(db), names(sp_to_add))

sp_to_add[, nms_to_add] <- "NE"

# TODO: fix in database

# making changes one at a time and saving csv and committing so that history is clear

# Fissidens exillis does have a status report but no date listed since it was
# since unlisted. Should update by hand if possible. Also seems to have typo SARA shows one l
Fe_fix <- db %>% filter(species == "Fissidens exillis") %>% select(rowID, speciesID, species, doc_type, date_of_listing) %>%
  mutate(species = "Fissidens exilis",
         date_of_listing = c(as.Date("2006-08-15"), NA))

db <- db %>%
  rows_update(Fe_fix, by = c("rowID", "speciesID"))

readr::write_csv(db, here::here(dat_pth, "data-out/CAN-SAR_database.csv"))

# typo in Urocyon cinereoargenteus gray fox recovery strategy species? Actually
# it looks like it is in the original extraction which is missing the e? Looks
# like we can remove the old recovery strategy entry and change the speciesID so
# that they are the same in both years because the new entry is marked final and
# has more data extracted.
Uc_fix <- db %>% filter(str_detect(species, "Urocyon cinere?oargenteus")) %>%
  select(rowID, speciesID, species) %>%
  mutate(species = "Urocyon cinereoargenteus",
         speciesID = 60)

# deleting row without threats extracted where there is a newer row for that RS
Uc_del <- db %>% filter(str_detect(species, "Urocyon cinere?oargenteus"), action_type == "NE") %>%
  select(rowID, speciesID)

db <- db %>%
  rows_update(Uc_fix, by = c("rowID")) %>%
  rows_delete(Uc_del, by = c("rowID"))

readr::write_csv(db, here::here(dat_pth, "data-out/CAN-SAR_database.csv"))

