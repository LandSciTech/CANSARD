# Combine SAR DMTS data with existing database

# This will fix species missing because they have no status report on the web

# Could additionally use this as a way to start adding new species


library(dplyr)
library(stringr)
library(tidyr)
# load the database
dat_pth <- "data-raw"

db <- readr::read_csv(here::here(dat_pth, "data-out/CAN-SAR_database.csv"))

# exported from SAR DMTS listing search page filtered by SARA Schedule = Schedule 1
dmts <- readr::read_csv(here::here(dat_pth, "data-raw/SAR_DMTS_FullExport-20240926-154900.csv"))

# link to species ids in db first

db_link <- db %>%
  select(speciesID, common_name, species, sara_status, cosewic_status,
         web_pub_date, year_published, doc_type, date_of_listing) %>%
  mutate(common_name = common_name %>% str_replace("populations", "population") %>%
           str_replace_all("�", "é") %>%
           str_replace_all("tope shark", "tope"))


dmts_link <- dmts %>%
  select(`COSEWIC ID`, `Listing ID`, `COSEWIC English common name`,
         `COSEWIC Population (E)`, `COSEWIC Scientific Name`, `COSEWIC WS status`,
         `Listing Common Name (English)`, `Listing Scientific Name`,
         `Listing Taxonomic group (E)`, `Listing Population (E)`,
         `On Schedule 1 date`, Endemic, `Ranges (E)`, `Type of Report`) %>%
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
  full_join(db_link, by = "common_name")

# some confusion due to species on Schedule 1 under one DU but new DU exists and
# has COSEWIC status but hasn't been added to schedule 1 yet. Looks like in
# CANSAR the old SARA status was applied to all three of the new DUs although
# probably that is incorrect since only two were included in the previous DU and
# the third was previously not assessed by COSEWIC and isn't really on SARA
# registry.
# CANSAR includes all species that were ever on schedule 1 it seems eg Sonora skipper


# TODO stopped while looking at species in DMTS but not CANSAR, should filter
# DMTS so only shows those listed before CANSAR end date
