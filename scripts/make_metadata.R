# Make EML metadata for CAN-SAR database

# see tutorial here https://docs.ropensci.org/EML/articles/creating-EML.html

library(EML)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(emld)

# Parameters to set #====================

# date that the database is current to
last_update <- "2021-03-23"

# Build data table #=====================
# Build attribute list using data dictionary
data_dict <- read_xlsx("data-raw/data-out/CAN-SAR_data_dictionary.xlsx",
                       sheet = 2, skip = 1)

attribs <- transmute(data_dict, attributeName = `Column header`,
                     attributeDefinition = paste0(Description, " Extraction details: ",
                                                  `Extraction Details`),
                     formatString = Format, definition = Description,
                     unit = ifelse(str_detect(Format, "units"), "kilometerSquared", "dimensionless"),
                     numberType = ifelse(Format %in% c("Integer", "Ordinal", "Binary"),
                                         "integer",
                                         ifelse(str_detect(Format, "units"), "real",
                                                NA_character_)),
                     measurementScale = case_when(Format == "Text" ~ "nominal",
                                                  Format == "Categorical" ~ "nominal",
                                                  str_detect(Format, "Integer") ~ "ratio",
                                                  str_detect(Format, "YYYY") ~ "dateTime",
                                                  Format == "Binary" ~ "nominal",
                                                  Format == "Ordinal" ~ "ordinal",
                                                  str_detect(Format, "not a") ~ "nominal",
                                                  str_detect(Format, "sheet") ~ "nominal"),
                     domain = case_when(Format == "Text" ~ "textDomain",
                                        str_detect(Format, "not a") ~ "textDomain",
                                        measurementScale == "ratio" ~ "numericDomain",
                                        measurementScale == "dateTime" ~ "dateTimeDomain",
                                        measurementScale %in% c("ordinal", "nominal") ~ "enumeratedDomain")
                     ) %>%
  mutate(formatString = ifelse(measurementScale == "dateTime", formatString, NA_character_))

facts <- data_dict %>% select(`Column header`, `Possible values`:ncol(.)) %>%
  rename(attributeName = `Column header`) %>%
  filter(!is.na(`Possible values`)) %>%
  pivot_longer(-attributeName) %>%
  select(-name) %>%
  filter(!is.na(value)) %>%
  separate(value, into = c("code", "definition"), sep = ": ", fill = "right") %>%
  mutate(definition = ifelse(is.na(definition), code, definition))

missVals <- facts %>% filter(code %in% c("NA", "NR"))

facts <- facts %>% group_by(attributeName) %>%
  filter(!all(code %in% c("NA", "NR")))

# facts is missing the action types
action_table <- read_xlsx("data-raw/data-out/CAN-SAR_data_dictionary.xlsx",
                          sheet = 3)
action_types <- action_table %>% group_by(`Action type`) %>%
  summarise(definition = paste0("See action sub-types: ",
                                paste0(`Action sub-types`, collapse = ", "))) %>%
  transmute(attributeName = "action_type, CC_action_type",
            code = `Action type`,
            definition = definition) %>%
  separate_rows(attributeName, sep = ", ")

action_subtypes <- action_table %>%
  transmute(attributeName = "action_subtype, CC_action_subtype",
            code = `Action sub-types`,
            definition = Description) %>%
  separate_rows(attributeName, sep = ", ")

facts <- bind_rows(facts, action_types, action_subtypes)

eml_attribs <- set_attributes(attribs, facts, missingValues = missVals)

eml_phys <- set_physical("data-raw/data-out/CAN-SAR_database.csv")

dataTable <- list(
  entityName = "CAN-SAR_database.csv",
  entityDescription = "CAN-SAR: A database of Canadian species at risk information",
  physical = eml_phys,
  attributeList = eml_attribs)

# Get metadata #=========================================
db <- read.csv("data-raw/data-out/CAN-SAR_database.csv")

cover <- set_coverage(begin = db$cosewic_examined_date %>% min(na.rm = TRUE),
                      end = last_update,
                      sci_names = unique(db$species) %>% as.list(),
                      geographicDescription = "Canada",
                      west = -140.99778, east = -52.6480987209,
                      north = 83.23324, south = 41.6751050889)

sarah <- person(given = "Sarah", family = "Endicott", role = "cre",
                email = "sarah.endicott@ec.gc.ca",
                comment = c(ORCID = "0000-0001-9644-5343")) %>%
  as_emld()

ilona <- person(given = "Ilona", family = "Naujokaitis-Lewis", role = "cre",
                email = "ilona.naujokaitis-lewis@ec.gc.ca",
                comment = c(ORCID = "0000-0001-9504-4484")) %>%
  as_emld()

jessica <-  person(given = "Jessica M.", family = "Guezen") %>%
  as_emld()

jessica$role <- "author"

nwrc_address <- list(
  deliveryPoint = "1125 Colonel By Drive",
  city = "ON",
  administrativeArea = "ON",
  postalCode = "K1S 5B6",
  country = "Canada")

publisher <- list(
  organizationName = "National Wildlife Research Centre, Environment and Climate Change Canada",
  address = nwrc_address)

contact <- list(
  individualName = ilona$individualName,
  electronicMailAddress = ilona$electronicMailAddress,
  address = nwrc_address,
  organizationName = "National Wildlife Research Centre, Environment and Climate Change Canada",
  phone = "613-302-0911")

keywordSet <- list(
  list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list("endangered species", "biodiversity")
  ),
  list(
    keywordThesaurus = "LTER core area",
    keyword =  list("populations")
  ))

pubDate <- "2022"

title <- "CAN-SAR: A database for Canadian species at risk information"

abstract <- "CAN-SAR: A database of Canadian Species at Risk information is an initiative led by Dr. Ilona Naujokaitis-Lewis from Environment Climate Change Canada. The aim of this database is to provide open and accessible data reflecting information obtained from Canadian species at risk listing and recovery planning documents. Ongoing efforts include development of a living database that will facilitate contributions from other parties in an effort to increase efficiencies and decrease multiple (redundant) efforts with the broad over-arching goal of improving the conservation of species at risk. **NOTE:** The current version of CAN-SAR includes documents available as of **March 23, 2021** for species with SARA statuses Special Concern, Threatened, or Endangered. For the authoritative source of current species at risk information please consult the SARA Public Registry (https://www.canada.ca/en/environment-climate-change/services/species-risk-public-registry.html)."

license <- list(licenseName = "Creative Commons Attribution 4.0 International Public License",
                url = "https://creativecommons.org/licenses/by/4.0/",
                identifier = "CC BY 4.0")

dataset <- list(
  title = title,
  creator = list(sarah, ilona),
  pubDate = pubDate,
  licensed = license,
  abstract = list(markdown = abstract),
  associatedParty = jessica,
  keywordSet = keywordSet,
  coverage = cover,
  contact = contact,
  dataTable = dataTable)

eml <- list(
  packageId = "doi:10.17605/OSF.IO/E4A58",
  system = "doi", # type of identifier
  dataset = dataset)

write_eml(eml, "data-raw/data-out/eml.xml")


# seems to come out in the wrong order
eml_validate("data-raw/data-out/eml.xml")
