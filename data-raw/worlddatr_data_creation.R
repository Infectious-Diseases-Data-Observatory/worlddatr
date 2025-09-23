#-------------------------------------------------------------------------------
### Load packages
library(readxl)
library(tidyverse)
library(janitor)
library(devtools)

#-------------------------------------------------------------------------------
### Import world bank data
### https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
income_class <- read_excel("data-raw/income_class.xlsx") %>%
  filter(!is.na(Region),
         !is.na(Economy)) %>%
  select(-`Lending category`, -Region) %>%
  clean_names() %>%
  bind_rows(
    data.frame(
      economy = c("Jersey", "Guernsey"), # Classified as 'Channel Islands' in original data
      code = c("JEY", "GGY"),
      income_group = "High income"))

#-------------------------------------------------------------------------------
### Import list of countries and respective codes
country_codes <- read_csv("data-raw/country code.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  bind_rows(
    data.frame(
      country = "Kosovo",               # Classified by World Bank, not by ISO
      alpha_2_code = "UNASSIGNED",      # 'NA' used by Namibia
      alpha_3_code = "XKX",
      numeric = 999
    )
  )

# Rename countries with special / non-ASCII characters
country_codes[which(country_codes$alpha_3_code == "CUW"), "country"] = "Curacao"
country_codes[which(country_codes$alpha_3_code == "REU"), "country"] = "Reunion"
country_codes[which(country_codes$alpha_3_code == "BLM"), "country"] = "Saint Barthelemy"
country_codes[which(country_codes$alpha_3_code == "CIV"), "country"] = "Cote d'Ivoire"
country_codes[which(country_codes$alpha_3_code == "TUR"), "country"] = "Turkiye"
country_codes[which(country_codes$alpha_3_code == "ALA"), "country"] = "Aland Islands"
country_codes[which(country_codes$alpha_3_code == "ESH"), "country"] = "Western Sahara"

#-------------------------------------------------------------------------------
# Import REDCap country codes (countries numbered in English alphabetical order)
redcap_codes <- read_csv("data-raw/REDCap_codes.csv", show_col_types = FALSE)

#-------------------------------------------------------------------------------
# Import centroids for countries
centroids <- read_csv("data-raw/centroids.csv", show_col_types = FALSE)

centroids[which(centroids$alpha_3_code == "CUW"), "country"] = "Curacao"
centroids[which(centroids$alpha_3_code == "REU"), "country"] = "Reunion"
centroids[which(centroids$alpha_3_code == "BLM"), "country"] = "Saint Barthelemy"
centroids[which(centroids$alpha_3_code == "CIV"), "country"] = "Cote d'Ivoire"
centroids[which(centroids$alpha_3_code == "TUR"), "country"] = "Turkiye"
centroids[which(centroids$alpha_3_code == "ALA"), "country"] = "Aland Islands"

#-------------------------------------------------------------------------------
### Import coordinate data for countries and regions
country_name_lookup <- read_excel("inst/extdata/country_name_lookup.xlsx")

map_df = left_join(map_data("world"), country_name_lookup, by = c("region" = "country_name"))

# Standardise country and regions to match country_codes and income_class
map_df[which(map_df$region == "Ascension Island"), "alpha_3_code"] = "SHN"
map_df[which(map_df$region == "Virgin Islands" & map_df$subregion == " British"), "alpha_3_code"] = "VGB"
map_df[which(map_df$region == "Virgin Islands" & map_df$subregion == " US"), "alpha_3_code"] = "VIR"
map_df[which(map_df$region == "China" & map_df$subregion == "Hong Kong"), "alpha_3_code"] = "HKG"
map_df[which(map_df$region == "China" & map_df$subregion == "Macao"), "alpha_3_code"] = "MAC"
map_df[which(map_df$region == "Norway" & map_df$subregion == "Svalbard"), "alpha_3_code"] = "SJM"
map_df[which(map_df$region == "Norway" & map_df$subregion == "Jan Mayen"), "alpha_3_code"] = "SJM"
map_df[which(map_df$region == "Finland" & map_df$subregion == "Aland Islands"), "alpha_3_code"] = "ALA"
# Note: Siachen Glacier does not have an ISO code

#-------------------------------------------------------------------------------
### Join country_codes, REDCap codes and world bank data, one rown per country/territory
world_income <- left_join(country_codes,
                          income_class,
                          by = c("alpha_3_code" = "code"))  %>%
  select(alpha_3_code, alpha_2_code, numeric, country, economy, income_group) %>%
  left_join(redcap_codes, by = "alpha_3_code") %>%
  mutate(income_group = factor(income_group, levels = c("High income", "Upper middle income",
                                                        "Lower middle income", "Low income")))

### Join world income data to map of region coordinates
world_map <- map_df %>%
  left_join(world_income, by = c("alpha_3_code", "alpha_2_code")) %>%
  select(alpha_3_code, alpha_2_code, numeric, long, lat, group, order, region, subregion, country, economy, income_group, redcap_number) %>%
  left_join(centroids %>%
              select(-country), by = c("alpha_3_code", "alpha_2_code"))

### Create datasets for worlddatr package
write.csv(world_income,"data/world_income.csv", row.names = FALSE)
write.csv(world_map, "data/world_map.csv", row.names = FALSE)

use_data(world_income, overwrite = TRUE)
use_data(world_map, overwrite = TRUE)
