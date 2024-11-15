library(readxl)
library(tidyverse)
library(janitor)

### Import world bank data
### https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
CLASS <- read_excel("data-raw/CLASS.xlsx") %>%
  select(-`Lending category`, -Region) %>%
  clean_names() %>%
  filter(!is.na(economy),
         !is.na(income_group)) %>%
  bind_rows(
    data.frame(
      economy = c("Jersey", "Guernsey"),
      code = c("JEY", "GGY"),
      income_group = "High income")
  )

### Import list of countries and respective codes
country_codes <- read_csv("data-raw/country_codes.csv", show_col_types = FALSE)

### Import coordinate data for countries and regions
map_df = map_data("world") %>%
  left_join(country_codes %>%
              select(country, alpha_3_code),
            by = c("region" = "country"))

### Standarise country and regions to match country_codes and CLASS
map_df[which(map_df$region == "French Southern and Antarctic Lands"), "alpha_3_code"] = "ATF"
map_df[which(map_df$region == "Antigua"), "alpha_3_code"] = "ATG"
map_df[which(map_df$region == "Barbuda"), "alpha_3_code"] = "ATG"
map_df[which(map_df$region == "Brunei"), "alpha_3_code"] = "BRN"
map_df[which(map_df$region == "Ivory Coast"), "alpha_3_code"] = "CIV"
map_df[which(map_df$region == "Democratic Republic of the Congo"), "alpha_3_code"] = "COD"
map_df[which(map_df$region == "Republic of Congo"), "alpha_3_code"] = "COG"
map_df[which(map_df$region == "Cape Verde"), "alpha_3_code"] = "CPV"
map_df[which(map_df$region == "Czech Republic"), "alpha_3_code"] = "CZE"
map_df[which(map_df$region == "Falkland Islands"), "alpha_3_code"] = "FLK"
map_df[which(map_df$region == "Micronesia"), "alpha_3_code"] = "FSM"
map_df[which(map_df$region == "UK"), "alpha_3_code"] = "GBR"
map_df[which(map_df$region == "Heard Island"), "alpha_3_code"] = "HMD"
map_df[which(map_df$region == "Cocos Islands"), "alpha_3_code"] = "CCK"
map_df[which(map_df$region == "Nevis"), "alpha_3_code"] = "KNA"
map_df[which(map_df$region == "Saint Kitts"), "alpha_3_code"] = "KNA"
map_df[which(map_df$region == "North Korea"), "alpha_3_code"] = "PRK"
map_df[which(map_df$region == "South Korea"), "alpha_3_code"] = "KOR"
map_df[which(map_df$region == "Laos"), "alpha_3_code"] = "LAO"
map_df[which(map_df$region == "Sint Maarten"), "alpha_3_code"] = "SXM"
map_df[which(map_df$region == "Saint Martin"), "alpha_3_code"] = "MAF"
map_df[which(map_df$region == "Moldova"), "alpha_3_code"] = "MDA"
map_df[which(map_df$region == "North Macedonia"), "alpha_3_code"] = "MKD"
map_df[which(map_df$region == "Bonaire"), "alpha_3_code"] = "BES"
map_df[which(map_df$region == "Sint Eustatius"), "alpha_3_code"] = "BES"
map_df[which(map_df$region == "Saba"), "alpha_3_code"] = "BES"
map_df[which(map_df$region == "Pitcairn Islands"), "alpha_3_code"] = "PCN"
map_df[which(map_df$region == "Palestine"), "alpha_3_code"] = "PSE"
map_df[which(map_df$region == "Russia"), "alpha_3_code"] = "RUS"
map_df[which(map_df$region == "South Sandwich Islands"), "alpha_3_code"] = "SGS"
map_df[which(map_df$region == "South Georgia"), "alpha_3_code"] = "SGS"
map_df[which(map_df$region == "Saint Helena"), "alpha_3_code"] = "SHN"
map_df[which(map_df$region == "Ascension Island"), "alpha_3_code"] = "SHN"
map_df[which(map_df$region == "Swaziland"), "alpha_3_code"] = "SWZ"
map_df[which(map_df$region == "Syria"), "alpha_3_code"] = "SYR"
map_df[which(map_df$region == "Trinidad"), "alpha_3_code"] = "TTO"
map_df[which(map_df$region == "Tobago"), "alpha_3_code"] = "TTO"
map_df[which(map_df$region == "Taiwan"), "alpha_3_code"] = "TWN"
map_df[which(map_df$region == "Tanzania"), "alpha_3_code"] = "TZA"
map_df[which(map_df$region == "USA"), "alpha_3_code"] = "USA"
map_df[which(map_df$region == "Vatican"), "alpha_3_code"] = "VAT"
map_df[which(map_df$region == "Grenadines"), "alpha_3_code"] = "VCT"
map_df[which(map_df$region == "Saint Vincent"), "alpha_3_code"] = "VCT"
map_df[which(map_df$region == "Venezuela"), "alpha_3_code"] = "VEN"
map_df[which(map_df$region == "Vietnam"), "alpha_3_code"] = "VNM"
map_df[which(map_df$region == "Chagos Archipelago"), "alpha_3_code"] = "IOT"

map_df[which(map_df$region == "Virgin Islands" & map_df$subregion == " British"), "alpha_3_code"] = "VGB"
map_df[which(map_df$region == "Virgin Islands" & map_df$subregion == " US"), "alpha_3_code"] = "VIR"
map_df[which(map_df$region == "China" & map_df$subregion == "Hong Kong"), "alpha_3_code"] = "HKG"
map_df[which(map_df$region == "China" & map_df$subregion == "Macao"), "alpha_3_code"] = "MAC"
map_df[which(map_df$region == "Norway" & map_df$subregion == "Svalbard"), "alpha_3_code"] = "SJM"
map_df[which(map_df$region == "Norway" & map_df$subregion == "Jan Mayen"), "alpha_3_code"] = "SJM"
map_df[which(map_df$region == "Finland" & map_df$subregion == "Aland Islands"), "alpha_3_code"] = "ALA"

### Join country_codes and world bank data, one rown per country/territory
world_income <- left_join(country_codes,
                          CLASS,
                          by = c("alpha_3_code" = "code"))  %>%
  select(alpha_3_code, alpha_2_code, numeric, country, economy, income_group)

### Join world income data to map of region coordinates
world_map <- map_df %>%
  left_join(world_income) %>%
  select(alpha_3_code, alpha_2_code, numeric, lat, long, group, order, region, subregion, country, economy, income_group)

### Create datasets for worlddatr package
write.csv(world_income,"data/world_income.csv", row.names = FALSE)
write.csv(world_map, "data/world_map.csv", row.names = FALSE)

use_data(world_income, overwrite = TRUE)
use_data(world_map, overwrite = TRUE)
