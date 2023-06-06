library(dplyr)

setwd('~/Desktop/Thesis')

# rm(list = ls())
# rm(list=setdiff(ls(), "epa_data"))
rm(list = ls())

# Demographics
demo_data_1 = read.csv("Data/raw/R13239467_SL871.csv")
demo_data_1 = rename(demo_data_1, "population_density" = "SE_T002_002")
demo_data_1 = rename(demo_data_1, "zip" = "Geo_ZCTA5")

demo_data_1 = rename(demo_data_1, "total_population_2" = "SE_T001_001")

demo_data_1 = rename(demo_data_1, "white_pop" = "SE_T054_002")
demo_data_1 = rename(demo_data_1, "black_pop" = "SE_T054_003")
demo_data_1 = rename(demo_data_1, "native_pop" = "SE_T054_004")
demo_data_1 = rename(demo_data_1, "asian_pop" = "SE_T054_005")
demo_data_1 = rename(demo_data_1, "hawaiian_pop" = "SE_T054_006")
demo_data_1 = rename(demo_data_1, "other_pop" = "SE_T054_007")
demo_data_1 = rename(demo_data_1, "two_plus_pop" = "SE_T054_008")

demo_data_1["share_white"] = demo_data_1$white_pop / demo_data_1$total_population_2
demo_data_1["share_black"] = demo_data_1$black_pop / demo_data_1$total_population_2
demo_data_1["share_native"] = demo_data_1$native_pop / demo_data_1$total_population_2
demo_data_1["share_asian"] = demo_data_1$asian_pop / demo_data_1$total_population_2
demo_data_1["share_hawaiian"] = demo_data_1$hawaiian_pop / demo_data_1$total_population_2
demo_data_1["share_other"] = demo_data_1$other_pop / demo_data_1$total_population_2
demo_data_1["share_two_plus"] = demo_data_1$two_plus_pop / demo_data_1$total_population_2

demo_data_1["diversity"] = 1 - (demo_data_1["share_white"]^2 + demo_data_1["share_black"]^2 + demo_data_1["share_native"]^2 + demo_data_1["share_asian"]^2 + demo_data_1["share_hawaiian"]^2 + demo_data_1["share_other"]^2 + demo_data_1["share_two_plus"]^2)



demo_data_2 = read.csv("Data/raw/demographics_1_zip.csv")
demo_data_2 = rename(demo_data_2, "median_household_income" = "income_and_benefits_in_2016_inflation_adjusted_dollars.dollars.median_household_income_dollars")
demo_data_2$median_household_income = demo_data_2$median_household_income / 1000

demo_data_2 = rename(demo_data_2, "unemployment_rate" = "employment_status.percent.unemployed.of.population_16_years_and_over")

demo_data_2 = rename(demo_data_2, "zip" = "ZCTA5")



demo_data_3 = read.csv("Data/raw/demographics_3_zip.csv")
demo_data_3 = rename(demo_data_3, "college_rates" = educational_attainment.percent.bachelors_degree_or_higher.of.population_25_years_and_over)
demo_data_3 = rename(demo_data_3, "zip" = "ZCTA5")



# Base social capital data
ec_data = read.csv("Data/raw/social_capital_zip.csv")

# Load in county - CBSA data
urban_data <- read.csv("Data/raw/urban.csv")
urban_data = rename(urban_data, "county" = "FIPS.code")


# EPA Data
epa_data = read.csv("Data/raw/walkability_index.csv")


# Zip to tract crosswalk
zip_to_tract <- read.csv("Data/raw/zip_to_tract.csv")


# Match each EPA data tract to zip code
get_tract <- function(state, county, tract) {
  if (nchar(toString(state)) == 1) {
    state = paste0("0", toString(state))
  }
  if (nchar(toString(county)) == 1) {
    county = paste0("00", toString(county))
  }
  else if (nchar(toString(county)) == 2) {
    county = paste0("0", toString(county))
  }
  if (nchar(toString(tract)) == 1) {
    tract = paste0("00000", toString(tract))
  }
  else if (nchar(toString(tract)) == 2) {
    tract = paste0("0000", toString(tract))
  }
  else if (nchar(toString(tract)) == 3) {
    tract = paste0("000", toString(tract))
  }
  else if (nchar(toString(tract)) == 4) {
    tract = paste0("00", toString(tract))
  }
  else if (nchar(toString(tract)) == 5) {
    tract = paste0("0", toString(tract))
  }
  return(as.numeric(paste0(state, county, tract)))
}

epa_data$tract <- mapply(get_tract, epa_data$STATEFP, epa_data$COUNTYFP, epa_data$TRACTCE)

epa_data_zip <- epa_data %>%
  left_join(zip_to_tract, by = c("tract"))

cols = epa_data_zip %>% select(
  zip,
  tot_ratio,
  TotPop,
  Ac_Total,
  D3B,
  D1C8_RET,
  D1C8_OFF,
  D1C8_IND,
  D1C8_SVC,
  D1C8_ENT,
  D1C8_ED,
  D1C8_HLTH, 
  D1C8_PUB,
  D2B_E8MIXA,
  NatWalkInd
)

means <- cols %>%
  group_by(zip) %>%
  ## Ac_Total is total acres
  summarize_at(vars(D3B:D2B_E8MIXA),funs(weighted.mean(., w=tot_ratio)))


#Intersection density data
int_dens_data = read.csv("Data/raw/usa-tracts-street_network-stats.csv")
int_dens_data = rename(int_dens_data, "tract" = "geoid")
int_dens_data <- int_dens_data %>%
  left_join(zip_to_tract, by = c("tract"))
cols = int_dens_data %>% select(
  zip,
  tot_ratio,
  intersect_density_km
)

means2 <- cols %>%
  group_by(zip) %>%
  ## Ac_Total is total acres
  summarize_at(vars(intersect_density_km),funs(weighted.mean(., w=tot_ratio)))

# Merge all data

ec_data <- ec_data %>% 
  left_join(means2, by = c("zip"))

ec_data <- ec_data %>% 
  left_join(means, by = c("zip"))

ec_data <- ec_data %>%
  left_join(demo_data_1, by = c("zip"))

ec_data <- ec_data %>%
  left_join(demo_data_2, by = c("zip"))

ec_data <- ec_data %>%
  left_join(demo_data_3, by = c("zip"))

ec_data <- ec_data %>% 
  left_join(urban_data, by=c("county"))



# Write to working data folder
write.csv(ec_data, "Data/working/data_merged_zip.csv")



