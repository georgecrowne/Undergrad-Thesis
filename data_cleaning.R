library(dplyr)

#############################
# County Level data merging
#############################

setwd('~/Desktop/Thesis')

# rm(list=setdiff(ls(), "epa_data"))
rm(list = ls())

# FIPS to county crosswalk
fips = read.csv("Data/raw/fips_county_crosswalk.csv")

# Base social capital data
ec_data = read.csv("Data/raw/social_capital_county.csv")
ec_data = rename(ec_data, "county_code" = "county")

# County demographic data
county_data = read.csv("Data/raw/county_demographics.csv")
county_data = rename(county_data, "county_code" = "fipscode")
county_data$diversity <- 1 - (county_data$pop_white_perc^2 + county_data$pop_black_perc^2 + county_data$pop_hispanic_perc^2)
ec_data_merged <- ec_data %>%
  left_join(county_data, by = c("county_code"))
ec_data_merged$county = ec_data_merged$county_code - ec_data_merged$state_fips*1000

# Education data
education_data = read.csv("Data/raw/education.csv", skip=4)
names(education_data)[1] <- "county_code"
names(education_data)[2] <- "state"
names(education_data)[3] <- "name"
names(education_data)[4] <- "2003_rucc"
names(education_data)[5] <- "2003_uic"
names(education_data)[6] <- "2013_rucc"
names(education_data)[7] <- "2013_uic"
names(education_data)[8] <- "1970_ls_hs"
names(education_data)[9] <- "1970_hs_only"
names(education_data)[10] <- "1970_some_college"
names(education_data)[11] <- "1970_gte_college"
names(education_data)[12] <- "1970_pct_ls_hs"
names(education_data)[13] <- "1970_pct_hs_only"
names(education_data)[14] <- "1970_pct_some_college"
names(education_data)[15] <- "1970_pct_gte_college"
names(education_data)[16] <- "1980_ls_hs"
names(education_data)[17] <- "1980_hs_only"
names(education_data)[18] <- "1980_some_college"
names(education_data)[19] <- "1980_gte_college"
names(education_data)[20] <- "1980_pct_ls_hs"
names(education_data)[21] <- "1980_pct_hs_only"
names(education_data)[22] <- "1980_pct_some_college"
names(education_data)[23] <- "1980_pct_gte_college"
names(education_data)[24] <- "1990_ls_hs"
names(education_data)[25] <- "1990_hs_only"
names(education_data)[26] <- "1990_some_college"
names(education_data)[27] <- "1990_gte_college"
names(education_data)[28] <- "1990_pct_ls_hs"
names(education_data)[29] <- "1990_pct_hs_only"
names(education_data)[30] <- "1990_pct_some_college"
names(education_data)[31] <- "1990_pct_gte_college"
names(education_data)[32] <- "2000_ls_hs"
names(education_data)[33] <- "2000_hs_only"
names(education_data)[34] <- "2000_some_college"
names(education_data)[35] <- "2000_gte_college"
names(education_data)[36] <- "2000_pct_ls_hs"
names(education_data)[37] <- "2000_pct_hs_only"
names(education_data)[38] <- "2000_pct_some_college"
names(education_data)[39] <- "2000_pct_gte_college"
names(education_data)[40] <- "2007_11_ls_hs"
names(education_data)[41] <- "2007_11_hs_only"
names(education_data)[42] <- "2007_11_some_college"
names(education_data)[43] <- "2007_11_gte_college"
names(education_data)[44] <- "2007_11_pct_ls_hs"
names(education_data)[45] <- "2007_11_pct_hs_only"
names(education_data)[46] <- "2007_11_pct_some_college"
names(education_data)[47] <- "2007_11_pct_gte_college"
names(education_data)[48] <- "2016_20_ls_hs"
names(education_data)[49] <- "2016_20_hs_only"
names(education_data)[50] <- "2016_20_some_college"
names(education_data)[51] <- "2016_20_gte_college"
names(education_data)[52] <- "2016_20_pct_ls_hs"
names(education_data)[53] <- "2016_20_pct_hs_only"
names(education_data)[54] <- "2016_20_pct_some_college"
# NEED TO UPDATE ALL VAR NAMES SO THEY DON'T START WITH NUMS
# Only use pct_gte_college_2016_20 for now though
names(education_data)[55] <- "pct_gte_college_2016_20"
education_data[55] <- education_data[55]/100
education_data$county_code <- as.numeric(education_data$county_code)
ec_data_merged <- ec_data_merged %>%
  left_join(education_data, by = c("county_code"))

# Unemployment
unemploy_data = read.csv("Data/raw/unemployment.csv", skip=4)
unemploy_data = subset(unemploy_data, FIPS_code%%1000 != 0)
unemploy_data = rename(unemploy_data, "county_code" = "FIPS_code")
unemploy_data$Median_Household_Income_2020 = as.numeric(gsub(",", "", as.character(unemploy_data$Median_Household_Income_2020)))
unemploy_data$Median_Household_Income_2020 = unemploy_data$Median_Household_Income_2020 / 1000
unemploy_data$Unemployment_rate_2021 <- as.numeric(unemploy_data$Unemployment_rate_2021) / 100
ec_data_merged <- ec_data_merged %>%
  left_join(unemploy_data, by = c("county_code"))

# GDP
gdp_data = read.csv("Data/raw/gdp.csv")
gdp_data$GeoFIPS = as.double(noquote(substring(gdp_data$GeoFIPS, 3, nchar(gdp_data$GeoFIPS)-1)))
gdp_data = rename(gdp_data, "county_code" = "GeoFIPS")
gdp_data = subset(gdp_data, LineCode == 1)
gdp_data$X2020 = as.double(gdp_data$X2020)
ec_data_merged <- ec_data_merged %>%
  left_join(gdp_data, by = c("county_code"))

# EPA Data
epa_data = read.csv("Data/raw/walkability_index.csv")
cols = epa_data %>% select(
  STATEFP,
  COUNTYFP,
  TotPop,
  Ac_Total,
  D1B,
  D1C8_RET,
  D1C8_OFF,
  D1C8_IND,
  D1C8_SVC,
  D1C8_ENT,
  D1C8_ED,
  D1C8_HLTH,
  D1C8_PUB,
  D2R_WRKEMP,
  D1D,
  D3A,
  D3B,
  D3APO,
  D3AAO,
  D3AMM,
  D3B,
  D3BPO4,
  
  P_WrkAge, # Percent working age
  Pct_AO0, # Percent owning 0 cars
  Pct_AO1,
  Pct_AO2p,
  D2B_E8MIX, # Employment entropy
  D2B_E8MIXA, # Employment entropy (different denominator - don't understand)
  D2A_EPHHM, # Household and employment entropy
  D2C_TRPMX1, # Different types of diversity - need to figure out what they mean
  D2C_TRPMX2,
  D2R_JOBPOP,
  NatWalkInd,
  D2A_WRKEMP
)
# Area weighted variables
means <- cols %>%
  group_by(STATEFP, COUNTYFP) %>%
  ## Ac_Total is total acres
  summarize_at(vars(D1B:D3BPO4),funs(weighted.mean(., w=Ac_Total)))
means = rename(means, "state_fips" = "STATEFP")
means = rename(means, "county" = "COUNTYFP")
ec_data_merged <- ec_data_merged %>%
  left_join(means, by = c("county", "state_fips"))

# Population weighted variables
means <- cols %>%
  group_by(STATEFP, COUNTYFP) %>%
  ## Ac_Total is total acres
  summarize_at(vars(P_WrkAge:D2A_WRKEMP),funs(weighted.mean(., w=TotPop)))
means = rename(means, "state_fips" = "STATEFP")
means = rename(means, "county" = "COUNTYFP")
ec_data_merged <- ec_data_merged %>%
  left_join(means, by = c("county", "state_fips"))

means <- cols %>% 
  group_by(STATEFP, COUNTYFP) %>% 
  summarise(Ac_Total = sum(Ac_Total))
means = rename(means, "state_fips" = "STATEFP")
means = rename(means, "county" = "COUNTYFP")
ec_data_merged <- ec_data_merged %>%
  left_join(means, by = c("county", "state_fips"))

# OI County data
oi_data = read.csv("Data/raw/cty_covariates.csv")
oi_data = rename(oi_data, "state_fips" = "state")
oi_data$oi_diversity <- 1 - (oi_data$share_white2010^2 + oi_data$share_black2010^2 + oi_data$share_hisp2010^2 + oi_data$share_asian2010^2)
ec_data_merged <- ec_data_merged %>%
  left_join(oi_data, by = c("county", "state_fips"))

ec_data_merged$share_other = 1 - (ec_data_merged$share_asian2010 + ec_data_merged$share_hisp2010 + ec_data_merged$share_white2010 + ec_data_merged$share_black2010)

ec_data_merged$white_pop <- ec_data_merged$share_white2010 * ec_data_merged$pop2018
ec_data_merged$black_pop <- ec_data_merged$share_black2010 * ec_data_merged$pop2018
ec_data_merged$hisp_pop <- ec_data_merged$share_hisp2010 * ec_data_merged$pop2018
ec_data_merged$asian_pop <- ec_data_merged$share_asian2010 * ec_data_merged$pop2018
ec_data_merged$other_pop <- ec_data_merged$share_other * ec_data_merged$pop2018


d <- (ec_data_merged$white_pop * (ec_data_merged$white_pop - 1)) + 
     (ec_data_merged$black_pop * (ec_data_merged$black_pop - 1)) +
     (ec_data_merged$hisp_pop * (ec_data_merged$hisp_pop - 1)) + 
     (ec_data_merged$asian_pop * (ec_data_merged$asian_pop - 1)) +
     (ec_data_merged$other_pop * (ec_data_merged$other_pop - 1))

d <- d/(ec_data_merged$pop2018 * (ec_data_merged$pop2018 - 1))

ec_data_merged$simpsons_index <- 1-d

# County Characteristics Data
cty_chars_data = read.csv("Data/raw/health_ineq_online_table_12.csv")
cty_chars_data = rename(cty_chars_data, "county_code" = "cty")
ec_data_merged <- ec_data_merged %>%
  left_join(cty_chars_data, by = c("county_code"))

# Urban/Rural Classification
urban <- read.csv("Data/raw/urban.csv")
urban = rename(urban, "county_code" = "FIPS.code")
ec_data_merged <- ec_data_merged %>%
  left_join(urban, by = "county_code")

# Add urban indicator
ec_data_merged$urban <- NA
ec_data_merged[ec_data_merged$X2013.code > 3,]$urban <- 0
ec_data_merged[ec_data_merged$X2013.code <= 3,]$urban <- 1

# Write to working data folder
write.csv(ec_data_merged, "Data/working/data_merged.csv")
