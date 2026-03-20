# ==============================================================================
# Script:      3_explore_AZ_data.R
# Project:     Arizona Residential Water Use Analysis (Flume Smart Meter Study)
# Description: Exploratory data analysis of daily indoor and outdoor household
#              water use across Arizona for Water Year 2022 (Oct 2021 - Sep 2022).
#              Integrates Flume smart meter data with household characteristics
#              and AZMET weather station data (precipitation, ET, temperature).
#
#              Analyses include:
#                - Data ingestion, cleaning, and wide-format reshaping
#                - Daily/weekly/household-level water use summaries (GPHD, GPCD)
#                - Multiple linear regression (indoor, outdoor, total use)
#                - Random Forest variable importance
#                - Principal Component Analysis (PCA)
#                - Classification and regression trees (train/test validation)
#                - Non-parametric group comparisons (Kruskal-Wallis, Dunn test)
#                - Stepwise regression (Flume 2 subset)
#
# Author:      Cibi Vishnu Chinnasamy
# Affiliation: Colorado State University
# Contact:     cibivishnuc@gmail.com
# Date:        2023-10-23
# Last updated:2026-03-21]
# License:     CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/)
#              You are free to share and adapt this code with attribution.
#
# Input files: (all expected in the working directory set below)
#   - AZ_Households.xlsx                         : Household demographics & building info
#   - AZ_FLUME_Zipcode_AZMET-stations.csv        : Zip code to AZMET station mapping
#   - AZ Water Use.csv                           : Daily indoor/outdoor GPHD per household
#   - AZ_Daily_Weather_Precip_ET_Temp_2021-22.csv: Daily weather from AZMET stations
#   - az disag.xlsx                              : Flume 2 fixture-level disaggregation data
#
# Notes:
#   - Location_ID 16003 (City = "Tbd", PostalCode = 66666) is excluded throughout
#     due to missing geographic information.
#   - Update setwd() path below before running.
#   - Fixture efficiency classification (Section 7.b.ii) is performed within this script.
#     HOME_Efficiency and INDOOR_Efficiency are composite scores derived from
#     fixture-level PAM cluster results — add derivation logic at end of Section 7.b.ii
#     if not already present in flume2_AZ_households_info from az disag.xlsx.
# ==============================================================================

# Set working directory to the folder containing all input files listed above
setwd("path/to/your/data/folder")  # <-- update before running

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------
# Data import & manipulation
library(readxl)       # read_excel(): import .xlsx files
library(lubridate)    # dmy(), month(): date parsing and extraction
library(dplyr)        # left_join(), mutate(), select(), summarize()
library(tidyr)        # spread(): reshape long data to wide format
library(tibble)       # rownames_to_column(): tidy row name handling

# Summary statistics
library(psych)        # describeBy(): grouped descriptive statistics

# Visualization
library(ggplot2)      # Core plotting
library(ggpubr)       # theme_classic2(): clean ggplot theme
library(gridExtra)    # grid.arrange(): multi-panel plot layouts
library(patchwork)    # plot_layout(): combine ggplot objects
library(corrplot)     # corrplot(): correlation matrix visualization

# Time series
library(zoo)          # zoo(): indexed time series; aggregate() by week

# Machine learning & multivariate analysis
library(randomForest) # randomForest(): variable importance and prediction
library(factoextra)   # fviz_pca_var(): PCA biplots
library(tree)         # tree(): classification and regression trees

# Missing data
library(VIM)          # aggr(): visualize missing data patterns

# Statistical tests
library(FSA)          # dunnTest(): post-hoc Dunn test after Kruskal-Wallis




# ==============================================================================
# SECTION 1: HOUSEHOLD DATA INGESTION & CLEANING
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.a Load household characteristics
# ------------------------------------------------------------------------------
# AZ_Households.xlsx contains demographic and building info for 1,258 households.
# Location_ID 16003 (City = "Tbd", PostalCode = 66666) is excluded throughout
# this script — it has valid water use data but no usable geographic information.

households_info <- read_excel("AZ_Households.xlsx")
households_info <- households_info[!households_info$City == 'Tbd', ]
households_info$IrrigationFrequency <- replace(households_info$IrrigationFrequency,
                                               households_info$IrrigationFrequency == 'N/A', NA)
households_info$Location_ID <- as.factor(households_info$Location_ID)


# ------------------------------------------------------------------------------
# 1.b Fill missing county names using postal code lookup
# ------------------------------------------------------------------------------
# Some households have a missing County field. This block builds a complete
# postal code -> county name lookup by:
#   (1) Extracting county names where they exist in the data
#   (2) Manually filling the 9 postal codes with no county match (looked up externally)
#   (3) Backfilling the County column in households_info using the completed lookup

# Step 1: Build postal code -> county lookup from existing data
county <- data.frame(PostalCode = unique(households_info$PostalCode),
                     County_name = NA)

for(i in 1:length(county$PostalCode)){
  for(j in 1:length(households_info$PostalCode)){
    if(county$PostalCode[i] == households_info$PostalCode[j] &
       !is.na(households_info$County[j])){
      county$County_name[i] <- households_info$County[j]
    }
  }
}

missing_county_names <- county[is.na(county$County_name), ]   # postal codes still unmatched
county <- county[complete.cases(county), ]                     # retain matched postal codes only

# Step 2: Manually fill the 9 unmatched postal codes (sourced externally)
missing_county_names_filled <- data.frame(
  PostalCode  = c("85646",       "85539", "85707", "86429",  "85305",    "86432",  "85363",    "85929",   "86403"),
  County_name = c("SANTA CRUZ",  "GILA",  "PIMA",  "MOHAVE", "MARICOPA", "MOHAVE", "MARICOPA", "NAVAJO",  "MOHAVE")
)

# Step 3: Merge and backfill County in households_info
county_postalcode <- rbind(county, missing_county_names_filled)
households_info$County <- county_postalcode$County_name[match(households_info$PostalCode,
                                                              county_postalcode$PostalCode)]

# Optional: export the completed zip code -> county lookup table
# write.csv(county_postalcode, "AZ_FLUME_household_zipcodes.csv")

# Create a complete-cases version of households_info (no NAs in any column)
households_info_COMPLETE <- households_info[complete.cases(households_info), ]


# ------------------------------------------------------------------------------
# 1.c Join nearest AZMET weather station to each household via postal code
# ------------------------------------------------------------------------------
# AZ_FLUME_Zipcode_AZMET-stations.csv maps each zip code to its nearest
# AZMET station (by distance in km). Columns 2, 5, 6, 7 are retained:
# PostalCode, AZMET_FID, distance (km), and Station_name.

AZMET_Station <- read.csv("AZ_FLUME_Zipcode_AZMET-stations.csv")
AZMET_Station <- AZMET_Station[, c(2, 6, 5, 7)]
colnames(AZMET_Station) <- c("PostalCode", "AZMET_FID",
                             "Station_distance_to_zipcode(KM)", "Station_name")

households_info$AZMET_Station <- AZMET_Station$Station_name[match(households_info$PostalCode,
                                                                  AZMET_Station$PostalCode)]
households_info$AZMET_FID <- AZMET_Station$AZMET_FID[match(households_info$PostalCode,
                                                           AZMET_Station$PostalCode)]

# Lightweight lookup table used later to join weather station info to water use data
for_weather_consolidation <- households_info[, c(1, 2, 3, 5, 15, 16)]
# Columns: Location_ID, City, County, PostalCode, AZMET_Station, AZMET_FID


# ------------------------------------------------------------------------------
# 1.d Group households by geographic and weather station clusters
# ------------------------------------------------------------------------------
# Split households_info into named lists for grouped analysis.
# Each list element is a subset data frame for one city / zip / county / station.

city_households    <- split(households_info, households_info$City)
zipcode_households <- split(households_info, households_info$PostalCode)
county_households  <- split(households_info, households_info$County)
weather_households <- split(households_info, households_info$AZMET_Station)




# ==============================================================================
# SECTION 2: DAILY WATER USE — INGESTION, WRANGLING & WEATHER JOIN
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.a Load and clean raw daily water use data
# ------------------------------------------------------------------------------
# AZ Water Use.csv contains daily indoor and outdoor water use (in gallons per
# household per day, GPHD) for each Location_ID in long format. Each row is one
# use-type measurement (Indoor GPHD or Outdoor GPHD) for one household on one day.

wateruse_raw <- read.csv("AZ Water Use.csv")
colnames(wateruse_raw)[1:2] <- c("Location_ID", "Date")
wateruse_raw <- wateruse_raw[!wateruse_raw$Location_ID == "16003", ]  # exclude City = "Tbd"
wateruse_raw$Location_ID <- as.factor(wateruse_raw$Location_ID)
wateruse_raw$Date <- dmy(wateruse_raw$Date)   # Convert date column into a Date class
wateruse_raw <- wateruse_raw[order(wateruse_raw[, 1], wateruse_raw[, 2]), ]  # sort by ID then date

# A small number of records have negative GPHD values (meter artifacts); set to zero
wateruse_raw$Measure.Values <- replace(wateruse_raw$Measure.Values,
                                       wateruse_raw$Measure.Values <= 0, 0)



# ------------------------------------------------------------------------------
# 2.b Reshape from long to wide: separate Indoor, Outdoor, and Total GPHD
# ------------------------------------------------------------------------------
# Split Measure.Names into three separate data frames, then join side by side.
# Total GPHD is computed as the row sum of Indoor + Outdoor via aggregate().

wateruse_raw_total <- aggregate(Measure.Values ~ Location_ID + Date,
                                data = wateruse_raw, FUN = sum, na.rm = TRUE)
colnames(wateruse_raw_total) <- c("Location_ID", "Date", "Total.GPHD")

wateruse_raw_indoor <- wateruse_raw[wateruse_raw$Measure.Names == "Indoor GPHD", c(1, 2, 4)]
colnames(wateruse_raw_indoor) <- c("Location_ID", "Date", "Indoor.GPHD")

wateruse_raw_outdoor <- wateruse_raw[wateruse_raw$Measure.Names == "Outdoor GPHD", c(1, 2, 4)]
colnames(wateruse_raw_outdoor) <- c("Location_ID", "Date", "Outdoor.GPHD")

# Join into a single wide-format data frame: one row per household per day
wateruse <- wateruse_raw_indoor %>%
  left_join(wateruse_raw_outdoor, by = c('Location_ID', 'Date')) %>%
  left_join(wateruse_raw_total,   by = c('Location_ID', 'Date'))



# ------------------------------------------------------------------------------
# 2.c Join AZMET station identifiers to daily water use
# ------------------------------------------------------------------------------
# Station name and FID are added here so the weather join in 2.d can match on
# AZMET_FID across both the water use and weather data frames.

wateruse$AZMET_Station <- for_weather_consolidation$AZMET_Station[
  match(wateruse$Location_ID, for_weather_consolidation$Location_ID)
]
wateruse$AZMET_FID <- for_weather_consolidation$AZMET_FID[
  match(wateruse$Location_ID, for_weather_consolidation$Location_ID)
]


# ------------------------------------------------------------------------------
# 2.d Load daily weather data and filter to Water Year 2022
# ------------------------------------------------------------------------------
# AZ_Daily_Weather_Precip_ET_Temp_2021-22.csv contains daily precipitation (in),
# Penman ET (in), and max air temperature (°F) from AZMET stations.
# Water Year 2022 spans Oct 1, 2021 – Sep 30, 2022.
#
# Note: an earlier version of this file (AZ_Daily_Weather_Precip_ET_2021-22.csv)
# did not include temperature and has been superseded by the file loaded below.

AZ_weather_daily <- read.csv("AZ_Daily_Weather_Precip_ET_Temp_2021-22.csv")
AZ_weather_daily$Date <- as.Date(AZ_weather_daily$Date, format = '%m/%d/%Y')
AZ_weather_daily$AZMET_Station <- as.factor(AZ_weather_daily$AZMET_Station)
AZ_weather_daily$Month <- month(AZ_weather_daily$Date)

# Filter to Water Year 2022
AZ_weather_daily_WY22 <- AZ_weather_daily[AZ_weather_daily$Date > "2021-09-30" &
                                            AZ_weather_daily$Date < "2022-10-01", ]

# Join weather to water use, matching on both Date and AZMET_FID.
# Columns retained from weather: Date, AZMET_FID, DAY_Penman (ET),
# DAY_Precip (precip), AirTempMax.DegF (max temp)
wateruse_wide <- wateruse %>%
  left_join(AZ_weather_daily_WY22[, c(2, 4, 7, 9, 11)],
            by = c('Date' = 'Date', 'AZMET_FID' = 'AZMET_FID'))

# Join household characteristics (building info, demographics, irrigation)
wateruse_wide <- wateruse_wide %>%
  left_join(households_info[, c(1:3, 5:14)], by = 'Location_ID')



# ------------------------------------------------------------------------------
# 2.e Derive per-capita use and finalize wateruse_wide
# ------------------------------------------------------------------------------
# GPCD (gallons per capita per day) = GPHD / number of residents

wateruse_wide$Indoor.GPCD  <- wateruse_wide$Indoor.GPHD  / wateruse_wide$NumResidents
wateruse_wide$Outdoor.GPCD <- wateruse_wide$Outdoor.GPHD / wateruse_wide$NumResidents
wateruse_wide$Total.GPCD   <- wateruse_wide$Total.GPHD   / wateruse_wide$NumResidents

wateruse_wide$Month <- month(wateruse_wide$Date)

# Round all water use columns to 2 decimal places
wateruse_wide <- wateruse_wide %>%
  mutate_at(vars(Indoor.GPHD, Outdoor.GPHD, Total.GPHD,
                 Indoor.GPCD,  Outdoor.GPCD,  Total.GPCD), funs(round(., 2)))

# Convert categorical fixture/irrigation columns to factors
wateruse_wide <- wateruse_wide %>%
  mutate_at(c('HasPool', 'IrrigationFrequency', 'IrrigationType'), as.factor)

# Reorder columns: Location_ID | Date | Month | weather | GPHD | GPCD | household info
# Final order: col 1 (ID), 2 (Date), 26 (Month), 11:13 (weather), 6-7 (station),
#              3:5 (GPHD), 23:25 (GPCD), 8:10 (station IDs), 14:22 (household chars)
wateruse_wide <- wateruse_wide[, c(1, 2, 26, 11:13, 6, 7, 3:5, 23:25, 8:10, 14:22)]

# Optional: export the fully joined daily water use dataset
# write.csv(wateruse_wide, "AZ_Wateruse_Wide.csv")



# ------------------------------------------------------------------------------
# 2.f Aggregate daily weather across all AZMET stations
# ------------------------------------------------------------------------------
# Pivot weather data to wide format (one column per station) then compute daily
# mean and median across stations. These station-aggregated summaries are used
# as region-wide climate signals in subsequent analyses.
#
# Note: Mean precipitation and ET are preferred over median as the primary
# climate metrics for regression models (more sensitive to high-precip events).

AZ_Precip_daily_WY22  <- spread(AZ_weather_daily_WY22[, c(3, 4, 9)],  AZMET_Station, "DAY_Precip")
AZ_ET_daily_WY22      <- spread(AZ_weather_daily_WY22[, c(3, 4, 7)],  AZMET_Station, "DAY_Penman")
AZ_TempMax_daily_WY22 <- spread(AZ_weather_daily_WY22[, c(3, 4, 11)], AZMET_Station, "AirTempMax.DegF.")

AZ_Precip_daily_WY22$Median.Precip   <- round(apply(AZ_Precip_daily_WY22[2:ncol(AZ_Precip_daily_WY22)],   1, median, na.rm = TRUE), 2)
AZ_Precip_daily_WY22$Mean.Precip     <- round(apply(AZ_Precip_daily_WY22[2:ncol(AZ_Precip_daily_WY22)],   1, mean,   na.rm = TRUE), 2)
AZ_ET_daily_WY22$Median.ET           <- round(apply(AZ_ET_daily_WY22[2:ncol(AZ_ET_daily_WY22)],           1, median, na.rm = TRUE), 2)
AZ_ET_daily_WY22$Mean.ET             <- round(apply(AZ_ET_daily_WY22[2:ncol(AZ_ET_daily_WY22)],           1, mean,   na.rm = TRUE), 2)
AZ_TempMax_daily_WY22$Median.TempMax <- round(apply(AZ_TempMax_daily_WY22[2:ncol(AZ_TempMax_daily_WY22)], 1, median, na.rm = TRUE), 2)
AZ_TempMax_daily_WY22$Mean.TempMax   <- round(apply(AZ_TempMax_daily_WY22[2:ncol(AZ_TempMax_daily_WY22)], 1, mean,   na.rm = TRUE), 2)




# ------------------------------------------------------------------------------
# 2.g Exploratory weather plots — station comparison and regional summaries
# ------------------------------------------------------------------------------
# These plots were used to visually QC the weather data and assess inter-station
# variability in precipitation and temperature across WY2022.

# Daily precipitation by station (colored lines)
ggplot(AZ_weather_daily_WY22, aes(x = Date, y = DAY_Precip)) + theme_classic() +
  geom_line(aes(color = AZMET_Station))

# Daily max temperature by station
ggplot(AZ_weather_daily_WY22, aes(x = Date, y = AirTempMax.DegF.)) + theme_classic() +
  geom_line(aes(color = AZMET_Station))

# Individual station precip (grey) overlaid with regional mean precip (black)
a0 <- ggplot() + theme_classic() +
  geom_line(data = AZ_weather_daily_WY22,  aes(x = Date, y = DAY_Precip, color = AZMET_Station)) +
  scale_color_grey() +
  geom_line(data = AZ_Precip_daily_WY22, aes(x = Date, y = Mean.Precip), color = 'black')

# Median and mean daily precipitation and ET across all stations
a1 <- ggplot(AZ_Precip_daily_WY22, aes(x = Date, y = Median.Precip)) + geom_line() + theme_classic2()
a2 <- ggplot(AZ_Precip_daily_WY22, aes(x = Date, y = Mean.Precip))   + geom_line() + theme_classic2()
b1 <- ggplot(AZ_ET_daily_WY22,     aes(x = Date, y = Median.ET))     + geom_line() + theme_classic2()
b2 <- ggplot(AZ_ET_daily_WY22,     aes(x = Date, y = Mean.ET))       + geom_line() + theme_classic2()

grid.arrange(a1, a0 + theme(legend.position = "none"), a2)






# ==============================================================================
# SECTION 3: INDOOR WATER USE ANALYSIS — ALL ARIZONA HOUSEHOLDS
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.a Filter daily data and pivot to wide format
# ------------------------------------------------------------------------------

# Type-1 filter: retain days where Indoor GPHD >= 10.
# This removes near-zero readings likely reflecting absent households or meter
# gaps, while retaining high-end outliers for the full-AZ dataset.
wateruse_wide_indoor_subset <- subset(wateruse_wide, Indoor.GPHD >= 10.0)


# !! Alternative !! Type-2 filter (stricter outlier exclusion) is shown below but
# not used in the primary analysis:
#   wateruse_wide_indoor_subset <- subset(wateruse_wide, Indoor.GPHD >= 5 &
#                                         Indoor.GPHD <= 1500)  # raw max = 25,313


# Pivot to wide format: one row per date, one column per household (Location_ID)
# GPHD wide: used to compute per-household averages and medians
wateruse_wide_indoorGPHD_daily <- spread(wateruse_wide_indoor_subset[, c(1, 2, 9)],
                                         Location_ID, Indoor.GPHD)
households_names <- colnames(wateruse_wide_indoorGPHD_daily)  # household ID vector (incl. Date)

# GPCD wide: same structure but per-capita values
wateruse_wide_indoorGPCD_daily <- spread(wateruse_wide_indoor_subset[, c(1, 2, 12)],
                                         Location_ID, Indoor.GPCD)
households_names2 <- colnames(wateruse_wide_indoorGPCD_daily)

# Optional: export wide-format daily indoor use tables
# write.csv(wateruse_wide_indoorGPHD_daily, "AZ_daily_indoorGPHD.csv")
# write.csv(wateruse_wide_indoorGPCD_daily, "AZ_daily_indoorGPCD.csv")





# ------------------------------------------------------------------------------
# 3.b Daily regional averages and seasonal distribution plots
# ------------------------------------------------------------------------------
# Compute daily mean and median across all households for both GPCD and GPHD.
# These regional summaries are used to visualize seasonal patterns in indoor use.

indoor.daily.avg.med <- data.frame(
  Date                = wateruse_wide_indoorGPCD_daily$Date,
  Average.Indoor.GPCD = round(apply(wateruse_wide_indoorGPCD_daily[2:ncol(wateruse_wide_indoorGPCD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.Indoor.GPCD  = round(apply(wateruse_wide_indoorGPCD_daily[2:ncol(wateruse_wide_indoorGPCD_daily)], 1, median, na.rm = TRUE), 2),
  Average.Indoor.GPHD = round(apply(wateruse_wide_indoorGPHD_daily[2:ncol(wateruse_wide_indoorGPHD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.Indoor.GPHD  = round(apply(wateruse_wide_indoorGPHD_daily[2:ncol(wateruse_wide_indoorGPHD_daily)], 1, median, na.rm = TRUE), 2)
)
indoor.daily.avg.med$Month <- as.factor(month(indoor.daily.avg.med$Date))

# Monthly boxplots of daily average and median indoor use (GPCD and GPHD)
aa <- ggplot(indoor.daily.avg.med, aes(x = Month, y = Average.Indoor.GPCD, group = Month)) +
  geom_boxplot() + ylim(30, 70) + theme_classic2()
bb <- ggplot(indoor.daily.avg.med, aes(x = Month, y = Median.Indoor.GPCD,  group = Month)) +
  geom_boxplot() + ylim(30, 70) + theme_classic2()
cc <- ggplot(indoor.daily.avg.med, aes(x = Month, y = Average.Indoor.GPHD, group = Month)) +
  geom_boxplot() + ylim(100, 175) + theme_classic2()
dd <- ggplot(indoor.daily.avg.med, aes(x = Month, y = Median.Indoor.GPHD,  group = Month)) +
  geom_boxplot() + ylim(100, 175) + theme_classic2()
grid.arrange(aa, bb, cc, dd, ncol = 2)

# Time series of median indoor GPHD across the full water year
ggplot(indoor.daily.avg.med, aes(x = Date, y = Median.Indoor.GPHD)) +
  geom_line() + theme_classic2()



# ------------------------------------------------------------------------------
# 3.c Build per-household summary tables and join household characteristics
# ------------------------------------------------------------------------------
# Collapse the wide daily matrices into one row per household, computing the
# mean and median of each household's daily indoor use over the full water year.

wateruse_indoorGPHD <- data.frame(
  Location_ID         = households_names[-1],
  Average.Indoor.GPHD = t(round(summarize_all(wateruse_wide_indoorGPHD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Indoor.GPHD  = t(round(summarize_all(wateruse_wide_indoorGPHD_daily[-1], median, na.rm = TRUE), 2))
)

wateruse_indoorGPCD <- data.frame(
  Location_ID         = households_names2[-1],
  Average.Indoor.GPCD = t(round(summarize_all(wateruse_wide_indoorGPCD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Indoor.GPCD  = t(round(summarize_all(wateruse_wide_indoorGPCD_daily[-1], median, na.rm = TRUE), 2))
)

# Join household characteristics to each per-household summary table
# (City, County, PostalCode, YearBuilt, HomeSize, LotSize, HomeValue,
#  NumBathrooms, NumResidents, HasPool)
for (df_name in c("wateruse_indoorGPHD", "wateruse_indoorGPCD")) {
  df <- get(df_name)
  df$City           <- wateruse_wide$City[match(df$Location_ID,           wateruse_wide$Location_ID)]
  df$County         <- wateruse_wide$County[match(df$Location_ID,         wateruse_wide$Location_ID)]
  df$PostalCode     <- wateruse_wide$PostalCode[match(df$Location_ID,     wateruse_wide$Location_ID)]
  df$YearBuilt      <- wateruse_wide$YearBuilt[match(df$Location_ID,      wateruse_wide$Location_ID)]
  df$HomeSize_Sq.Ft <- wateruse_wide$HomeSize_Sq.Ft[match(df$Location_ID, wateruse_wide$Location_ID)]
  df$LotSize_Sq.Ft  <- wateruse_wide$LotSize_Sq.Ft[match(df$Location_ID,  wateruse_wide$Location_ID)]
  df$HomeValue_USD  <- wateruse_wide$HomeValue_USD[match(df$Location_ID,   wateruse_wide$Location_ID)]
  df$NumBathrooms   <- wateruse_wide$NumBathrooms[match(df$Location_ID,    wateruse_wide$Location_ID)]
  df$NumResidents   <- wateruse_wide$NumResidents[match(df$Location_ID,    wateruse_wide$Location_ID)]
  df$HasPool        <- as.factor(wateruse_wide$HasPool[match(df$Location_ID, wateruse_wide$Location_ID)])
  assign(df_name, df)
}

# HasPool converted to numeric in the GPCD table for use in correlation matrix
wateruse_indoorGPCD$HasPool <- as.numeric(wateruse_indoorGPCD$HasPool)


# ------------------------------------------------------------------------------
# 3.d Indoor use vs. number of residents — quick summary and regression
# ------------------------------------------------------------------------------
# Summarize average indoor GPCD and GPHD by household size (NumResidents) to
# check the expected scaling relationship before running full MLR models.

df <- data.frame(
  summarise(group_by(wateruse_indoorGPCD, NumResidents),
            Mean.GPCD   = mean(Average.Indoor.GPCD),
            Median.GPCD = median(Average.Indoor.GPCD),
            Max.GPCD    = max(Average.Indoor.GPCD),
            Min.GPCD    = min(Average.Indoor.GPCD)),
  summarise(group_by(wateruse_indoorGPHD, NumResidents),
            Mean.GPHD   = mean(Average.Indoor.GPHD),
            Median.GPHD = median(Average.Indoor.GPHD),
            Max.GPHD    = max(Average.Indoor.GPHD),
            Min.GPHD    = min(Average.Indoor.GPHD))
)

ggplot(df, aes(NumResidents, Mean.GPHD)) + geom_area() + theme_bw()
plot(df$NumResidents, df$Mean.GPCD, type = "l")

# Simple linear regression: GPHD ~ NumResidents
a <- lm(Mean.GPHD ~ NumResidents, data = df)
summary(a)




# ------------------------------------------------------------------------------
# 3.e Multiple linear regression — Indoor GPHD
# ------------------------------------------------------------------------------
# Predictors: YearBuilt, HomeSize, HomeValue, NumResidents, NumBathrooms,
#             LotSize, HasPool
# Models are run for: (1) all AZ, (2) Maricopa, (3) Pima, (4) all other counties.
# Maricopa and Pima are separated due to their dominant household counts.
#
# Note: Average.Indoor.GPHD outperforms Median.Indoor.GPHD when high-end
# outliers (Indoor.GPHD > 15,000) are excluded via the Type-2 filter above.

# All Arizona
fit.avg.indoor.GPHD <- lm(Average.Indoor.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                            HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft + HasPool,
                          data = wateruse_indoorGPHD, na.action = na.exclude)
summary(fit.avg.indoor.GPHD)

fit.med.indoor.GPHD <- lm(Median.Indoor.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                            HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft + HasPool,
                          data = wateruse_indoorGPHD, na.action = na.exclude)
summary(fit.med.indoor.GPHD)

# Maricopa County
maricopa.wateruse_indoorGPHD <- wateruse_indoorGPHD[wateruse_indoorGPHD$County == "MARICOPA", ]
fit.maricopa.avg.indoor.GPHD <- lm(Average.Indoor.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                                     HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft + HasPool,
                                   data = maricopa.wateruse_indoorGPHD, na.action = na.exclude)
summary(fit.maricopa.avg.indoor.GPHD)

# Pima County
pima.wateruse_indoorGPHD <- wateruse_indoorGPHD[wateruse_indoorGPHD$County == "PIMA", ]
fit.pima.avg.indoor.GPHD <- lm(Average.Indoor.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                                 HomeValue_USD + NumResidents + LotSize_Sq.Ft + NumBathrooms + HasPool,
                               data = pima.wateruse_indoorGPHD, na.action = na.exclude)
summary(fit.pima.avg.indoor.GPHD)

# All other counties (excluding Maricopa and Pima)
others.wateruse_indoorGPHD <- wateruse_indoorGPHD[wateruse_indoorGPHD$County != "PIMA" &
                                                    wateruse_indoorGPHD$County != "MARICOPA", ]
fit.others.avg.indoor.GPHD <- lm(Average.Indoor.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                                   HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft + HasPool,
                                 data = others.wateruse_indoorGPHD, na.action = na.exclude)
summary(fit.others.avg.indoor.GPHD)



# ------------------------------------------------------------------------------
# 3.f Multiple linear regression — Indoor GPCD
# ------------------------------------------------------------------------------
# Same county structure as 3.e but using per-capita use as the response variable.
# Correlation matrices are included for the all-AZ and Maricopa subsets to check
# for multicollinearity among predictors before interpreting model coefficients.

# All Arizona
fit.avg.indoor.GPCD <- lm(Average.Indoor.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                            HomeValue_USD + LotSize_Sq.Ft + HasPool + NumBathrooms + NumResidents,
                          data = wateruse_indoorGPCD)
summary(fit.avg.indoor.GPCD)

# Correlation matrix — all AZ indoor GPCD predictors
M_indoor_AZ <- cor(wateruse_indoorGPCD[, c(2, 3, 7:12)], use = "complete.obs")
corrplot(M_indoor_AZ, method = 'number')

fit.med.indoor.GPCD <- lm(Median.Indoor.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                            HomeValue_USD + LotSize_Sq.Ft + HasPool + NumBathrooms + NumResidents,
                          data = wateruse_indoorGPCD)
summary(fit.med.indoor.GPCD)

# Maricopa County
maricopa.wateruse_indoorGPCD <- wateruse_indoorGPCD[wateruse_indoorGPCD$County == "MARICOPA", ]
fit.maricopa.avg.indoor.GPCD <- lm(Average.Indoor.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                                     HomeValue_USD + NumBathrooms + NumResidents + LotSize_Sq.Ft + HasPool,
                                   data = maricopa.wateruse_indoorGPCD, na.action = na.exclude)
summary(fit.maricopa.avg.indoor.GPCD)

# Correlation matrix — Maricopa indoor GPCD predictors
M_indoor_Maricopa <- cor(maricopa.wateruse_indoorGPCD[, c(2, 3, 7:12)], use = "complete.obs")
corrplot(M_indoor_Maricopa, method = 'number')

# Pima County
pima.wateruse_indoorGPCD <- wateruse_indoorGPCD[wateruse_indoorGPCD$County == "PIMA", ]
fit.pima.avg.indoor.GPCD <- lm(Average.Indoor.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                                 HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft + HasPool,
                               data = pima.wateruse_indoorGPCD, na.action = na.exclude)
summary(fit.pima.avg.indoor.GPCD)

# All other counties (excluding Maricopa and Pima)
others.wateruse_indoorGPCD <- wateruse_indoorGPCD[wateruse_indoorGPCD$County != "PIMA" &
                                                    wateruse_indoorGPCD$County != "MARICOPA", ]
fit.others.avg.indoor.GPCD <- lm(Average.Indoor.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                                   HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft + HasPool,
                                 data = others.wateruse_indoorGPCD, na.action = na.exclude)
summary(fit.others.avg.indoor.GPCD)





# ==============================================================================
# SECTION 4: OUTDOOR WATER USE ANALYSIS — ALL ARIZONA HOUSEHOLDS
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.a Filter daily data and pivot to wide format
# ------------------------------------------------------------------------------
# Type-1 filter: retain days where Outdoor GPHD >= 10.
# This removes near-zero readings (no irrigation days) while keeping the full
# distribution for the all-AZ dataset.
#
# Alternative filters tested but not used in primary analysis:
#   Type-2 (histogram method — upper bound based on distribution inspection):
#     wateruse_wide_outdoor_subset <- subset(wateruse_wide, Outdoor.GPHD >= 1.0 &
#                                            Outdoor.GPCD <= 2000)
#   Type-3 (conservative upper cap):
#     wateruse_wide_outdoor_subset <- subset(wateruse_wide, Outdoor.GPHD >= 1.0 &
#                                            Outdoor.GPHD <= 900)

wateruse_wide_outdoor_subset <- subset(wateruse_wide, Outdoor.GPHD >= 10.0)

# Pivot to wide format: one row per date, one column per household (Location_ID)
wateruse_wide_outdoorGPHD_daily <- spread(wateruse_wide_outdoor_subset[, c(1, 2, 10)],
                                          Location_ID, Outdoor.GPHD)
households_names3 <- colnames(wateruse_wide_outdoorGPHD_daily)

# Optional: export wide-format daily outdoor GPHD table
# write.csv(wateruse_wide_outdoorGPHD_daily, "AZ_daily_outdoorGPHD.csv")



# ------------------------------------------------------------------------------
# 4.b Daily regional averages and seasonal distribution plots
# ------------------------------------------------------------------------------
# Compute daily mean and median outdoor GPHD across all households.

outdoor.daily.avg.med <- data.frame(
  Date                 = wateruse_wide_outdoorGPHD_daily$Date,
  Average.Outdoor.GPHD = round(apply(wateruse_wide_outdoorGPHD_daily[2:ncol(wateruse_wide_outdoorGPHD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.Outdoor.GPHD  = round(apply(wateruse_wide_outdoorGPHD_daily[2:ncol(wateruse_wide_outdoorGPHD_daily)], 1, median, na.rm = TRUE), 2)
)
outdoor.daily.avg.med$Month <- as.factor(month(outdoor.daily.avg.med$Date))

# Exploratory regression: daily outdoor GPHD ~ regional mean ET + precipitation.
# Used to confirm that outdoor use responds to weather signals before building
# the full household-level MLR models in section 4.d.
outdoor.daily.avg.med$Average.Precip  <- AZ_Precip_daily_WY22$Mean.Precip
outdoor.daily.avg.med$Average.ET      <- AZ_ET_daily_WY22$Mean.ET
outdoor.daily.avg.med$Median.Precip   <- AZ_Precip_daily_WY22$Median.Precip
outdoor.daily.avg.med$Median.ET       <- AZ_ET_daily_WY22$Median.ET

out.fit <- lm(Average.Outdoor.GPHD ~ Average.ET + Average.Precip,
               data = outdoor.daily.avg.med, na.action = na.exclude)
summary(out.fit)

# Monthly boxplots of daily average and median outdoor GPHD
aa <- ggplot(outdoor.daily.avg.med, aes(x = Month, y = Average.Outdoor.GPHD, group = Month)) +
  geom_boxplot() + theme_classic2()
bb <- ggplot(outdoor.daily.avg.med, aes(x = Month, y = Median.Outdoor.GPHD,  group = Month)) +
  geom_boxplot() + theme_classic2()
grid.arrange(aa, bb, ncol = 2)

# Time series of median outdoor GPHD across the full water year
ggplot(outdoor.daily.avg.med, aes(x = Date, y = Median.Outdoor.GPHD)) +
  geom_line() + theme_classic2()




# ------------------------------------------------------------------------------
# 4.c Build per-household summary table and join household characteristics
# ------------------------------------------------------------------------------
# Collapse wide daily matrix into one row per household (mean and median GPHD).
# The wide matrix has households as columns, so summarize_all + transpose (t())
# is used to produce a tidy household-level data frame.

wateruse_outdoorGPHD <- data.frame(
  Location_ID          = households_names3[-1],
  Average.Outdoor.GPHD = t(round(summarize_all(wateruse_wide_outdoorGPHD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Outdoor.GPHD  = t(round(summarize_all(wateruse_wide_outdoorGPHD_daily[-1], median, na.rm = TRUE), 2))
)

# Join household characteristics
# Note: IrrigationFrequency, IrrigationType, and Landscape_size are included
# here (unlike the indoor table) as they are key outdoor use predictors.
wateruse_outdoorGPHD$City               <- wateruse_wide$City[match(wateruse_outdoorGPHD$Location_ID,              wateruse_wide$Location_ID)]
wateruse_outdoorGPHD$County             <- wateruse_wide$County[match(wateruse_outdoorGPHD$Location_ID,            wateruse_wide$Location_ID)]
wateruse_outdoorGPHD$PostalCode         <- wateruse_wide$PostalCode[match(wateruse_outdoorGPHD$Location_ID,        wateruse_wide$Location_ID)]
wateruse_outdoorGPHD$YearBuilt          <- wateruse_wide$YearBuilt[match(wateruse_outdoorGPHD$Location_ID,         wateruse_wide$Location_ID)]
wateruse_outdoorGPHD$HomeSize_Sq.Ft     <- wateruse_wide$HomeSize_Sq.Ft[match(wateruse_outdoorGPHD$Location_ID,    wateruse_wide$Location_ID)]
wateruse_outdoorGPHD$LotSize_Sq.Ft      <- wateruse_wide$LotSize_Sq.Ft[match(wateruse_outdoorGPHD$Location_ID,     wateruse_wide$Location_ID)]
wateruse_outdoorGPHD$HomeValue_USD      <- wateruse_wide$HomeValue_USD[match(wateruse_outdoorGPHD$Location_ID,     wateruse_wide$Location_ID)]
wateruse_outdoorGPHD$NumResidents       <- wateruse_wide$NumResidents[match(wateruse_outdoorGPHD$Location_ID,      wateruse_wide$Location_ID)]
wateruse_outdoorGPHD$HasPool            <- as.factor(wateruse_wide$HasPool[match(wateruse_outdoorGPHD$Location_ID, wateruse_wide$Location_ID)])
wateruse_outdoorGPHD$IrrigationFrequency <- as.factor(wateruse_wide$IrrigationFrequency[match(wateruse_outdoorGPHD$Location_ID, wateruse_wide$Location_ID)])
wateruse_outdoorGPHD$IrrigationType     <- as.factor(wateruse_wide$IrrigationType[match(wateruse_outdoorGPHD$Location_ID,      wateruse_wide$Location_ID)])

# Landscape size = lot size minus home footprint (proxy for irrigable area)
# Negative values (lot < home) are data errors and are removed.
wateruse_outdoorGPHD$Landscape_size <- as.numeric(wateruse_outdoorGPHD$LotSize_Sq.Ft -
                                                    wateruse_outdoorGPHD$HomeSize_Sq.Ft)
wateruse_outdoorGPHD <- subset(wateruse_outdoorGPHD, Landscape_size >= 0)

# Complete-cases version for models that cannot handle NAs
wateruse_outdoorGPHD_complete <- wateruse_outdoorGPHD[complete.cases(wateruse_outdoorGPHD), ]

# Pool subsets — used in separate MLR models below
wateruse_outdoorGPHD_pool   <- wateruse_outdoorGPHD[wateruse_outdoorGPHD$HasPool == TRUE,  ]
wateruse_outdoorGPHD_nopool <- wateruse_outdoorGPHD[wateruse_outdoorGPHD$HasPool == FALSE, ]



# ------------------------------------------------------------------------------
# 4.d Multiple linear regression — Outdoor GPHD
# ------------------------------------------------------------------------------
# Predictors: YearBuilt, HomeSize, HomeValue, LotSize, HasPool, NumResidents,
#             Landscape_size, IrrigationFrequency, IrrigationType
# Models are run for: (1) all homes, (2) homes with pool, (3) homes without pool.
# Pool status is excluded from the pool/no-pool models as it becomes a constant.
#
# Landscape_size correlation with Median.Outdoor.GPHD is checked first as a
# quick validation that irrigable area is a meaningful predictor.

cor(wateruse_outdoorGPHD$Median.Outdoor.GPHD, wateruse_outdoorGPHD$Landscape_size)

# All homes
fit.outdoor.GPHD <- lm(Average.Outdoor.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                         HomeValue_USD + LotSize_Sq.Ft + HasPool + NumResidents +
                         Landscape_size + IrrigationFrequency + IrrigationType,
                       data = wateruse_outdoorGPHD, na.action = na.exclude)
summary(fit.outdoor.GPHD)

# Homes with pool
fit.outdoor.pool.GPHD <- lm(Average.Outdoor.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                              HomeValue_USD + LotSize_Sq.Ft + NumResidents +
                              IrrigationFrequency + IrrigationType,
                            data = wateruse_outdoorGPHD_pool, na.action = na.omit)
summary(fit.outdoor.pool.GPHD)

# Homes without pool
fit.outdoor.nopool.GPHD <- lm(Average.Outdoor.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                                HomeValue_USD + LotSize_Sq.Ft + NumResidents +
                                IrrigationFrequency + IrrigationType,
                              data = wateruse_outdoorGPHD_nopool, na.action = na.omit)
summary(fit.outdoor.nopool.GPHD)





# ==============================================================================
# SECTION 5: TOTAL WATER USE ANALYSIS — ALL ARIZONA HOUSEHOLDS
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.a Filter daily data and pivot to wide format
# ------------------------------------------------------------------------------
# Type-1 filter: retain days where Total GPHD >= 20 (approximately Indoor >= 10
# + Outdoor >= 10, consistent with thresholds applied in Sections 3 and 4).

wateruse_wide_total_subset <- subset(wateruse_wide, Total.GPHD >= 20.0)

# Pivot to wide format: one row per date, one column per household
wateruse_wide_totalGPHD_daily <- spread(wateruse_wide_total_subset[, c(1, 2, 11)],
                                        Location_ID, Total.GPHD)
households_names4 <- colnames(wateruse_wide_totalGPHD_daily)

wateruse_wide_totalGPCD_daily <- spread(wateruse_wide_total_subset[, c(1, 2, 14)],
                                        Location_ID, Total.GPCD)
households_names5 <- colnames(wateruse_wide_totalGPCD_daily)

# Optional: export wide-format daily total use tables
# write.csv(wateruse_wide_totalGPHD_daily, "AZ_daily_TotalGPHD.csv")
# write.csv(wateruse_wide_totalGPCD_daily, "AZ_daily_TotalGPCD.csv")




# ------------------------------------------------------------------------------
# 5.b Build per-household summary tables and join household characteristics
# ------------------------------------------------------------------------------
# Collapse wide daily matrices into one row per household (mean and median).

wateruse_totalGPHD <- data.frame(
  Location_ID        = households_names4[-1],
  Average.Total.GPHD = t(round(summarize_all(wateruse_wide_totalGPHD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Total.GPHD  = t(round(summarize_all(wateruse_wide_totalGPHD_daily[-1], median, na.rm = TRUE), 2))
)

wateruse_totalGPCD <- data.frame(
  Location_ID        = households_names5[-1],
  Average.Total.GPCD = t(round(summarize_all(wateruse_wide_totalGPCD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Total.GPCD  = t(round(summarize_all(wateruse_wide_totalGPCD_daily[-1], median, na.rm = TRUE), 2))
)

# Join household characteristics to both GPHD and GPCD summary tables
# Total use includes irrigation predictors (IrrigationFrequency, IrrigationType)
# in addition to the building and demographic variables used for indoor use.
for (df_name in c("wateruse_totalGPHD", "wateruse_totalGPCD")) {
  df <- get(df_name)
  df$City                <- wateruse_wide$City[match(df$Location_ID,                wateruse_wide$Location_ID)]
  df$County              <- wateruse_wide$County[match(df$Location_ID,              wateruse_wide$Location_ID)]
  df$PostalCode          <- wateruse_wide$PostalCode[match(df$Location_ID,          wateruse_wide$Location_ID)]
  df$YearBuilt           <- wateruse_wide$YearBuilt[match(df$Location_ID,           wateruse_wide$Location_ID)]
  df$HomeSize_Sq.Ft      <- wateruse_wide$HomeSize_Sq.Ft[match(df$Location_ID,      wateruse_wide$Location_ID)]
  df$LotSize_Sq.Ft       <- wateruse_wide$LotSize_Sq.Ft[match(df$Location_ID,       wateruse_wide$Location_ID)]
  df$HomeValue_USD       <- wateruse_wide$HomeValue_USD[match(df$Location_ID,        wateruse_wide$Location_ID)]
  df$NumBathrooms        <- wateruse_wide$NumBathrooms[match(df$Location_ID,         wateruse_wide$Location_ID)]
  df$NumResidents        <- wateruse_wide$NumResidents[match(df$Location_ID,         wateruse_wide$Location_ID)]
  df$HasPool             <- as.factor(wateruse_wide$HasPool[match(df$Location_ID,    wateruse_wide$Location_ID)])
  df$IrrigationFrequency <- as.factor(wateruse_wide$IrrigationFrequency[match(df$Location_ID, wateruse_wide$Location_ID)])
  df$IrrigationType      <- as.factor(wateruse_wide$IrrigationType[match(df$Location_ID,      wateruse_wide$Location_ID)])
  assign(df_name, df)
}

# Pool subsets for pool-stratified MLR models
wateruse_totalGPHD_pool   <- wateruse_totalGPHD[wateruse_totalGPHD$HasPool == "TRUE",  ]
wateruse_totalGPHD_nopool <- wateruse_totalGPHD[wateruse_totalGPHD$HasPool == "FALSE", ]
wateruse_totalGPCD_pool   <- wateruse_totalGPCD[wateruse_totalGPCD$HasPool == "TRUE",  ]
wateruse_totalGPCD_nopool <- wateruse_totalGPCD[wateruse_totalGPCD$HasPool == "FALSE", ]



# ------------------------------------------------------------------------------
# 5.c Daily regional summaries and seasonal distribution plots
# ------------------------------------------------------------------------------

total.daily.avg.med <- data.frame(
  Date               = wateruse_wide_totalGPHD_daily$Date,
  Average.total.GPHD = round(apply(wateruse_wide_totalGPHD_daily[2:ncol(wateruse_wide_totalGPHD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.total.GPHD  = round(apply(wateruse_wide_totalGPHD_daily[2:ncol(wateruse_wide_totalGPHD_daily)], 1, median, na.rm = TRUE), 2),
  Average.total.GPCD = round(apply(wateruse_wide_totalGPCD_daily[2:ncol(wateruse_wide_totalGPCD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.total.GPCD  = round(apply(wateruse_wide_totalGPCD_daily[2:ncol(wateruse_wide_totalGPCD_daily)], 1, median, na.rm = TRUE), 2)
)
total.daily.avg.med$Month <- as.factor(month(total.daily.avg.med$Date))

# Monthly boxplots of daily average and median total use (GPHD and GPCD)
aa <- ggplot(total.daily.avg.med, aes(x = Month, y = Average.total.GPHD, group = Month)) +
  geom_boxplot() + theme_classic2()
bb <- ggplot(total.daily.avg.med, aes(x = Month, y = Median.total.GPHD,  group = Month)) +
  geom_boxplot() + theme_classic2()
cc <- ggplot(total.daily.avg.med, aes(x = Month, y = Average.total.GPCD, group = Month)) +
  geom_boxplot() + theme_classic2()
dd <- ggplot(total.daily.avg.med, aes(x = Month, y = Median.total.GPCD,  group = Month)) +
  geom_boxplot() + theme_classic2()
grid.arrange(aa, bb, cc, dd, ncol = 2)

# Time series of median total GPCD across the full water year
ggplot(total.daily.avg.med, aes(x = Date, y = Median.total.GPCD)) +
  geom_line() + theme_classic2()



# ------------------------------------------------------------------------------
# 5.d Total use vs. number of residents — summary and area plots
# ------------------------------------------------------------------------------
# Summarize mean total GPHD and GPCD by household size as a sanity check on
# the expected scaling relationship before running full MLR models.

df_numresidents_wateruse <- data.frame(
  summarise(group_by(wateruse_totalGPCD, NumResidents),
            Mean.GPCD   = mean(Average.Total.GPCD),
            Median.GPCD = median(Average.Total.GPCD),
            Max.GPCD    = max(Average.Total.GPCD),
            Min.GPCD    = min(Average.Total.GPCD),
            Count       = length(NumResidents)),
  summarise(group_by(wateruse_totalGPHD, NumResidents),
            Mean.GPHD   = mean(Average.Total.GPHD),
            Median.GPHD = median(Average.Total.GPHD),
            Max.GPHD    = max(Average.Total.GPHD),
            Min.GPHD    = min(Average.Total.GPHD))
)

aa <- ggplot(df_numresidents_wateruse, aes(NumResidents, Mean.GPHD)) +
  geom_area(fill = "orangered3") + theme_classic2() +
  xlab("Number of Residents") + ylab("Household Water Use\nin a Day (Gallons)") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8))

bb <- ggplot(df_numresidents_wateruse, aes(NumResidents, Mean.GPCD)) +
  geom_area(fill = "olivedrab3") + theme_classic2() +
  xlab("Number of Residents") + ylab("Per Person Water Use\nin a Day (Gallons)") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8))

grid.arrange(aa, bb, ncol = 1)



# ------------------------------------------------------------------------------
# 5.e Multiple linear regression — Total GPHD
# ------------------------------------------------------------------------------
# Predictors: YearBuilt, HomeSize, HomeValue, NumResidents, NumBathrooms,
#             LotSize, HasPool, IrrigationType, IrrigationFrequency
# Models run for: (1) all AZ, (2) pool homes, (3) no-pool homes.
# HasPool is excluded from pool/no-pool sub-models (constant within each subset).

# All Arizona — average and median
fit.avg.total.GPHD <- lm(Average.Total.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                           HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                           HasPool + IrrigationType + IrrigationFrequency,
                         data = wateruse_totalGPHD, na.action = na.exclude)
summary(fit.avg.total.GPHD)

fit.med.total.GPHD <- lm(Median.Total.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                           HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                           HasPool + IrrigationType + IrrigationFrequency,
                         data = wateruse_totalGPHD, na.action = na.exclude)
summary(fit.med.total.GPHD)

# Homes with pool
fit.avg.total.GPHD_pool <- lm(Average.Total.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                                HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                                IrrigationType + IrrigationFrequency,
                              data = wateruse_totalGPHD_pool, na.action = na.omit)
summary(fit.avg.total.GPHD_pool)

fit.med.total.GPHD_pool <- lm(Median.Total.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                                HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                                IrrigationType + IrrigationFrequency,
                              data = wateruse_totalGPHD_pool, na.action = na.omit)
summary(fit.med.total.GPHD_pool)

# Homes without pool
fit.avg.total.GPHD_nopool <- lm(Average.Total.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                                  HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                                  IrrigationType + IrrigationFrequency,
                                data = wateruse_totalGPHD_nopool, na.action = na.omit)
summary(fit.avg.total.GPHD_nopool)

fit.med.total.GPHD_nopool <- lm(Median.Total.GPHD ~ YearBuilt + HomeSize_Sq.Ft +
                                  HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                                  IrrigationType + IrrigationFrequency,
                                data = wateruse_totalGPHD_nopool, na.action = na.omit)
summary(fit.med.total.GPHD_nopool)


# ------------------------------------------------------------------------------
# 5.f Multiple linear regression — Total GPCD
# ------------------------------------------------------------------------------
# Same structure as 5.e but using per-capita total use as the response variable.

# All Arizona
fit.avg.total.GPCD <- lm(Average.Total.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                           HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                           HasPool + IrrigationType + IrrigationFrequency,
                         data = wateruse_totalGPCD, na.action = na.exclude)
summary(fit.avg.total.GPCD)

fit.med.total.GPCD <- lm(Median.Total.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                           HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                           HasPool + IrrigationType + IrrigationFrequency,
                         data = wateruse_totalGPCD, na.action = na.exclude)
summary(fit.med.total.GPCD)

# Homes with pool
fit.avg.total.GPCD_pool <- lm(Average.Total.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                                HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                                IrrigationType + IrrigationFrequency,
                              data = wateruse_totalGPCD_pool, na.action = na.omit)
summary(fit.avg.total.GPCD_pool)

fit.med.total.GPCD_pool <- lm(Median.Total.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                                HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                                IrrigationType + IrrigationFrequency,
                              data = wateruse_totalGPCD_pool, na.action = na.omit)
summary(fit.med.total.GPCD_pool)

# Homes without pool
fit.avg.total.GPCD_nopool <- lm(Average.Total.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                                  HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                                  IrrigationType + IrrigationFrequency,
                                data = wateruse_totalGPCD_nopool, na.action = na.omit)
summary(fit.avg.total.GPCD_nopool)

fit.med.total.GPCD_nopool <- lm(Median.Total.GPCD ~ YearBuilt + HomeSize_Sq.Ft +
                                  HomeValue_USD + NumResidents + NumBathrooms + LotSize_Sq.Ft +
                                  IrrigationType + IrrigationFrequency,
                                data = wateruse_totalGPCD_nopool, na.action = na.omit)
summary(fit.med.total.GPCD_nopool)




# ------------------------------------------------------------------------------
# 5.g MLR with monthly weather predictors — Outdoor and Total GPHD
# ------------------------------------------------------------------------------
# Extends the household-level MLR models by adding monthly climate variables
# from each household's nearest AZMET station. Precipitation and ET are summed
# by month; max temperature is averaged by month. Annual totals/means are also
# included as summary predictors.
#
# These are exploratory kitchen-sink models intended to identify which months'
# climate signals are most strongly associated with water use before variable
# selection (see Section 6 — stepwise regression).
#
# Note: a planned extension grouping households by CBSA/CSA metro area
# (Phoenix-Mesa-Scottsdale, Tucson, Nogales) is not yet implemented.
# Relevant county groupings for reference:
#   Phoenix MSA  (CBSA 38060): Maricopa, Pinal
#   Tucson MSA   (CBSA 46060): Pima
#   Nogales MSA  (CBSA 35700): Santa Cruz

# Aggregate daily weather to monthly totals/means per AZMET station
monthly_weather <- AZ_weather_daily_WY22 %>%
  group_by(Month, AZMET_Station) %>%
  summarise(Precip  = sum(DAY_Precip),
            ET      = sum(DAY_Penman),
            TempMax = mean(AirTempMax.DegF.))

monthly_weather$AZMET_FID <- AZ_weather_daily_WY22$AZMET_FID[match(monthly_weather$AZMET_Station,
                                                                   AZ_weather_daily_WY22$AZMET_Station)]

# Pivot monthly weather to wide format: one row per station, one column per month
monthly_precip <- spread(monthly_weather[, 1:3], Month, Precip)
colnames(monthly_precip) <- c("AZMET_Station",
                              "Jan.Precip", "Feb.Precip", "Mar.Precip", "Apr.Precip",
                              "May.Precip", "Jun.Precip", "Jul.Precip", "Aug.Precip",
                              "Sep.Precip", "Oct.Precip", "Nov.Precip", "Dec.Precip")
monthly_precip$Annual.Precip <- round(apply(monthly_precip[2:13], 1, sum,  na.rm = TRUE), 2)

monthly_ET <- spread(monthly_weather[, c(1, 2, 4)], Month, ET)
colnames(monthly_ET) <- c("AZMET_Station",
                          "Jan.ET", "Feb.ET", "Mar.ET", "Apr.ET",
                          "May.ET", "Jun.ET", "Jul.ET", "Aug.ET",
                          "Sep.ET", "Oct.ET", "Nov.ET", "Dec.ET")
monthly_ET$Annual.ET <- round(apply(monthly_ET[2:13], 1, sum,  na.rm = TRUE), 2)

monthly_TempMax <- spread(monthly_weather[, c(1, 2, 5)], Month, TempMax)
colnames(monthly_TempMax) <- c("AZMET_Station",
                               "Jan.TempMax", "Feb.TempMax", "Mar.TempMax", "Apr.TempMax",
                               "May.TempMax", "Jun.TempMax", "Jul.TempMax", "Aug.TempMax",
                               "Sep.TempMax", "Oct.TempMax", "Nov.TempMax", "Dec.TempMax")
monthly_TempMax$Annual.TempMax <- round(apply(monthly_TempMax[2:13], 1, mean, na.rm = TRUE), 2)

# Join monthly weather to outdoor and total household summary tables
wateruse_outdoorGPHD$AZMET_Station <- households_info$AZMET_Station[match(wateruse_outdoorGPHD$Location_ID,
                                                                          households_info$Location_ID)]
wateruse_outdoorGPHD <- wateruse_outdoorGPHD %>%
  left_join(monthly_ET,      by = 'AZMET_Station') %>%
  left_join(monthly_precip,  by = 'AZMET_Station') %>%
  left_join(monthly_TempMax, by = 'AZMET_Station')

wateruse_totalGPHD$AZMET_Station <- households_info$AZMET_Station[match(wateruse_totalGPHD$Location_ID,
                                                                        households_info$Location_ID)]
wateruse_totalGPHD <- wateruse_totalGPHD %>%
  left_join(monthly_ET,      by = 'AZMET_Station') %>%
  left_join(monthly_precip,  by = 'AZMET_Station') %>%
  left_join(monthly_TempMax, by = 'AZMET_Station')

# Kitchen-sink MLR: outdoor GPHD ~ household characteristics + all monthly weather
outdoor.fit <- lm(Average.Outdoor.GPHD ~ PostalCode + YearBuilt + HomeSize_Sq.Ft +
                    HomeValue_USD + LotSize_Sq.Ft + HasPool + NumResidents +
                    Landscape_size + IrrigationFrequency + IrrigationType +
                    Jan.Precip  + Feb.Precip  + Mar.Precip  + Apr.Precip  + May.Precip  + Jun.Precip  +
                    Jul.Precip  + Aug.Precip  + Sep.Precip  + Oct.Precip  + Nov.Precip  + Dec.Precip  + Annual.Precip +
                    Jan.ET      + Feb.ET      + Mar.ET      + Apr.ET      + May.ET      + Jun.ET      +
                    Jul.ET      + Aug.ET      + Sep.ET      + Oct.ET      + Nov.ET      + Dec.ET      + Annual.ET +
                    Jan.TempMax + Feb.TempMax + Mar.TempMax + Apr.TempMax + May.TempMax + Jun.TempMax +
                    Jul.TempMax + Aug.TempMax + Sep.TempMax + Oct.TempMax + Nov.TempMax + Dec.TempMax + Annual.TempMax,
                  data = wateruse_outdoorGPHD, na.action = na.exclude)
summary(outdoor.fit)

# Kitchen-sink MLR: total GPHD ~ household characteristics + monthly precip and ET
# (TempMax excluded from total use model based on prior exploratory results)
total.fit <- lm(Average.Total.GPHD ~ PostalCode + YearBuilt + HomeSize_Sq.Ft +
                  HomeValue_USD + LotSize_Sq.Ft + HasPool +
                  NumResidents + NumBathrooms + IrrigationFrequency + IrrigationType +
                  Jan.Precip  + Feb.Precip  + Mar.Precip  + Apr.Precip  + May.Precip  + Jun.Precip  +
                  Jul.Precip  + Aug.Precip  + Sep.Precip  + Oct.Precip  + Nov.Precip  + Dec.Precip  + Annual.Precip +
                  Jan.ET      + Feb.ET      + Mar.ET      + Apr.ET      + May.ET      + Jun.ET      +
                  Jul.ET      + Aug.ET      + Sep.ET      + Oct.ET      + Nov.ET      + Dec.ET      + Annual.ET,
                data = wateruse_totalGPHD, na.action = na.exclude)
summary(total.fit)





# ==============================================================================
# SECTION 6: VARIABLE IMPORTANCE, PCA, AND REGRESSION TREES — ALL ARIZONA
# ==============================================================================
# Note: this section was originally titled "Factor Analysis" but contains
# Random Forest variable importance and PCA — not classical factor analysis.
# The section header has been corrected accordingly.

# ------------------------------------------------------------------------------
# 6.a Random Forest variable importance
# ------------------------------------------------------------------------------
# Random Forest models are used to rank predictors by their contribution to
# reducing node impurity (IncNodePurity). Results guide predictor selection for
# the stepwise regression models in Section 10.
#
# Three response variables are modeled:
#   - Average Indoor GPHD  : household building characteristics only (cols 6:13)
#   - Average Outdoor GPHD : building + irrigation + monthly weather (cols 6:15, 17:42)
#   - Average Total GPHD   : same predictor set as outdoor
#
# The error convergence plot (plot.randomForest) is used to confirm ntree is
# sufficient for stable importance estimates.
# Optional: uncomment png() / dev.off() lines to save plots to file.

# Helper function to build and plot a ranked RF importance bar chart
plot_rf_importance <- function(rf_model, title = "") {
  rf_imp <- rownames_to_column(data.frame(rf_model$importance), "variables")
  rf_imp <- rf_imp[order(rf_imp$IncNodePurity, decreasing = TRUE), ]
  ggplot(data = rf_imp, aes(x = reorder(variables, IncNodePurity), y = IncNodePurity)) +
    geom_bar(stat = "identity", width = 0.1, color = "blue") +
    coord_flip() + theme_bw() + xlab("") + ggtitle(title) +
    theme(axis.text.x  = element_text(color = "black", size = 7,    angle = -90),
          axis.text.y  = element_text(color = "black", size = 7.25, angle = 0),
          axis.title.x = element_text(color = "black", size = 7.2,  angle = 0),
          axis.title.y = element_text(color = "black", size = 6,    angle = 90))
}

# Indoor GPHD — building characteristics only
# Columns 6:13: YearBuilt, HomeSize, LotSize, HomeValue, NumBathrooms, NumResidents, HasPool
rforest.indoor <- randomForest(Average.Indoor.GPHD ~ .,
                               data = wateruse_indoorGPHD[, c(2, 6:13)],
                               ntree = 200, na.action = na.exclude)
plot(rforest.indoor, main = "Indoor RF — error convergence")
rf_in <- rownames_to_column(data.frame(rforest.indoor$importance), "variables")
rf_in <- rf_in[order(rf_in$IncNodePurity, decreasing = TRUE), ]
plot_rf_importance(rforest.indoor, title = "Indoor GPHD — Variable Importance")

# Outdoor GPHD — building + irrigation + monthly weather
# Columns 6:15: building/irrigation chars; 17:42: monthly precip, ET, TempMax
rforest.outdoor <- randomForest(Average.Outdoor.GPHD ~ .,
                                data = wateruse_outdoorGPHD[, c(2, 6:15, 17:42)],
                                ntree = 200, na.action = na.exclude)
plot(rforest.outdoor, main = "Outdoor RF — error convergence")
rf_out <- rownames_to_column(data.frame(rforest.outdoor$importance), "variables")
rf_out <- rf_out[order(rf_out$IncNodePurity, decreasing = TRUE), ]
rf_out <- rf_out[1:25, ]  # display top 25 predictors only
plot_rf_importance(rforest.outdoor, title = "Outdoor GPHD — Variable Importance (Top 25)")

# Total GPHD — same predictor set as outdoor
rforest.total <- randomForest(Average.Total.GPHD ~ .,
                              data = wateruse_totalGPHD[, c(2, 6:15, 17:42)],
                              ntree = 200, na.action = na.exclude)
plot(rforest.total, main = "Total RF — error convergence")
rf_tot <- rownames_to_column(data.frame(rforest.total$importance), "variables")
rf_tot <- rf_tot[order(rf_tot$IncNodePurity, decreasing = TRUE), ]
rf_tot <- rf_tot[1:25, ]  # display top 25 predictors only
plot_rf_importance(rforest.total, title = "Total GPHD — Variable Importance (Top 25)")



# ------------------------------------------------------------------------------
# 6.b Principal Component Analysis (PCA)
# ------------------------------------------------------------------------------
# PCA is run on the household-level predictor matrices (excluding the response
# variable) to visualize multicollinearity structure among predictors.
# Factor variables are converted to numeric prior to PCA.
# Rows with any NA are removed (complete cases only); median imputation was
# considered but not used in the primary analysis.
#
# Note: Outdoor GPHD PCA is not implemented here — only Indoor and Total.
# pca_loadings captures the first 4 component loadings for inspection; it is
# overwritten by the Total PCA block and not exported downstream.

# Indoor GPHD PCA — predictors: cols 7:13 (YearBuilt through HasPool)
water_indoor <- wateruse_indoorGPHD
rownames(water_indoor) <- water_indoor[, 1]
water_indoor <- water_indoor[, c(7:13)]
water_indoor$HasPool <- as.numeric(water_indoor$HasPool)
water_indoor <- water_indoor[complete.cases(water_indoor), ]

pca_indoor <- princomp(water_indoor)
print(pca_indoor)
plot(pca_indoor, type = "l")   # Scree plot — inspect elbow for component retention
summary(pca_indoor)
pca_loadings <- data.frame(pca_indoor$loadings[, 1:4])

fviz_pca_var(pca_indoor,
             col.var      = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title        = "Indoor GPHD PCA",
             arrowsize    = 0.001,
             repel        = TRUE) +
  theme_bw() + labs(color = 'Contribution')


# Total GPHD PCA — predictors: cols 7:15 (building/irrigation) + 17:42 (monthly weather)
# IrrigationType and IrrigationFrequency converted to numeric for PCA compatibility
water_total <- wateruse_totalGPHD
rownames(water_total) <- water_total[, 1]
water_total <- water_total[, c(7:15, 17:42)]
water_total$HasPool             <- as.numeric(water_total$HasPool)
water_total$IrrigationType      <- as.numeric(water_total$IrrigationType)
water_total$IrrigationFrequency <- as.numeric(water_total$IrrigationFrequency)
water_total <- water_total[complete.cases(water_total), ]

pca_total <- princomp(water_total)
print(pca_total)
plot(pca_total, type = "l")    # Scree plot
summary(pca_total)
pca_loadings <- data.frame(pca_total$loadings[, 1:4])  # overwrites indoor loadings

fviz_pca_var(pca_total,
             col.var       = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title         = "Total GPHD PCA",
             arrowsize     = 0.001,
             repel         = TRUE) +
  theme_bw() + labs(color = 'Contribution')


# ------------------------------------------------------------------------------
# 6.c Regression tree — Total GPHD (all predictors)
# ------------------------------------------------------------------------------
# A full regression tree is fit to explore non-linear predictor relationships
# and interaction structure before the stepwise and RF analyses in Section 10.
# Predictors mirror the Total GPHD RF model (cols 7:15, 17:42).

tree.water <- tree(Average.Total.GPHD ~ .,
                   data = wateruse_totalGPHD[, c(2, 7:15, 17:42)])
plot(tree.water, col = "black")
text(tree.water, pretty = 1, cex = 1, col = "black")
summary(tree.water)


# ------------------------------------------------------------------------------
# 6.d Regression tree — train/test validation (80/20 split)
# ------------------------------------------------------------------------------
# The dataset is randomly split 80/20 into training and test sets.
# A tree is fit on the training set and predictions are evaluated on the test
# set using Pearson correlation and a linear model (predicted ~ observed).
# An empirical CDF plot overlays observed vs. tree-predicted Total GPHD
# to visualize the distributional fit.

set.seed(42)  # set seed for reproducibility of the random split
water_total <- wateruse_totalGPHD[, c(2, 7:15, 17:42)]
ind         <- sample(2, nrow(water_total), replace = TRUE, prob = c(0.80, 0.20))
train_water <- water_total[ind == 1, ]
test_water  <- water_total[ind == 2, ]

# Fit tree on training data
tree_train <- tree(Average.Total.GPHD ~ ., data = train_water)
plot(tree_train, col = "black")
text(tree_train, pretty = 1, cex = 1, col = "black")
summary(tree_train)

# Generate predictions on test data and merge with observed values
testing_water <- data.frame(predict(tree_train, test_water))
df2 <- merge(testing_water, test_water[, 1:2], by = 0, all = TRUE)
df2 <- df2[, -4]  # remove duplicate row-name column introduced by merge
colnames(df2) <- c("S.no", "tree.classified.TotalGPHD", "observed.TotalGPHD")

# Evaluate prediction performance
cor.model2 <- cor(df2[, 2], df2[, 3], use = "pairwise.complete.obs")
cor.model2

lm.model2 <- lm(df2[, 2] ~ df2[, 3])
summary(lm.model2)

# Empirical CDF plot: observed (black, solid) vs. tree-predicted (red, dashed)
# Optional: uncomment png() / dev.off() to save plot to file
# png("eCDF_plot.png", units="in", width=5, height=5, res=300)
plot(ecdf(df2$observed.TotalGPHD), verticals = TRUE, do.points = FALSE,
     col = "black", lty = 1, lwd = 2,
     xlab = "Total GPHD", ylab = "F(Total GPHD)", main = "")
plot(ecdf(df2$tree.classified.TotalGPHD), verticals = TRUE, lty = 2, lwd = 2,
     do.points = FALSE, add = TRUE, col = "red")
legend("bottomright", legend = c("Observed", "Tree classified"),
       col = c("black", "red"), lty = 1:2, cex = 0.75, box.lty = 0)
# dev.off()






# ==============================================================================
# SECTION 7: FLUME 2 DEVICE SUBSET — DATA INGESTION, WRANGLING & SUMMARIES
# ==============================================================================
# This section restricts analysis to households with a Flume 2 smart meter device.
# Flume 2 devices provide fixture-level disaggregation data (toilet, shower,
# faucet, etc.) in addition to the whole-home indoor/outdoor GPHD from Section 2.
#
# Pipeline: flume2_AZ_households_info is assembled in 7.a–7.b.
# Then Fixture efficiency labels are added via cluster analysis in Section "7.b.ii", and 
# the classified object is factorized in 7.c before use in all downstream Flume 2 analyses (Sections 8–11).

# ------------------------------------------------------------------------------
# 7.a Load and reshape fixture disaggregation data
# ------------------------------------------------------------------------------
# az disag.xlsx contains per-fixture water use metrics for each Flume 2 household:
# average volume (gal), duration (min), and flow rate (GPM) per fixture type.
# Location_ID is stored only on the first row of each household block in the raw
# file; the loop below forward-fills missing IDs down the fixture rows.

flume2_disag <- read_excel("az disag.xlsx")
colnames(flume2_disag)[1] <- "Location_ID"

# Forward-fill Location_ID: copy value from previous row where missing
for (i in 1:nrow(flume2_disag)) {
  if (is.na(flume2_disag$Location_ID[i])) {
    flume2_disag$Location_ID[i] <- flume2_disag$Location_ID[i - 1]
  }
}

# Pivot each metric to wide format: one row per household, one column per fixture
disag_Avg.gal      <- spread(flume2_disag[, c(1, 2, 4)], Fixture, `Avg. Volume (gal)`)
disag_Avg.min      <- spread(flume2_disag[, c(1, 2, 3)], Fixture, `Avg. Duration (min)`)
disag_Avg.flow.GPM <- spread(flume2_disag[, c(1, 2, 5)], Fixture, `Avg. Flow Rate (GPM)`)

colnames(disag_Avg.gal)[-1]      <- paste(colnames(disag_Avg.gal)[-1],      "Avg.gal", sep = "_")
colnames(disag_Avg.min)[-1]      <- paste(colnames(disag_Avg.min)[-1],      "Avg.min", sep = "_")
colnames(disag_Avg.flow.GPM)[-1] <- paste(colnames(disag_Avg.flow.GPM)[-1], "Avg.GPM", sep = "_")


# ------------------------------------------------------------------------------
# 7.b.i Reconcile NumResidents and build Flume 2 household info table
# ------------------------------------------------------------------------------
# NumResidents differs between AZ_Households.xlsx (flume1_2) and az disag.xlsx
# (flume2). The disaggregation file value is used as the authoritative source
# for all Flume 2 analyses, as it reflects device-reported occupancy.

flume2_numresidents   <- data.frame(flume2_disag[!duplicated(flume2_disag[c(1, 9)]), c(1, 9)])
colnames(flume2_numresidents) <- c("Location_ID", "NumResf2")

flume1_2_numresidents <- households_info[, c(1, 11)]
colnames(flume1_2_numresidents) <- c("Location_ID", "NumResf1_2")

# Diagnostic comparison: difference between the two NumResidents sources
flume2_AZ_numresidents       <- inner_join(flume1_2_numresidents, flume2_numresidents, by = "Location_ID")
flume2_AZ_numresidents$diff  <- flume2_AZ_numresidents$NumResf2 - flume2_AZ_numresidents$NumResf1_2

# Build Flume 2 household info: intersect Flume 2 devices with known households,
# then join building characteristics, disaggregation metrics, and monthly weather
flume2_devices <- unique(flume2_disag$Location_ID)

flume2_AZ_households_info <- data.frame(
  Location_ID = intersect(flume2_devices, households_info$Location_ID)
)
flume2_AZ_households_info <- merge(flume2_AZ_households_info, households_info, by = "Location_ID")

# Override NumResidents with Flume 2 disaggregation file value
flume2_AZ_households_info$NumResidents <- flume2_numresidents$NumResf2[
  match(flume2_AZ_households_info$Location_ID, flume2_numresidents$Location_ID)
]

# Join fixture disaggregation metrics and monthly weather
flume2_AZ_households_info <- flume2_AZ_households_info %>%
  left_join(disag_Avg.gal,      by = "Location_ID") %>%
  left_join(disag_Avg.min,      by = "Location_ID") %>%
  left_join(disag_Avg.flow.GPM, by = "Location_ID") %>%
  left_join(monthly_ET,         by = "AZMET_Station") %>%
  left_join(monthly_precip,     by = "AZMET_Station") %>%
  left_join(monthly_TempMax,    by = "AZMET_Station") %>%
  mutate_at(c('HasPool', 'IrrigationFrequency', 'IrrigationType'), as.factor)


# ==============================================================================
# SECTION 7.b.ii: FIXTURE EFFICIENCY CLASSIFICATION VIA CLUSTER ANALYSIS
# ==============================================================================
# This section classifies each Flume 2 household's fixture efficiency using
# unsupervised clustering on per-fixture water use metrics from az disag.xlsx.
#
#
# Fixtures classified:
#   Toilet          — avg. gallons per flush     (Low/High efficiency)
#   Showerhead      — avg. GPM per event         (Low/High efficiency)
#   Clothes washer  — avg. gallons per cycle      (Low/High efficiency)
#   Dishwasher      — avg. gallons per cycle      (Low/High efficiency)
#   Irrigation      — avg. gallons per event      (Low/High efficiency)
#   Irrigation      — avg. GPM per event          (Low/High efficiency)
#   Faucet          — avg. GPM per event          (Low/High efficiency)
#   Leak            — avg. gallons per event      (Low/High leak level)
#   Home value      — USD                         (Low/High)
#   Landscape size  — sq.ft (LotSize - HomeSize)  (Small/Small-med/Med-large/Large)
#
# Clustering approach:
#   Two methods are compared for each fixture:
#     - k-means   : fast, centroid-based; cluster assignment can change between runs
#     - PAM       : Partitioning Around Medoids; robust to outliers, deterministic
#   The optimal number of clusters is selected using the silhouette method (pam).
#   PAM results are used as the authoritative efficiency labels in all downstream
#   analyses (Section 7.c onward) due to greater stability and outlier robustness.
#
#   IMPORTANT — k-means label instability: k-means cluster numbering (1, 2) is
#   arbitrary and may swap between runs. Always inspect model$centers after
#   fitting to confirm which cluster number corresponds to Low vs. High efficiency
#   before applying the label mapping loops below.
#
# Additional packages required (not in main script library block):
library(cluster)    # pam(): Partitioning Around Medoids clustering


# 7.b.ii.a Exploratory plots — fixture water use distributions
# ------------------------------------------------------------------------------
# Density plots and histograms provide a visual check of distribution shape
# before clustering. Right-skewed distributions are expected for most fixtures.

par(mfrow = c(3, 2))
plot(density(na.omit(flume2_AZ_households_info$SHOWER_Avg.GPM)),
     main = "Shower — avg. GPM per event",         xlab = "Gallons per minute")
plot(density(na.omit(flume2_AZ_households_info$TOILET_Avg.gal)),
     main = "Toilet — avg. gallons per flush",     xlab = "Gallons")
plot(density(na.omit(flume2_AZ_households_info$DISH_WASHER_Avg.gal)),
     main = "Dishwasher — avg. gallons per cycle", xlab = "Gallons")
plot(density(na.omit(flume2_AZ_households_info$CLOTHES_WASHER_Avg.gal)),
     main = "Clothes washer — avg. gallons per cycle", xlab = "Gallons")
plot(density(na.omit(flume2_AZ_households_info$IRRIGATION_Avg.gal)),
     main = "Irrigation — avg. gallons per event", xlab = "Gallons")
par(mfrow = c(1, 1))

par(mfrow = c(3, 2))
hist(na.omit(flume2_AZ_households_info$SHOWER_Avg.GPM),
     main = "Shower — avg. GPM per event",         xlab = "Gallons per minute")
hist(na.omit(flume2_AZ_households_info$TOILET_Avg.gal),
     main = "Toilet — avg. gallons per flush",     xlab = "Gallons")
hist(na.omit(flume2_AZ_households_info$DISH_WASHER_Avg.gal),
     main = "Dishwasher — avg. gallons per cycle", xlab = "Gallons")
hist(na.omit(flume2_AZ_households_info$CLOTHES_WASHER_Avg.gal),
     main = "Clothes washer — avg. gallons per cycle", xlab = "Gallons")
hist(na.omit(flume2_AZ_households_info$IRRIGATION_Avg.gal),
     main = "Irrigation — avg. gallons per event", xlab = "Gallons")
par(mfrow = c(1, 1))


# ------------------------------------------------------------------------------
# 7.b.ii.b Silhouette analysis — optimal number of clusters per fixture
# ------------------------------------------------------------------------------
# The silhouette method is applied to PAM clustering to determine whether k=2
# is consistently the optimal number of clusters across all fixtures.
# Results are plotted individually and overlaid in a combined summary chart.

# Individual silhouette plots (k = 1:10)
a <- fviz_nbclust(scale(na.omit(flume2_AZ_households_info$TOILET_Avg.gal)),
                  pam, method = "silhouette") + theme_minimal() + ggtitle("Toilet [avg. gallons per flush]")
b <- fviz_nbclust(scale(na.omit(flume2_AZ_households_info$SHOWER_Avg.GPM)),
                  pam, method = "silhouette") + theme_minimal() + ggtitle("Showerhead [avg. gallons per minute]")
c <- fviz_nbclust(scale(na.omit(flume2_AZ_households_info$CLOTHES_WASHER_Avg.gal)),
                  pam, method = "silhouette") + theme_minimal() + ggtitle("Clothes washer [avg. gallons per load]")
d <- fviz_nbclust(scale(na.omit(flume2_AZ_households_info$DISH_WASHER_Avg.gal)),
                  pam, method = "silhouette") + theme_minimal() + ggtitle("Dishwasher [avg. gallons per load]")
e <- fviz_nbclust(scale(na.omit(flume2_AZ_households_info$FAUCET_Avg.GPM)),
                  pam, method = "silhouette") + theme_minimal() + ggtitle("Faucet [avg. gallons per minute]")
grid.arrange(a, b, c, d, e, ncol = 3)

# Combined silhouette score overlay — all fixtures on one chart
# A vertical dashed line at k=2 confirms it as the optimal cluster count
toilet_sil     <- data.frame(fviz_nbclust(scale(na.omit(flume2_AZ_households_info$TOILET_Avg.gal)),         pam, method = "silhouette")$data)
shower_sil     <- data.frame(fviz_nbclust(scale(na.omit(flume2_AZ_households_info$SHOWER_Avg.GPM)),         pam, method = "silhouette")$data)
washer_sil     <- data.frame(fviz_nbclust(scale(na.omit(flume2_AZ_households_info$CLOTHES_WASHER_Avg.gal)), pam, method = "silhouette")$data)
dishwasher_sil <- data.frame(fviz_nbclust(scale(na.omit(flume2_AZ_households_info$DISH_WASHER_Avg.gal)),    pam, method = "silhouette")$data)
faucet_sil     <- data.frame(fviz_nbclust(scale(na.omit(flume2_AZ_households_info$FAUCET_Avg.GPM)),         pam, method = "silhouette")$data)

ggplot() + theme_classic() +
  geom_line(data = toilet_sil,     aes(x = as.numeric(clusters), y = y, color = "Toilet (gallons/flush)"),          linewidth = 1) +
  geom_line(data = shower_sil,     aes(x = as.numeric(clusters), y = y, color = "Showerhead (gallons/minute)"),     linewidth = 1) +
  geom_line(data = washer_sil,     aes(x = as.numeric(clusters), y = y, color = "Clothes washer (gallons/load)"),   linewidth = 1) +
  geom_line(data = dishwasher_sil, aes(x = as.numeric(clusters), y = y, color = "Dishwasher (gallons/load)"),       linewidth = 1) +
  geom_line(data = faucet_sil,     aes(x = as.numeric(clusters), y = y, color = "Faucet (gallons/minute)"),         linewidth = 1) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(x = 2.8, y = 0.2, label = "Optimal #\nof clusters"), vjust = 1.5, color = "red", size = 4.5) +
  scale_color_manual(
    name   = "Appliance Flow Characteristics",
    values = c("blue", "lightblue", "green", "orange", "purple"),
    labels = c("Toilet (gallons/flush)", "Showerhead (gallons/minute)",
               "Clothes washer (gallons/load)", "Dishwasher (gallons/load)",
               "Faucet (gallons/minute)")
  ) +
  labs(title = "Combined Silhouette Scores — All Fixtures",
       x = "Number of Clusters", y = "Silhouette Score") +
  ylim(0, 0.8) + scale_x_continuous(breaks = 1:10) +
  theme(
    legend.position      = c(0.6, 0.25),
    legend.justification = "left",
    legend.title         = element_text(size = 10, face = "bold"),
    legend.text          = element_text(size = 10),
    axis.title           = element_text(size = 12, color = "black", face = "bold"),
    axis.text            = element_text(size = 11, color = "black")
  )

# Optional: save combined silhouette plot
# ggsave("Appliance_Silhouette_scores.tiff", width = 6, height = 4, units = "in", dpi = 600)
# ggsave("Appliance_Silhouette_scores.png",  width = 6, height = 4, units = "in", dpi = 600)


# ------------------------------------------------------------------------------
# 7.b.ii.c Helper function — run k-means and PAM, label clusters, validate
# ------------------------------------------------------------------------------
# Each fixture follows the same pattern: fit k-means (k=2) and PAM (k=2),
# map numeric cluster IDs to "Low"/"High" efficiency labels, reshape to long
# format for side-by-side comparison, and validate separation with Kruskal-Wallis.
#
# A reusable helper handles the label-mapping loop, replacing the repetitive
# for-loop pattern in the original script.

label_clusters <- function(cluster_ids, low_id, high_id) {
  # Maps numeric cluster IDs to "Low"/"High" factor labels.
  # low_id  : the cluster number (1 or 2) corresponding to Low efficiency
  # high_id : the cluster number (1 or 2) corresponding to High efficiency
  labels <- ifelse(cluster_ids == low_id, "Low",
                   ifelse(cluster_ids == high_id, "High", NA))
  factor(labels, levels = c("Low", "High"))
}


# ------------------------------------------------------------------------------
# 7.b.ii.d Cluster each fixture — k-means and PAM (k=2)
# ------------------------------------------------------------------------------
# For each fixture: fit both models, inspect centers/medoids to confirm
# Low/High mapping, apply labels, plot, and test cluster separation.
# PAM labels are the authoritative efficiency scores used downstream.

### TOILET — avg. gallons per flush ###
# Low efficiency = higher water use per flush (older, less efficient toilets)
# Inspect model$centers / model2$medoids to confirm cluster-to-label mapping

toilet_vals <- flume2_AZ_households_info$TOILET_Avg.gal
model_toilet_kmeans <- kmeans(na.omit(toilet_vals), 2); model_toilet_kmeans$centers
model_toilet_pam    <- pam(na.omit(toilet_vals),    2); model_toilet_pam$medoids

cluster_toilet <- data.frame(na.omit(flume2_AZ_households_info[, c(1, 25)]))
cluster_toilet$kmeans <- label_clusters(model_toilet_kmeans$cluster, low_id = 1, high_id = 2)
cluster_toilet$pams   <- label_clusters(model_toilet_pam$clustering, low_id = 2, high_id = 1)

cluster_toilet_long <- gather(cluster_toilet, ClusterMethod, Efficiency, kmeans:pams, factor_key = TRUE)
cluster_toilet_long$Efficiency <- factor(cluster_toilet_long$Efficiency, levels = c("Low", "High"))

ggplot(cluster_toilet_long, aes(x = Efficiency, y = TOILET_Avg.gal, fill = ClusterMethod)) +
  geom_boxplot() + theme_classic2() +
  ylab("Toilet Flush Volume (gallons)") + xlab("Toilet Efficiency") +
  theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1)) +
  scale_y_continuous(minor_breaks = seq(0, 5, 0.5))

kruskal.test(TOILET_Avg.gal ~ kmeans, data = cluster_toilet)
kruskal.test(TOILET_Avg.gal ~ pams,   data = cluster_toilet)


### SHOWERHEAD — avg. GPM per event ###
# Low efficiency = higher flow rate (older showerheads)

shower_vals <- flume2_AZ_households_info$SHOWER_Avg.GPM
model_shower_kmeans <- kmeans(na.omit(shower_vals), 2); model_shower_kmeans$centers
model_shower_pam    <- pam(na.omit(shower_vals),    2); model_shower_pam$medoids

cluster_shower <- data.frame(na.omit(flume2_AZ_households_info[, c(1, 41)]))
cluster_shower$kmeans <- label_clusters(model_shower_kmeans$cluster, low_id = 1, high_id = 2)
cluster_shower$pams   <- label_clusters(model_shower_pam$clustering, low_id = 1, high_id = 2)

cluster_shower_long <- gather(cluster_shower, ClusterMethod, Efficiency, kmeans:pams, factor_key = TRUE)
cluster_shower_long$Efficiency <- factor(cluster_shower_long$Efficiency, levels = c("Low", "High"))

ggplot(cluster_shower_long, aes(x = Efficiency, y = SHOWER_Avg.GPM, fill = ClusterMethod)) +
  geom_boxplot() + theme_classic2() +
  ylab("Shower Flow Rate (GPM)") + xlab("Showerhead Efficiency") +
  theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1)) +
  scale_y_continuous(minor_breaks = seq(0, 7, 1))

kruskal.test(SHOWER_Avg.GPM ~ kmeans, data = cluster_shower)
kruskal.test(SHOWER_Avg.GPM ~ pams,   data = cluster_shower)


### CLOTHES WASHER — avg. gallons per cycle ###
# Low efficiency = higher water use per load

washer_vals <- flume2_AZ_households_info$CLOTHES_WASHER_Avg.gal
model_washer_kmeans <- kmeans(na.omit(washer_vals), 2); model_washer_kmeans$centers
model_washer_pam    <- pam(na.omit(washer_vals),    2); model_washer_pam$medoids

cluster_washer <- data.frame(na.omit(flume2_AZ_households_info[, c(1, 17)]))
cluster_washer$kmeans <- label_clusters(model_washer_kmeans$cluster, low_id = 2, high_id = 1)
cluster_washer$pams   <- label_clusters(model_washer_pam$clustering, low_id = 1, high_id = 2)

cluster_washer_long <- gather(cluster_washer, ClusterMethod, Efficiency, kmeans:pams, factor_key = TRUE)
cluster_washer_long$Efficiency <- factor(cluster_washer_long$Efficiency, levels = c("Low", "High"))

ggplot(cluster_washer_long, aes(x = Efficiency, y = CLOTHES_WASHER_Avg.gal, fill = ClusterMethod)) +
  geom_boxplot() + theme_classic2() +
  ylab("Clothes Washer Water Use per Cycle (gallons)") + xlab("Clothes Washer Efficiency") +
  theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))

kruskal.test(CLOTHES_WASHER_Avg.gal ~ kmeans, data = cluster_washer)
kruskal.test(CLOTHES_WASHER_Avg.gal ~ pams,   data = cluster_washer)


### DISHWASHER — avg. gallons per cycle ###
# Low efficiency = higher water use per cycle

dishwasher_vals <- flume2_AZ_households_info$DISH_WASHER_Avg.gal
model_dishwasher_kmeans <- kmeans(na.omit(dishwasher_vals), 2); model_dishwasher_kmeans$centers
model_dishwasher_pam    <- pam(na.omit(dishwasher_vals),    2); model_dishwasher_pam$medoids

cluster_dishwasher <- data.frame(na.omit(flume2_AZ_households_info[, c(1, 18)]))
cluster_dishwasher$kmeans <- label_clusters(model_dishwasher_kmeans$cluster, low_id = 2, high_id = 1)
cluster_dishwasher$pams   <- label_clusters(model_dishwasher_pam$clustering, low_id = 2, high_id = 1)

cluster_dishwasher_long <- gather(cluster_dishwasher, ClusterMethod, Efficiency, kmeans:pams, factor_key = TRUE)
cluster_dishwasher_long$Efficiency <- factor(cluster_dishwasher_long$Efficiency, levels = c("Low", "High"))

ggplot(cluster_dishwasher_long, aes(x = Efficiency, y = DISH_WASHER_Avg.gal, fill = ClusterMethod)) +
  geom_boxplot() + theme_classic2() +
  ylab("Dishwasher Water Use per Cycle (gallons)") + xlab("Dishwasher Efficiency") +
  theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))

kruskal.test(DISH_WASHER_Avg.gal ~ kmeans, data = cluster_dishwasher)
kruskal.test(DISH_WASHER_Avg.gal ~ pams,   data = cluster_dishwasher)


### IRRIGATION — avg. gallons per event ###

irrigation_gal_vals <- flume2_AZ_households_info$IRRIGATION_Avg.gal
model_irgal_kmeans <- kmeans(na.omit(irrigation_gal_vals), 2); model_irgal_kmeans$centers
model_irgal_pam    <- pam(na.omit(irrigation_gal_vals),    2); model_irgal_pam$medoids

cluster_irrigation_gal <- data.frame(na.omit(flume2_AZ_households_info[, c(1, 21)]))
cluster_irrigation_gal$kmeans <- label_clusters(model_irgal_kmeans$cluster, low_id = 1, high_id = 2)
cluster_irrigation_gal$pams   <- label_clusters(model_irgal_pam$clustering, low_id = 1, high_id = 2)

cluster_irrigation_gal_long <- gather(cluster_irrigation_gal, ClusterMethod, Efficiency, kmeans:pams, factor_key = TRUE)
cluster_irrigation_gal_long$Efficiency <- factor(cluster_irrigation_gal_long$Efficiency, levels = c("Low", "High"))

ggplot(cluster_irrigation_gal_long, aes(x = Efficiency, y = IRRIGATION_Avg.gal, fill = ClusterMethod)) +
  geom_boxplot() + theme_classic2() +
  ylab("Irrigation Avg. Gallons per Event") + xlab("Irrigation Efficiency") +
  theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))

kruskal.test(IRRIGATION_Avg.gal ~ kmeans, data = cluster_irrigation_gal)
kruskal.test(IRRIGATION_Avg.gal ~ pams,   data = cluster_irrigation_gal)


### IRRIGATION — avg. GPM per event ###

irrigation_gpm_vals <- flume2_AZ_households_info$IRRIGATION_Avg.GPM
model_irgpm_kmeans <- kmeans(na.omit(irrigation_gpm_vals), 2); model_irgpm_kmeans$centers
model_irgpm_pam    <- pam(na.omit(irrigation_gpm_vals),    2); model_irgpm_pam$medoids

cluster_irrigation_gpm <- data.frame(na.omit(flume2_AZ_households_info[, c(1, 39)]))
cluster_irrigation_gpm$kmeans <- label_clusters(model_irgpm_kmeans$cluster, low_id = 1, high_id = 2)
cluster_irrigation_gpm$pams   <- label_clusters(model_irgpm_pam$clustering, low_id = 2, high_id = 1)

cluster_irrigation_gpm_long <- gather(cluster_irrigation_gpm, ClusterMethod, Efficiency, kmeans:pams, factor_key = TRUE)
cluster_irrigation_gpm_long$Efficiency <- factor(cluster_irrigation_gpm_long$Efficiency, levels = c("Low", "High"))

ggplot(cluster_irrigation_gpm_long, aes(x = Efficiency, y = IRRIGATION_Avg.GPM, fill = ClusterMethod)) +
  geom_boxplot() + theme_classic2() +
  ylab("Irrigation Avg. GPM") + xlab("Irrigation Efficiency") +
  theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))

kruskal.test(IRRIGATION_Avg.GPM ~ kmeans, data = cluster_irrigation_gpm)
kruskal.test(IRRIGATION_Avg.GPM ~ pams,   data = cluster_irrigation_gpm)


### FAUCET — avg. GPM per event ###
# Low efficiency = higher flow rate

faucet_vals <- flume2_AZ_households_info$FAUCET_Avg.GPM
model_faucet_kmeans <- kmeans(na.omit(faucet_vals), 2); model_faucet_kmeans$centers
model_faucet_pam    <- pam(na.omit(faucet_vals),    2); model_faucet_pam$medoids

cluster_faucet <- data.frame(na.omit(flume2_AZ_households_info[, c(1, 37)]))
cluster_faucet$kmeans <- label_clusters(model_faucet_kmeans$cluster, low_id = 1, high_id = 2)
cluster_faucet$pams   <- label_clusters(model_faucet_pam$clustering, low_id = 2, high_id = 1)

cluster_faucet_long <- gather(cluster_faucet, ClusterMethod, Efficiency, kmeans:pams, factor_key = TRUE)
cluster_faucet_long$Efficiency <- factor(cluster_faucet_long$Efficiency, levels = c("Low", "High"))

ggplot(cluster_faucet_long, aes(x = Efficiency, y = FAUCET_Avg.GPM, fill = ClusterMethod)) +
  geom_boxplot() + theme_classic2() +
  ylab("Faucet Avg. GPM") + xlab("Faucet Efficiency") +
  theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))

kruskal.test(FAUCET_Avg.GPM ~ kmeans, data = cluster_faucet)
kruskal.test(FAUCET_Avg.GPM ~ pams,   data = cluster_faucet)


### LEAK — avg. gallons per event ###
# Low = lower leak volume; High = higher leak volume (not efficiency per se)

leak_vals <- flume2_AZ_households_info$LEAK_Avg.gal
boxplot(leak_vals, ylim = c(0, 70), main = "Boxplot of Leak avg. gallons per event")

model_leak_kmeans <- kmeans(na.omit(leak_vals), 2); model_leak_kmeans$centers
model_leak_pam    <- pam(na.omit(leak_vals),    2); model_leak_pam$medoids

cluster_leak <- data.frame(na.omit(flume2_AZ_households_info[, c(1, 22)]))
cluster_leak$kmeans <- label_clusters(model_leak_kmeans$cluster, low_id = 1, high_id = 2)
cluster_leak$pams   <- label_clusters(model_leak_pam$clustering, low_id = 1, high_id = 2)

cluster_leak_long <- gather(cluster_leak, ClusterMethod, Efficiency, kmeans:pams, factor_key = TRUE)
cluster_leak_long$Efficiency <- factor(cluster_leak_long$Efficiency, levels = c("Low", "High"))

ggplot(cluster_leak_long, aes(x = Efficiency, y = LEAK_Avg.gal, fill = ClusterMethod)) +
  geom_boxplot() + theme_classic2() +
  ylab("Leak Avg. Gallons per Event") + xlab("Leak Level") +
  theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))

kruskal.test(LEAK_Avg.gal ~ kmeans, data = cluster_leak)
kruskal.test(LEAK_Avg.gal ~ pams,   data = cluster_leak)


### HOME VALUE — USD ###
# Clustered as a contextual property variable alongside fixture efficiency

boxplot(flume2_AZ_households_info$HomeValue_USD, main = "Home Value (USD)")

homevalue_vals <- flume2_AZ_households_info$HomeValue_USD
model_hv_kmeans <- kmeans(na.omit(homevalue_vals), 2); model_hv_kmeans$centers
model_hv_pam    <- pam(na.omit(homevalue_vals),    2); model_hv_pam$medoids

cluster_homevalue <- data.frame(na.omit(flume2_AZ_households_info[, c(1, 9)]))
cluster_homevalue$kmeans <- label_clusters(model_hv_kmeans$cluster, low_id = 1, high_id = 2)
cluster_homevalue$pams   <- label_clusters(model_hv_pam$clustering, low_id = 1, high_id = 2)

cluster_homevalue_long <- gather(cluster_homevalue, ClusterMethod, Efficiency, kmeans:pams, factor_key = TRUE)
cluster_homevalue_long$Efficiency <- factor(cluster_homevalue_long$Efficiency, levels = c("Low", "High"))

ggplot(cluster_homevalue_long, aes(x = Efficiency, y = HomeValue_USD, fill = ClusterMethod)) +
  geom_boxplot() + theme_classic2() +
  ylab("Home Value (USD)") + xlab("Home Value") +
  theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))

kruskal.test(HomeValue_USD ~ kmeans, data = cluster_homevalue)
kruskal.test(HomeValue_USD ~ pams,   data = cluster_homevalue)


### LANDSCAPE SIZE — sq.ft (LotSize - HomeSize) ###
# Negative values (lot < home footprint) are data errors; set to NA.
# Four clusters used (k=4) to reflect the wider size range: Small, Small-med,
# Med-large, Large. Confirmed via silhouette analysis.

flume2_AZ_households_info$LandscapeSize_Sq.Ft <- flume2_AZ_households_info$LotSize_Sq.Ft -
  flume2_AZ_households_info$HomeSize_Sq.Ft
flume2_AZ_households_info$LandscapeSize_Sq.Ft <- replace(
  flume2_AZ_households_info$LandscapeSize_Sq.Ft,
  flume2_AZ_households_info$LandscapeSize_Sq.Ft < 0, NA
)

fviz_nbclust(scale(na.omit(flume2_AZ_households_info$LandscapeSize_Sq.Ft)),
             pam, method = "silhouette") + theme_minimal()

landscape_vals <- flume2_AZ_households_info$LandscapeSize_Sq.Ft
model_ls_kmeans <- kmeans(na.omit(landscape_vals), 4); model_ls_kmeans$centers
model_ls_pam    <- pam(na.omit(landscape_vals),    4); model_ls_pam$medoids

cluster_landscape <- data.frame(na.omit(flume2_AZ_households_info[, c(1, 94)]))
cluster_landscape$kmeans <- factor(
  ifelse(model_ls_kmeans$cluster == 1, "Small",
         ifelse(model_ls_kmeans$cluster == 2, "Small-med",
                ifelse(model_ls_kmeans$cluster == 3, "Med-large",
                       ifelse(model_ls_kmeans$cluster == 4, "Large", NA)))),
  levels = c("Small", "Small-med", "Med-large", "Large")
)
cluster_landscape$pams <- factor(
  ifelse(model_ls_pam$clustering == 1, "Small",
         ifelse(model_ls_pam$clustering == 2, "Small-med",
                ifelse(model_ls_pam$clustering == 3, "Med-large",
                       ifelse(model_ls_pam$clustering == 4, "Large", NA)))),
  levels = c("Small", "Small-med", "Med-large", "Large")
)

cluster_landscape_long <- gather(cluster_landscape, ClusterMethod, Efficiency, kmeans:pams, factor_key = TRUE)
cluster_landscape_long$Efficiency <- factor(cluster_landscape_long$Efficiency,
                                            levels = c("Small", "Small-med", "Med-large", "Large"))

ggplot(cluster_landscape_long, aes(x = Efficiency, y = LandscapeSize_Sq.Ft, fill = ClusterMethod)) +
  geom_boxplot() + theme_classic2() +
  ylab("Landscape Size (Sq.Ft)") + xlab("Landscape Size Category") +
  theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))

kruskal.test(LandscapeSize_Sq.Ft ~ kmeans, data = cluster_landscape)
dunnTest(LandscapeSize_Sq.Ft ~ kmeans, data = cluster_landscape, method = "holm")
kruskal.test(LandscapeSize_Sq.Ft ~ pams, data = cluster_landscape)
dunnTest(LandscapeSize_Sq.Ft ~ pams,   data = cluster_landscape, method = "holm")


# ------------------------------------------------------------------------------
# 7.b.ii.e Summary panel plots — k-means vs. PAM side by side, then PAM only
# ------------------------------------------------------------------------------
# Two panel sets: (1) both clustering methods overlaid for method comparison;
# (2) PAM-only results for the final efficiency classifications used downstream.

# Both methods — side-by-side comparison
combined_both <- ggplot(cluster_toilet_long,        aes(x = Efficiency, y = TOILET_Avg.gal,          fill = ClusterMethod)) + geom_boxplot() + theme_classic2() + ylab("Toilet Flush Volume (gallons)")             + xlab("Toilet Efficiency")         + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1)) + scale_y_continuous(minor_breaks = seq(0, 5, 0.5))
b_both        <- ggplot(cluster_shower_long,         aes(x = Efficiency, y = SHOWER_Avg.GPM,          fill = ClusterMethod)) + geom_boxplot() + theme_classic2() + ylab("Shower Flow Rate (GPM)")                    + xlab("Showerhead Efficiency")     + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1)) + scale_y_continuous(minor_breaks = seq(0, 7, 1))
c_both        <- ggplot(cluster_washer_long,         aes(x = Efficiency, y = CLOTHES_WASHER_Avg.gal,  fill = ClusterMethod)) + geom_boxplot() + theme_classic2() + ylab("Clothes Washer Use per Cycle (gallons)")   + xlab("Clothes Washer Efficiency") + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))
d_both        <- ggplot(cluster_dishwasher_long,     aes(x = Efficiency, y = DISH_WASHER_Avg.gal,     fill = ClusterMethod)) + geom_boxplot() + theme_classic2() + ylab("Dishwasher Use per Cycle (gallons)")        + xlab("Dishwasher Efficiency")     + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))
e_both        <- ggplot(cluster_faucet_long,         aes(x = Efficiency, y = FAUCET_Avg.GPM,          fill = ClusterMethod)) + geom_boxplot() + theme_classic2() + ylab("Faucet Avg. GPM")                           + xlab("Faucet Efficiency")         + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))
f_both        <- ggplot(cluster_irrigation_gal_long, aes(x = Efficiency, y = IRRIGATION_Avg.gal,      fill = ClusterMethod)) + geom_boxplot() + theme_classic2() + ylab("Irrigation Avg. Gallons per Event")        + xlab("Irrigation Efficiency")     + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))

combined_both_panel <- combined_both + b_both + c_both + d_both + e_both + f_both &
  theme(legend.position = "bottom")
combined_both_panel + plot_layout(guides = "collect")

# PAM only — final efficiency classifications
pam_toilet    <- ggplot(filter(cluster_toilet_long,          ClusterMethod == "pams"), aes(x = Efficiency, y = TOILET_Avg.gal,         fill = Efficiency)) + geom_boxplot() + theme_classic2() + ylab("Toilet Flush Volume (gallons)")           + xlab("Toilet Efficiency")         + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1)) + scale_y_continuous(minor_breaks = seq(0, 5, 0.5))
pam_shower    <- ggplot(filter(cluster_shower_long,          ClusterMethod == "pams"), aes(x = Efficiency, y = SHOWER_Avg.GPM,          fill = Efficiency)) + geom_boxplot() + theme_classic2() + ylab("Shower Flow Rate (GPM)")                  + xlab("Showerhead Efficiency")     + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1)) + scale_y_continuous(minor_breaks = seq(0, 7, 1))
pam_washer    <- ggplot(filter(cluster_washer_long,          ClusterMethod == "pams"), aes(x = Efficiency, y = CLOTHES_WASHER_Avg.gal,  fill = Efficiency)) + geom_boxplot() + theme_classic2() + ylab("Clothes Washer Use per Cycle (gallons)")  + xlab("Clothes Washer Efficiency") + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))
pam_dishwash  <- ggplot(filter(cluster_dishwasher_long,      ClusterMethod == "pams"), aes(x = Efficiency, y = DISH_WASHER_Avg.gal,     fill = Efficiency)) + geom_boxplot() + theme_classic2() + ylab("Dishwasher Use per Cycle (gallons)")       + xlab("Dishwasher Efficiency")     + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))
pam_faucet    <- ggplot(filter(cluster_faucet_long,          ClusterMethod == "pams"), aes(x = Efficiency, y = FAUCET_Avg.GPM,          fill = Efficiency)) + geom_boxplot() + theme_classic2() + ylab("Faucet Avg. GPM")                          + xlab("Faucet Efficiency")         + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))
pam_irrigation <- ggplot(filter(cluster_irrigation_gal_long, ClusterMethod == "pams"), aes(x = Efficiency, y = IRRIGATION_Avg.gal,      fill = Efficiency)) + geom_boxplot() + theme_classic2() + ylab("Irrigation Avg. Gallons per Event")       + xlab("Irrigation Efficiency")     + theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1), panel.grid.major.y = element_line(linetype = "dashed", colour = "blue", linewidth = 0.1))

pam_panel <- pam_toilet + pam_shower + pam_washer + pam_dishwash + pam_faucet + pam_irrigation &
  theme(legend.position = "bottom")
pam_panel + plot_layout(guides = "collect")


# ------------------------------------------------------------------------------
# 7.b.ii.f Join PAM efficiency labels to household info and export
# ------------------------------------------------------------------------------
# PAM-derived efficiency labels for all fixtures are matched back to
# flume2_AZ_households_info by Location_ID and added as new columns.


flume2_AZ_households_info$Toilet_Efficiency    <- cluster_toilet$pams[match(flume2_AZ_households_info$Location_ID,         cluster_toilet$Location_ID)]
flume2_AZ_households_info$ShowerHead_Efficiency <- cluster_shower$pams[match(flume2_AZ_households_info$Location_ID,        cluster_shower$Location_ID)]
flume2_AZ_households_info$Laundry_Efficiency   <- cluster_washer$pams[match(flume2_AZ_households_info$Location_ID,         cluster_washer$Location_ID)]
flume2_AZ_households_info$Dishwash_Efficiency  <- cluster_dishwasher$pams[match(flume2_AZ_households_info$Location_ID,     cluster_dishwasher$Location_ID)]
flume2_AZ_households_info$Irrigation_Efficiency <- cluster_irrigation_gal$pams[match(flume2_AZ_households_info$Location_ID, cluster_irrigation_gal$Location_ID)]
flume2_AZ_households_info$Faucet_Efficiency    <- cluster_faucet$pams[match(flume2_AZ_households_info$Location_ID,         cluster_faucet$Location_ID)]
flume2_AZ_households_info$Leak_Level           <- cluster_leak$pams[match(flume2_AZ_households_info$Location_ID,           cluster_leak$Location_ID)]
flume2_AZ_households_info$Home_Cost            <- cluster_homevalue$pams[match(flume2_AZ_households_info$Location_ID,      cluster_homevalue$Location_ID)]




# ------------------------------------------------------------------------------
# 7.c Factorize efficiency classifications for downstream use
# ------------------------------------------------------------------------------
# Converts the PAM-derived efficiency columns added in Section "7.b.ii" into ordered
# factors for use in regression models and group comparisons (Sections 8–11).
#
# NOTE: HOME_Efficiency and INDOOR_Efficiency are composite scores that aggregate
# across multiple fixture efficiency labels. If these columns are not present in
# flume2_AZ_households_info at this point, they must be derived before proceeding.
# Typical derivation: a household is "High" HOME_Efficiency if the majority of its
# fixture clusters are High; otherwise "Low" or "Mixed".

flume2_AZ_households_info$Location_ID <- as.character(flume2_AZ_households_info$Location_ID)

# Set factor levels for efficiency and leak classifications (Low < High)
flume2_AZ_households_info$Irrigation_Efficiency <- factor(flume2_AZ_households_info$Irrigation_Efficiency,
                                                          levels = c("Low", "High"))
flume2_AZ_households_info$HOME_Efficiency   <- factor(flume2_AZ_households_info$HOME_Efficiency,
                                                      levels = c("Low", "High"))
flume2_AZ_households_info$INDOOR_Efficiency <- factor(flume2_AZ_households_info$INDOOR_Efficiency,
                                                      levels = c("Low", "High"))
flume2_AZ_households_info$Leak_Level        <- factor(flume2_AZ_households_info$Leak_Level,
                                                      levels = c("Low", "High"))

# Optional: export for external use and documentation
# write.csv(flume2_AZ_households_info, "Flume2_AZ_Households_Info.csv")


# ------------------------------------------------------------------------------
# 7.d Build Flume 2 daily water use dataset
# ------------------------------------------------------------------------------
# Subset the all-AZ daily water use data to Flume 2 households only, then join
# weather and household info. NumResidents is overridden with the Flume 2 value.
#
# Note: GPCD columns (lines below) divide by NumResidents using the column name
# as it appears after the wateruse_wide join — verify column name is correct if
# the wateruse_wide structure changes upstream.

flume2_AZ_wateruse <- data.frame(
  Location_ID = intersect(flume2_devices, unique(wateruse$Location_ID))
)
flume2_AZ_wateruse <- merge(flume2_AZ_wateruse, wateruse, by = "Location_ID")
flume2_AZ_wateruse <- flume2_AZ_wateruse[order(flume2_AZ_wateruse[, 1],
                                               flume2_AZ_wateruse[, 2]), ]

# Join weather and select columns from wateruse_wide
# Cols 1:6 = ID/date/use; 15:17 = weather variables
flume2_AZ_wateruse_wide <- flume2_AZ_wateruse %>%
  left_join(wateruse_wide[, c(1:6, 15:17)], by = c('Location_ID', 'Date'))

# Override NumResidents with Flume 2 value
flume2_AZ_wateruse_wide$NumResidents <- flume2_numresidents$NumResf2[
  match(flume2_AZ_wateruse_wide$Location_ID, flume2_numresidents$Location_ID)
]

# Count active device days per household (days with recorded water use)
flume2_wateruse_activedays <- flume2_AZ_wateruse_wide %>% count(Location_ID)
colnames(flume2_wateruse_activedays) <- c("Location_ID", "DeviceActiveDays")

flume2_AZ_wateruse_wide$DeviceActiveDays <- flume2_wateruse_activedays$DeviceActiveDays[
  match(flume2_AZ_wateruse_wide$Location_ID, flume2_wateruse_activedays$Location_ID)
]
flume2_AZ_households_info$DeviceActiveDays <- flume2_wateruse_activedays$DeviceActiveDays[
  match(flume2_AZ_households_info$Location_ID, flume2_wateruse_activedays$Location_ID)
]

# Derive per-capita use (GPCD = GPHD / NumResidents)
flume2_AZ_wateruse_wide$Indoor.GPCD  <- flume2_AZ_wateruse_wide$Indoor.GPHD  / flume2_AZ_wateruse_wide$NumResidents
flume2_AZ_wateruse_wide$Outdoor.GPCD <- flume2_AZ_wateruse_wide$Outdoor.GPHD / flume2_AZ_wateruse_wide$NumResidents
flume2_AZ_wateruse_wide$Total.GPCD   <- flume2_AZ_wateruse_wide$Total.GPHD   / flume2_AZ_wateruse_wide$NumResidents

flume2_AZ_wateruse_wide <- flume2_AZ_wateruse_wide %>%
  mutate_at(vars(Indoor.GPHD, Outdoor.GPHD, Total.GPHD,
                 Indoor.GPCD, Outdoor.GPCD, Total.GPCD), funs(round(., 2)))

# Reorder columns for consistency with wateruse_wide structure
flume2_AZ_wateruse_wide <- flume2_AZ_wateruse_wide[, c(1, 2, 8:11, 3:5, 17:19, 6, 7, 12:14, 15, 16)]

# Optional: export raw daily Flume 2 water use
# write.csv(flume2_AZ_wateruse_wide, "Flume2_AZ_Household_DailyWaterUse_RAW.csv")


# ------------------------------------------------------------------------------
# 7.e Filter and pivot to wide format (GPHD and GPCD)
# ------------------------------------------------------------------------------
# Three filters applied: minimum use threshold + minimum active device days (>=90).
# The 90-day threshold ensures households with very short monitoring periods
# do not skew household-level averages.

flume2_AZ_wateruse_indoor  <- subset(flume2_AZ_wateruse_wide[, c(1, 2, 7, 10, 19)],
                                     Indoor.GPHD  >= 10.0 & DeviceActiveDays >= 90)
flume2_AZ_wateruse_outdoor <- subset(flume2_AZ_wateruse_wide[, c(1, 2, 8, 11, 19)],
                                     Outdoor.GPHD >= 10.0 & DeviceActiveDays >= 90)
flume2_AZ_wateruse_total   <- subset(flume2_AZ_wateruse_wide[, c(1, 2, 9, 12, 19)],
                                     Total.GPHD   >= 20.0 & DeviceActiveDays >= 90)

# Pivot daily GPHD and GPCD to wide format (one column per household)
flume2_AZ_wateruse_indoorGPHD_daily  <- spread(flume2_AZ_wateruse_indoor[,  c(1:3)], Location_ID, Indoor.GPHD)
flume2_AZ_wateruse_outdoorGPHD_daily <- spread(flume2_AZ_wateruse_outdoor[, c(1:3)], Location_ID, Outdoor.GPHD)
flume2_AZ_wateruse_totalGPHD_daily   <- spread(flume2_AZ_wateruse_total[,   c(1:3)], Location_ID, Total.GPHD)

flume2_AZ_wateruse_indoorGPCD_daily  <- spread(flume2_AZ_wateruse_indoor[,  c(1, 2, 4)], Location_ID, Indoor.GPCD)
flume2_AZ_wateruse_outdoorGPCD_daily <- spread(flume2_AZ_wateruse_outdoor[, c(1, 2, 4)], Location_ID, Outdoor.GPCD)
flume2_AZ_wateruse_totalGPCD_daily   <- spread(flume2_AZ_wateruse_total[,   c(1, 2, 4)], Location_ID, Total.GPCD)

# Household ID vectors (GPCD column names match GPHD column names for each use type)
households_names5  <- colnames(flume2_AZ_wateruse_indoorGPHD_daily)
households_names6  <- colnames(flume2_AZ_wateruse_outdoorGPHD_daily)
households_names7  <- colnames(flume2_AZ_wateruse_totalGPHD_daily)


# ------------------------------------------------------------------------------
# 7.f Build per-household summary tables (mean and median)
# ------------------------------------------------------------------------------
# Collapse wide daily matrices to one row per household, then join full
# household characteristics from flume2_AZ_households_info (includes efficiency
# classifications and disaggregation metrics loaded in 7.c).

flume2_household_indoor <- data.frame(
  Location_ID         = households_names5[-1],
  Average.Indoor.GPHD = t(round(summarize_all(flume2_AZ_wateruse_indoorGPHD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Indoor.GPHD  = t(round(summarize_all(flume2_AZ_wateruse_indoorGPHD_daily[-1], median, na.rm = TRUE), 2)),
  Average.Indoor.GPCD = t(round(summarize_all(flume2_AZ_wateruse_indoorGPCD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Indoor.GPCD  = t(round(summarize_all(flume2_AZ_wateruse_indoorGPCD_daily[-1], median, na.rm = TRUE), 2))
)
flume2_household_indoor <- flume2_household_indoor %>%
  left_join(flume2_AZ_households_info, by = "Location_ID")

flume2_household_outdoor <- data.frame(
  Location_ID          = households_names6[-1],
  Average.Outdoor.GPHD = t(round(summarize_all(flume2_AZ_wateruse_outdoorGPHD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Outdoor.GPHD  = t(round(summarize_all(flume2_AZ_wateruse_outdoorGPHD_daily[-1], median, na.rm = TRUE), 2)),
  Average.Outdoor.GPCD = t(round(summarize_all(flume2_AZ_wateruse_outdoorGPCD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Outdoor.GPCD  = t(round(summarize_all(flume2_AZ_wateruse_outdoorGPCD_daily[-1], median, na.rm = TRUE), 2))
)
flume2_household_outdoor <- flume2_household_outdoor %>%
  left_join(flume2_AZ_households_info, by = "Location_ID")

# Annual outdoor water use metrics: total gallons per year and gallons per sq.ft of lot
flume2_household_outdoor$OutdoorUse_gal      <- flume2_household_outdoor$Average.Outdoor.GPCD *
  flume2_household_outdoor$NumResidents * 365
flume2_household_outdoor$Outdoor_Gal.per.Sq.ft <- flume2_household_outdoor$OutdoorUse_gal /
  flume2_household_outdoor$LotSize_Sq.Ft

flume2_household_total <- data.frame(
  Location_ID        = households_names7[-1],
  Average.Total.GPHD = t(round(summarize_all(flume2_AZ_wateruse_totalGPHD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Total.GPHD  = t(round(summarize_all(flume2_AZ_wateruse_totalGPHD_daily[-1], median, na.rm = TRUE), 2)),
  Average.Total.GPCD = t(round(summarize_all(flume2_AZ_wateruse_totalGPCD_daily[-1], mean,   na.rm = TRUE), 2)),
  Median.Total.GPCD  = t(round(summarize_all(flume2_AZ_wateruse_totalGPCD_daily[-1], median, na.rm = TRUE), 2))
)
flume2_household_total <- flume2_household_total %>%
  left_join(flume2_AZ_households_info, by = "Location_ID")

# Combined household-level summary: indoor + outdoor + total side by side
flume2_household_wateruse <- flume2_household_indoor[, c(1:5)] %>%
  left_join(flume2_household_outdoor[, c(1:5)], by = "Location_ID") %>%
  left_join(flume2_household_total,              by = "Location_ID")

# Optional: export household-level filtered average water use
# write.csv(flume2_household_wateruse, "Flume2_AZ_Household_Filtered_Avg_Wateruse.csv")

# Pool subsets for stratified analysis
flume2_household_wateruse_pool   <- subset(flume2_household_wateruse, HasPool == "TRUE")
flume2_household_wateruse_nopool <- subset(flume2_household_wateruse, HasPool == "FALSE")


# ------------------------------------------------------------------------------
# 7.g Use vs. occupancy plots — all households, pool, and no-pool subsets
# ------------------------------------------------------------------------------
# Area plots show how mean total GPHD and GPCD scale with household size.
# Panels are produced for: all Flume 2 households, pool homes, and no-pool homes.

# Helper to summarize total use by NumResidents
summarise_by_occupancy <- function(df_total, df_gpcd = NULL) {
  if (is.null(df_gpcd)) df_gpcd <- df_total
  data.frame(
    summarise(group_by(df_gpcd,   NumResidents),
              Mean.GPCD = mean(Average.Total.GPCD), Median.GPCD = median(Average.Total.GPCD),
              Max.GPCD  = max(Average.Total.GPCD),  Min.GPCD    = min(Average.Total.GPCD),
              Count     = length(NumResidents)),
    summarise(group_by(df_total,  NumResidents),
              Mean.GPHD = mean(Average.Total.GPHD), Median.GPHD = median(Average.Total.GPHD),
              Max.GPHD  = max(Average.Total.GPHD),  Min.GPHD    = min(Average.Total.GPHD))
  )
}

plot_occupancy <- function(df, title_label) {
  aa <- ggplot(df, aes(NumResidents, Mean.GPHD)) +
    geom_area(fill = "orangered3") + theme_classic2() +
    xlab("Number of Residents") + ylab("Household Water Use\nin a Day (Gallons)") +
    ylim(0, 1000) + scale_x_continuous(breaks = 1:8)
  bb <- ggplot(df, aes(NumResidents, Mean.GPCD)) +
    geom_area(fill = "olivedrab3") + theme_classic2() +
    xlab("Number of Residents") + ylab("Per Person Water Use\nin a Day (Gallons)") +
    ylim(0, 400)  + scale_x_continuous(breaks = 1:8)
  grid.arrange(aa, bb, ncol = 1, top = title_label)
}

df2         <- summarise_by_occupancy(flume2_household_wateruse)
df2_pool    <- summarise_by_occupancy(flume2_household_wateruse_pool)
df2_nopool  <- summarise_by_occupancy(flume2_household_wateruse_nopool)

plot_occupancy(df2,        "Water Use and Household Occupancy (All households: 706)")
plot_occupancy(df2_pool,   "Water Use and Household Occupancy (Has Swimming Pool: 419)")
plot_occupancy(df2_nopool, "Water Use and Household Occupancy (No Swimming Pool: 287)")


# ------------------------------------------------------------------------------
# 7.h Regional daily summaries and weather join
# ------------------------------------------------------------------------------
# Compute daily mean/median across all Flume 2 households for indoor, outdoor,
# and total use, then join regional mean weather (precip, ET, TempMax).
# These daily summaries feed the time series plots and MLR models in Section 8.

flume2_daily_indoor <- data.frame(
  Date                = flume2_AZ_wateruse_indoorGPHD_daily$Date,
  Average.Indoor.GPHD = round(apply(flume2_AZ_wateruse_indoorGPHD_daily[2:ncol(flume2_AZ_wateruse_indoorGPHD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.Indoor.GPHD  = round(apply(flume2_AZ_wateruse_indoorGPHD_daily[2:ncol(flume2_AZ_wateruse_indoorGPHD_daily)], 1, median, na.rm = TRUE), 2),
  Average.Indoor.GPCD = round(apply(flume2_AZ_wateruse_indoorGPCD_daily[2:ncol(flume2_AZ_wateruse_indoorGPCD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.Indoor.GPCD  = round(apply(flume2_AZ_wateruse_indoorGPCD_daily[2:ncol(flume2_AZ_wateruse_indoorGPCD_daily)], 1, median, na.rm = TRUE), 2)
)

flume2_daily_outdoor <- data.frame(
  Date                 = flume2_AZ_wateruse_outdoorGPHD_daily$Date,
  Average.Outdoor.GPHD = round(apply(flume2_AZ_wateruse_outdoorGPHD_daily[2:ncol(flume2_AZ_wateruse_outdoorGPHD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.Outdoor.GPHD  = round(apply(flume2_AZ_wateruse_outdoorGPHD_daily[2:ncol(flume2_AZ_wateruse_outdoorGPHD_daily)], 1, median, na.rm = TRUE), 2),
  Average.Outdoor.GPCD = round(apply(flume2_AZ_wateruse_outdoorGPCD_daily[2:ncol(flume2_AZ_wateruse_outdoorGPCD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.Outdoor.GPCD  = round(apply(flume2_AZ_wateruse_outdoorGPCD_daily[2:ncol(flume2_AZ_wateruse_outdoorGPCD_daily)], 1, median, na.rm = TRUE), 2)
)

flume2_daily_total <- data.frame(
  Date               = flume2_AZ_wateruse_totalGPHD_daily$Date,
  Average.Total.GPHD = round(apply(flume2_AZ_wateruse_totalGPHD_daily[2:ncol(flume2_AZ_wateruse_totalGPHD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.Total.GPHD  = round(apply(flume2_AZ_wateruse_totalGPHD_daily[2:ncol(flume2_AZ_wateruse_totalGPHD_daily)], 1, median, na.rm = TRUE), 2),
  Average.Total.GPCD = round(apply(flume2_AZ_wateruse_totalGPCD_daily[2:ncol(flume2_AZ_wateruse_totalGPCD_daily)], 1, mean,   na.rm = TRUE), 2),
  Median.Total.GPCD  = round(apply(flume2_AZ_wateruse_totalGPCD_daily[2:ncol(flume2_AZ_wateruse_totalGPCD_daily)], 1, median, na.rm = TRUE), 2)
)

# Join regional mean weather to each daily summary by matching on Date
for (df_name in c("flume2_daily_indoor", "flume2_daily_outdoor", "flume2_daily_total")) {
  df <- get(df_name)
  df$Average.Precip  <- AZ_Precip_daily_WY22$Mean.Precip[match(AZ_Precip_daily_WY22$Date,  df$Date)]
  df$Average.ET      <- AZ_ET_daily_WY22$Mean.ET[match(AZ_ET_daily_WY22$Date,              df$Date)]
  df$Average.TempMax <- AZ_TempMax_daily_WY22$Mean.TempMax[match(AZ_TempMax_daily_WY22$Date, df$Date)]
  assign(df_name, df)
}

# Combine indoor, outdoor, and total daily summaries into a single data frame
flume2_daily_wateruse <- flume2_daily_indoor[, c(1:5)] %>%
  left_join(flume2_daily_outdoor[, c(1:5)], by = "Date") %>%
  left_join(flume2_daily_total,              by = "Date")

# Optional: export combined daily Flume 2 water use summary
# write.csv(flume2_daily_wateruse, "Flume2_AZ_Daily_Avg_WaterUse.csv")


# ------------------------------------------------------------------------------
# 7.i Daily household-level dataset with efficiency and weather info
# ------------------------------------------------------------------------------
# Joins daily water use for each Flume 2 household with household characteristics
# and efficiency classifications from flume2_AZ_households_info.
# A Season variable is derived from Month using Arizona-specific season breaks:
#   Winter (Jan-Feb), Spring (Mar-Apr), Dry-Summer (May-Jun),
#   Wet-Summer (Jul-Sep, monsoon), Fall (Oct-Dec)

flume2_AZ_daily_household <- flume2_AZ_wateruse_wide %>%
  left_join(flume2_AZ_households_info[, c(1, 6:14, 84:92)], by = "Location_ID")

flume2_AZ_daily_household$Season <- cut(
  flume2_AZ_daily_household$Month,
  breaks = c(0, 2, 4, 6, 9, 12),
  labels = c('Winter', 'Spring', 'Dry-Summer', 'Wet-Summer', 'Fall')
)

# Optional: export daily household-level dataset
# write.csv(flume2_AZ_daily_household, "Flume2_AZ_Household_DailyWateruse_Info_RAW.csv")

# Seasonal distribution plots — daily Total GPHD by month and season
ggplot(flume2_AZ_daily_household, aes(x = Month,  y = Total.GPHD, group = Month)) +
  geom_boxplot() + theme_classic2() + ylim(0, 3000)

ggplot(flume2_AZ_daily_household, aes(x = Season, y = Total.GPHD, group = Season)) +
  geom_boxplot() + theme_classic2() + ylim(0, 1500)

ggplot(flume2_AZ_daily_household, aes(x = Season, y = Total.GPHD, group = Season)) +
  geom_boxplot(outlier.shape = NA) + theme_classic2() + ylim(0, 1500)

# Quick daily regression: total GPHD ~ ET + max temperature (household level)
flume2_outdoor.household.daily.fit <- lm(Total.GPHD ~ DAY_Penman + AirTempMax.DegF.,
                                         data = flume2_AZ_daily_household)
summary(flume2_outdoor.household.daily.fit)



# ==============================================================================
# SECTION 8: FLUME 2 — TIME SERIES VISUALIZATIONS (DAILY AND WEEKLY)
# ==============================================================================

# ------------------------------------------------------------------------------
# 8.a Daily median water use — indoor, outdoor, total (GPHD and GPCD)
# ------------------------------------------------------------------------------
# Side-by-side time series of median daily use across all Flume 2 households
# for the full Water Year 2022. Legend is collected at the bottom via patchwork.

a1 <- ggplot(flume2_daily_wateruse) +
  geom_line(aes(x = Date, y = Median.Total.GPHD,   color = "Total")) +
  geom_line(aes(x = Date, y = Median.Indoor.GPHD,  color = "Indoor")) +
  geom_line(aes(x = Date, y = Median.Outdoor.GPHD, color = "Outdoor")) +
  ylab("Median GPHD") + theme_bw() +
  scale_color_manual(name = "Water Use",
                     values = c("Total" = "black", "Indoor" = "red", "Outdoor" = "orange"))

a2 <- ggplot(flume2_daily_wateruse) +
  geom_line(aes(x = Date, y = Median.Total.GPCD,   color = "Total")) +
  geom_line(aes(x = Date, y = Median.Indoor.GPCD,  color = "Indoor")) +
  geom_line(aes(x = Date, y = Median.Outdoor.GPCD, color = "Outdoor")) +
  ylab("Median GPCD") + theme_bw() +
  scale_color_manual(name = "Water Use",
                     values = c("Total" = "black", "Indoor" = "red", "Outdoor" = "orange"))

combined <- a1 + a2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")


# ------------------------------------------------------------------------------
# 8.b Aggregate daily data to weekly means and build climate summary table
# ------------------------------------------------------------------------------
# Daily zoo time series are aggregated to weekly means (water use) or weekly
# sums (precipitation, ET) to smooth day-to-day noise for climate comparison plots.
# Water use and climate variables are combined into a single wide data frame.
#
# Note: the dual y-axis plots below scale GPCD by a factor (x0.01 or x0.1) to
# fit on the same axis as precipitation/ET/temperature. The secondary axis
# reverses this scaling for labeling.

WY22_dates <- seq(from = as.Date("2021-10-01"), to = as.Date("2022-09-30"), by = 1)

ts_indoor_GPCD  <- zoo(flume2_daily_wateruse$Average.Indoor.GPCD,  WY22_dates)
ts_outdoor_GPCD <- zoo(flume2_daily_wateruse$Average.Outdoor.GPCD, WY22_dates)
ts_total_GPCD   <- zoo(flume2_daily_wateruse$Average.Total.GPCD,   WY22_dates)
ts_indoor_GPHD  <- zoo(flume2_daily_wateruse$Average.Indoor.GPHD,  WY22_dates)
ts_outdoor_GPHD <- zoo(flume2_daily_wateruse$Average.Outdoor.GPHD, WY22_dates)
ts_total_GPHD   <- zoo(flume2_daily_wateruse$Average.Total.GPHD,   WY22_dates)
ts_precip       <- zoo(flume2_daily_wateruse$Average.Precip,        WY22_dates)
ts_ET           <- zoo(flume2_daily_wateruse$Average.ET,            WY22_dates)
ts_TempMax      <- zoo(flume2_daily_wateruse$Average.TempMax,       WY22_dates)

# Aggregate to weekly means (water use, TempMax) or sums (precip, ET)
agg_week <- function(ts, FUN) data.frame(aggregate(ts, as.Date(cut(index(ts), "week")), FUN))

weekly_ts_indoor_GPCD  <- agg_week(ts_indoor_GPCD,  mean)
weekly_ts_outdoor_GPCD <- agg_week(ts_outdoor_GPCD, mean)
weekly_ts_total_GPCD   <- agg_week(ts_total_GPCD,   mean)
weekly_ts_indoor_GPHD  <- agg_week(ts_indoor_GPHD,  mean)
weekly_ts_outdoor_GPHD <- agg_week(ts_outdoor_GPHD, mean)
weekly_ts_total_GPHD   <- agg_week(ts_total_GPHD,   mean)
weekly_ts_precip        <- agg_week(ts_precip,       sum)
weekly_ts_ET            <- agg_week(ts_ET,           sum)
weekly_ts_TempMax       <- agg_week(ts_TempMax,      mean)

flume2_weekly_wateruse_climate <- cbind(
  weekly_ts_indoor_GPCD,  weekly_ts_indoor_GPHD,
  weekly_ts_outdoor_GPCD, weekly_ts_outdoor_GPHD,
  weekly_ts_total_GPCD,   weekly_ts_total_GPHD,   # note: GPCD then GPHD
  weekly_ts_precip, weekly_ts_ET, weekly_ts_TempMax
)
colnames(flume2_weekly_wateruse_climate) <- c(
  "Weekly.Indoor.GPCD",  "Weekly.Indoor.GPHD",
  "Weekly.Outdoor.GPCD", "Weekly.Outdoor.GPHD",
  "Weekly.Total.GPCD",   "Weekly.Total.GPHD",
  "Weekly.Precip", "Weekly.ET", "Weekly.TempMax"
)
flume2_weekly_wateruse_climate <- rownames_to_column(flume2_weekly_wateruse_climate, var = "Date")

# Optional: export weekly summary
# write.csv(flume2_weekly_wateruse_climate, "Flume2_Weekly_Avg_Wateruse.csv")


# ------------------------------------------------------------------------------
# 8.c Weekly water use vs. climate — dual y-axis panel plots
# ------------------------------------------------------------------------------
# 3x3 grid: rows = climate variable (precip, ET, TempMax);
#           cols = use type (indoor, outdoor, total).
# Bars = weekly climate; red line = weekly water use (GPCD, scaled to fit axis).

make_climate_wateruse_plot <- function(climate_var, use_var, scale_factor,
                                       y_label, title_label) {
  ggplot(flume2_weekly_wateruse_climate, aes(x = as.Date(Date))) +
    theme_bw() +
    geom_bar(aes(y = .data[[climate_var]]), stat = "identity", fill = "blue") +
    geom_line(aes(y = .data[[use_var]] * scale_factor), color = "red", linewidth = 1.5, group = 1) +
    scale_y_continuous(
      name      = y_label,
      sec.axis  = sec_axis(~ . / scale_factor, name = "Water Use (GPCD)")
    ) +
    labs(title = title_label, x = "Date") +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")
}

# Precipitation vs. indoor / outdoor / total
a1 <- make_climate_wateruse_plot("Weekly.Precip", "Weekly.Indoor.GPCD",  0.01, "Precipitation (in)", "Weekly Precipitation and Indoor Water Use")
a2 <- make_climate_wateruse_plot("Weekly.Precip", "Weekly.Outdoor.GPCD", 0.01, "Precipitation (in)", "Weekly Precipitation and Outdoor Water Use")
a3 <- make_climate_wateruse_plot("Weekly.Precip", "Weekly.Total.GPCD",   0.01, "Precipitation (in)", "Weekly Precipitation and Total Water Use")

# ET vs. indoor / outdoor / total
b1 <- make_climate_wateruse_plot("Weekly.ET", "Weekly.Indoor.GPCD",  0.01, "Evapotranspiration (in)", "Weekly ET and Indoor Water Use")
b2 <- make_climate_wateruse_plot("Weekly.ET", "Weekly.Outdoor.GPCD", 0.01, "Evapotranspiration (in)", "Weekly ET and Outdoor Water Use")
b3 <- make_climate_wateruse_plot("Weekly.ET", "Weekly.Total.GPCD",   0.01, "Evapotranspiration (in)", "Weekly ET and Total Water Use")

# Max temperature vs. indoor / outdoor / total
c1 <- make_climate_wateruse_plot("Weekly.TempMax", "Weekly.Indoor.GPCD",  0.1, "Temp (°F)", "Weekly Temperature and Indoor Water Use")
c2 <- make_climate_wateruse_plot("Weekly.TempMax", "Weekly.Outdoor.GPCD", 0.1, "Temp (°F)", "Weekly Temperature and Outdoor Water Use")
c3 <- make_climate_wateruse_plot("Weekly.TempMax", "Weekly.Total.GPCD",   0.1, "Temp (°F)", "Weekly Temperature and Total Water Use")

# Full 3x3 panel and total-only summary panel
grid.arrange(a1, a2, a3,
             b1, b2, b3,
             c1, c2, c3,
             ncol = 3)

grid.arrange(a3, b3, c3, ncol = 1)


# ==============================================================================
# SECTION 9: FLUME 2 — MLR MODELS (DAILY, WEEKLY, HOUSEHOLD LEVEL)
# ==============================================================================

# ------------------------------------------------------------------------------
# 9.a Daily and weekly weather-driven regression models
# ------------------------------------------------------------------------------
# Response: average daily/weekly outdoor and total GPHD
# Predictors: regional mean precipitation, ET, and max temperature
# Two daily outdoor models are fit: one with all three climate variables,
# one with TempMax only — to assess marginal contribution of each climate signal.
# The weekly outdoor model is similarly run with all three then TempMax + ET only.

# Daily models
flume2_outdoor.daily.fit.full <- lm(Average.Outdoor.GPHD ~ Average.Precip + Average.ET + Average.TempMax,
                                    data = flume2_daily_outdoor)
summary(flume2_outdoor.daily.fit.full)

flume2_total.daily.fit <- lm(Average.Total.GPHD ~ Average.Precip + Average.ET + Average.TempMax,
                             data = flume2_daily_total)
summary(flume2_total.daily.fit)

flume2_outdoor.daily.fit.temponly <- lm(Average.Outdoor.GPHD ~ Average.TempMax,
                                        data = flume2_daily_outdoor)
summary(flume2_outdoor.daily.fit.temponly)

# Weekly models
flume2_outdoor.weekly.fit.full <- lm(Weekly.Outdoor.GPHD ~ Weekly.Precip + Weekly.ET + Weekly.TempMax,
                                     data = flume2_weekly_wateruse_climate)
summary(flume2_outdoor.weekly.fit.full)

flume2_total.weekly.fit <- lm(log(Weekly.Total.GPHD) ~ Weekly.Precip + Weekly.ET + Weekly.TempMax,
                              data = flume2_weekly_wateruse_climate)
summary(flume2_total.weekly.fit)

flume2_outdoor.weekly.fit.reduced <- lm(Weekly.Outdoor.GPHD ~ Weekly.TempMax + Weekly.ET,
                                        data = flume2_weekly_wateruse_climate)
summary(flume2_outdoor.weekly.fit.reduced)


# ------------------------------------------------------------------------------
# 9.b Household-level MLR — indoor, outdoor, and total GPHD
# ------------------------------------------------------------------------------
# Two column subsets are provided for each use type reflecting different
# predictor strategies:
#   Version 1: individual fixture metrics (volume, duration, flow) + key chars
#   Version 2: composite efficiency scores + key household characteristics
# Both are run; Version 2 is the preferred parsimonious model for reporting.

# Indoor GPHD
# Version 1: individual fixture metrics
flume2_household_indoor.fit.v1 <- lm(Average.Indoor.GPHD ~ .,
                                     data = flume2_household_indoor[, c(2, 10:16, 21:24, 26:33, 35:42, 44:47, 74)])
summary(flume2_household_indoor.fit.v1)

# Version 2: composite efficiency scores (preferred)
flume2_household_indoor.fit.v2 <- lm(Average.Indoor.GPHD ~ .,
                                     data = flume2_household_indoor[, c(2, 10:16, 88:91, 93, 96)])
summary(flume2_household_indoor.fit.v2)

# Outdoor GPHD
# Version 1: individual fixture metrics
flume2_household_outdoor.fit.v1 <- lm(Average.Outdoor.GPHD ~ .,
                                      data = flume2_household_outdoor[, c(2, 10:18, 25, 29, 34, 38, 43, 47, 60, 73, 74)])
summary(flume2_household_outdoor.fit.v1)

# Version 2: composite efficiency scores (preferred)
flume2_household_outdoor.fit.v2 <- lm(Average.Outdoor.GPHD ~ .,
                                      data = flume2_household_outdoor[, c(2, 10:18, 48:87, 92)])
summary(flume2_household_outdoor.fit.v2)

# Total GPHD
# Version 1: individual fixture metrics
flume2_household_total.fit.v1 <- lm(Average.Total.GPHD ~ .,
                                    data = flume2_household_total[, c(2, 10:18, 21:74)])
summary(flume2_household_total.fit.v1)

# Version 2: composite efficiency scores (preferred)
flume2_household_total.fit.v2 <- lm(Average.Total.GPHD ~ .,
                                    data = flume2_household_total[, c(2, 10:18, 48:96)])
summary(flume2_household_total.fit.v2)

# Optional: save total GPHD MLR summary to a text file
# sink("Total.GPHD_MLR.txt")
# print(summary(flume2_household_total.fit.v1))
# sink()


# ==============================================================================
# SECTION 10: FLUME 2 — RF, STEPWISE REGRESSION, PCA, AND TREES
# ==============================================================================

# ------------------------------------------------------------------------------
# 10.a Random Forest — Total GPHD (Flume 2 households)
# ------------------------------------------------------------------------------
# Predictors: household characteristics + efficiency scores + fixture metrics
# (cols 10:18, 60, 73, 86, 88:96 from flume2_household_total)
# ntree = 300; top 35 variables shown in importance plot.

rforest.total.flume2 <- randomForest(Average.Total.GPHD ~ .,
                                     data = flume2_household_total[, c(2, 10:18, 60, 73, 86, 88:96)],
                                     ntree = 300, na.action = na.exclude)
plot(rforest.total.flume2, main = "Flume 2 Total RF — error convergence")

rf_tot_flume2 <- rownames_to_column(data.frame(rforest.total.flume2$importance), "variables")
rf_tot_flume2 <- rf_tot_flume2[order(rf_tot_flume2$IncNodePurity, decreasing = TRUE), ]
rf_tot_flume2 <- rf_tot_flume2[1:35, ]
plot_rf_importance(rforest.total.flume2, title = "Flume 2 Total GPHD — Variable Importance (Top 35)")


# ------------------------------------------------------------------------------
# 10.b PCA — Total GPHD (Flume 2 households, pre-stepwise predictor set)
# ------------------------------------------------------------------------------
# Predictor matrix: cols 9:18 (building/irrigation/efficiency chars) +
#                   cols 21:74 (fixture metrics)
# Factor variables converted to numeric; complete cases only.

water_total_f2 <- flume2_household_total
rownames(water_total_f2) <- water_total_f2[, 1]
water_total_f2 <- water_total_f2[, c(9:18, 21:74)]
water_total_f2$HasPool             <- as.numeric(water_total_f2$HasPool)
water_total_f2$IrrigationType      <- as.numeric(water_total_f2$IrrigationType)
water_total_f2$IrrigationFrequency <- as.numeric(water_total_f2$IrrigationFrequency)
water_total_f2 <- water_total_f2[complete.cases(water_total_f2), ]

pca_total_f2 <- princomp(water_total_f2)
print(pca_total_f2)
plot(pca_total_f2, type = "l")   # Scree plot
summary(pca_total_f2)
pca_loadings_f2 <- data.frame(pca_total_f2$loadings[, 1:4])
pca_loadings_f2

fviz_pca_var(pca_total_f2,
             col.var       = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title         = "Flume 2 Total GPHD PCA",
             arrowsize     = 0.001,
             repel         = TRUE) +
  theme_bw() + labs(color = 'Contribution')


# ------------------------------------------------------------------------------
# 10.c Regression tree — Total GPHD (simplified: HomeValue + HomeSize only)
# ------------------------------------------------------------------------------
# A two-predictor tree using HomeValue_USD and HomeSize_Sq.Ft is fit as a
# simple interpretable benchmark before the full stepwise-selected model below.

tree.water.flume2.simple <- tree(Average.Total.GPHD ~ .,
                                 data = flume2_total_stepwise_RF[, c("Average.Total.GPHD",
                                                                     "HomeValue_USD",
                                                                     "HomeSize_Sq.Ft")])
plot(tree.water.flume2.simple, col = "black")
text(tree.water.flume2.simple, pretty = 1, cex = 1, col = "black")
summary(tree.water.flume2.simple)


# ------------------------------------------------------------------------------
# 10.d Stepwise regression — prepare predictor matrices
# ------------------------------------------------------------------------------
# Factor variables are converted to ordinal numeric codes for stepwise
# regression compatibility. Level ordering reflects hypothesized direction
# of effect (Low = 1, High = 2; or frequency increasing left to right).
#
# Note: Leak_Level levels are reversed (High = 1, Low = 2) — higher leak
# level is coded as the lower numeric value. Verify this matches analysis intent.
#
# Columns dropped before stepwise:
#   Indoor  : cols 3:9 (IDs/dates), 17:86 (fixture metrics), 92 (redundant)
#   Outdoor : cols 3:9, 19:47 (fixture metrics), 88:91, 93, 95, 96
#   Total   : cols 3:9, 19:47 (fixture metrics)

# Indoor stepwise predictor matrix
flume2_indoor_stepwise <- flume2_household_indoor[, -c(3:9, 17:86, 92)]
rownames(flume2_indoor_stepwise) <- flume2_indoor_stepwise[, 1]
flume2_indoor_stepwise <- flume2_indoor_stepwise[, -1]

flume2_indoor_stepwise$HasPool             <- as.numeric(factor(flume2_indoor_stepwise$HasPool,             levels = c("FALSE", "TRUE")))
flume2_indoor_stepwise$Toilet_Efficiency   <- as.numeric(factor(flume2_indoor_stepwise$Toilet_Efficiency,   levels = c("Low", "High")))
flume2_indoor_stepwise$ShowerHead_Efficiency <- as.numeric(factor(flume2_indoor_stepwise$ShowerHead_Efficiency, levels = c("Low", "High")))
flume2_indoor_stepwise$Laundry_Efficiency  <- as.numeric(factor(flume2_indoor_stepwise$Laundry_Efficiency,  levels = c("Low", "High")))
flume2_indoor_stepwise$Dishwash_Efficiency <- as.numeric(factor(flume2_indoor_stepwise$Dishwash_Efficiency, levels = c("Low", "High")))
flume2_indoor_stepwise$Faucet_Efficiency   <- as.numeric(factor(flume2_indoor_stepwise$Faucet_Efficiency,   levels = c("Low", "High")))
flume2_indoor_stepwise$Leak_Level          <- as.numeric(factor(flume2_indoor_stepwise$Leak_Level,          levels = c("High", "Low")))  # reversed: High=1
flume2_indoor_stepwise$HOME_Efficiency     <- as.numeric(factor(flume2_indoor_stepwise$HOME_Efficiency,     levels = c("Low", "Mixed", "High")))
flume2_indoor_stepwise$INDOOR_Efficiency   <- as.numeric(factor(flume2_indoor_stepwise$INDOOR_Efficiency,   levels = c("Low", "Mixed", "High")))

# Outdoor stepwise predictor matrix
flume2_outdoor_stepwise <- flume2_household_outdoor[, -c(3:9, 19:47, 88:91, 93, 95, 96)]
rownames(flume2_outdoor_stepwise) <- flume2_outdoor_stepwise[, 1]
flume2_outdoor_stepwise <- flume2_outdoor_stepwise[, -1]

flume2_outdoor_stepwise$HasPool              <- as.numeric(factor(flume2_outdoor_stepwise$HasPool,              levels = c("FALSE", "TRUE")))
flume2_outdoor_stepwise$IrrigationFrequency  <- as.numeric(factor(flume2_outdoor_stepwise$IrrigationFrequency,  levels = c("1_PER_WEEK", "2_PER_WEEK", "3_PER_WEEK", "4+_PER_WEEK")))
flume2_outdoor_stepwise$IrrigationType       <- as.numeric(factor(flume2_outdoor_stepwise$IrrigationType,       levels = c("NONE", "GARDEN_HOSE", "DRIP", "SOAKER_HOSE", "SPRINKLER_SYSTEM")))
flume2_outdoor_stepwise$Leak_Level           <- as.numeric(factor(flume2_outdoor_stepwise$Leak_Level,           levels = c("High", "Low")))  # reversed: High=1
flume2_outdoor_stepwise$Irrigation_Efficiency <- as.numeric(factor(flume2_outdoor_stepwise$Irrigation_Efficiency, levels = c("Low", "High")))

# Total stepwise predictor matrix
flume2_total_stepwise <- flume2_household_total[, -c(3:9, 19:47)]
rownames(flume2_total_stepwise) <- flume2_total_stepwise[, 1]
flume2_total_stepwise <- flume2_total_stepwise[, -1]

flume2_total_stepwise$HasPool              <- as.numeric(factor(flume2_total_stepwise$HasPool,              levels = c("TRUE", "FALSE")))
flume2_total_stepwise$IrrigationFrequency  <- as.numeric(factor(flume2_total_stepwise$IrrigationFrequency,  levels = c("1_PER_WEEK", "2_PER_WEEK", "3_PER_WEEK", "4+_PER_WEEK")))
flume2_total_stepwise$IrrigationType       <- as.numeric(factor(flume2_total_stepwise$IrrigationType,       levels = c("NONE", "GARDEN_HOSE", "DRIP", "SOAKER_HOSE", "SPRINKLER_SYSTEM")))
flume2_total_stepwise$Leak_Level           <- as.numeric(factor(flume2_total_stepwise$Leak_Level,           levels = c("High", "Low")))  # reversed: High=1
flume2_total_stepwise$Irrigation_Efficiency  <- as.numeric(factor(flume2_total_stepwise$Irrigation_Efficiency,  levels = c("Low", "High")))
flume2_total_stepwise$Toilet_Efficiency    <- as.numeric(factor(flume2_total_stepwise$Toilet_Efficiency,    levels = c("Low", "High")))
flume2_total_stepwise$ShowerHead_Efficiency <- as.numeric(factor(flume2_total_stepwise$ShowerHead_Efficiency, levels = c("Low", "High")))
flume2_total_stepwise$Laundry_Efficiency   <- as.numeric(factor(flume2_total_stepwise$Laundry_Efficiency,   levels = c("Low", "High")))
flume2_total_stepwise$Dishwash_Efficiency  <- as.numeric(factor(flume2_total_stepwise$Dishwash_Efficiency,  levels = c("Low", "High")))
flume2_total_stepwise$Faucet_Efficiency    <- as.numeric(factor(flume2_total_stepwise$Faucet_Efficiency,    levels = c("Low", "High")))
flume2_total_stepwise$Leak_Level           <- as.numeric(factor(flume2_total_stepwise$Leak_Level,           levels = c("High", "Low")))  # reversed: High=1
flume2_total_stepwise$HOME_Efficiency      <- as.numeric(factor(flume2_total_stepwise$HOME_Efficiency,      levels = c("Low", "Mixed", "High")))
flume2_total_stepwise$INDOOR_Efficiency    <- as.numeric(factor(flume2_total_stepwise$INDOOR_Efficiency,    levels = c("Low", "Mixed", "High")))


# ------------------------------------------------------------------------------
# 10.e Stepwise regression — Total GPHD (forward, backward, both)
# ------------------------------------------------------------------------------
# Variables with >20% missing data are excluded prior to fitting.
# Three stepwise directions are run and results are combined into a single
# comparison table (stepwise_results) for inspection.
# AIC-based selection is used (default for step()).

# Check and visualize missing data pattern
VIM::aggr(flume2_total_stepwise)

# Remove columns with >20% NAs
stepwise_input <- flume2_total_stepwise %>% select(where(~ mean(is.na(.)) < 0.2))
VIM::aggr(stepwise_input)

# Remove rows with any remaining NAs
stepwise_complete <- na.exclude(stepwise_input)
VIM::aggr(stepwise_complete)

# Define null (intercept-only) and full models as stepwise bounds
intercept_only <- lm(Average.Total.GPHD ~ 1,  data = stepwise_complete)
full_model      <- lm(Average.Total.GPHD ~ .,  data = stepwise_complete)

# Forward stepwise (starts from intercept, adds predictors)
forward <- step(intercept_only, direction = 'forward', scope = formula(full_model), trace = 0)
forward$anova
forward$coefficients
stepwise_forward <- rownames_to_column(data.frame(forward$coefficients), "Variable")

# Backward stepwise (starts from full model, removes predictors)
backward <- step(full_model, direction = 'backward', scope = formula(full_model), trace = 0)
backward$anova
backward$coefficients
stepwise_backward <- rownames_to_column(data.frame(backward$coefficients), "Variable")

# Bidirectional stepwise (both additions and removals)
both <- step(intercept_only, direction = 'both', scope = formula(full_model), trace = 0)
both$anova
both$coefficients
stepwise_both <- rownames_to_column(data.frame(both$coefficients), "Variable")

# Combine all three stepwise results for comparison
stepwise_results <- stepwise_forward %>%
  full_join(stepwise_backward, by = "Variable") %>%
  full_join(stepwise_both,     by = "Variable")


# ------------------------------------------------------------------------------
# 10.f RF and tree on stepwise-selected variables
# ------------------------------------------------------------------------------
# Use the union of variables selected by any stepwise direction to build a
# focused RF model and train/test regression tree on the Flume 2 subset.

imp_variables <- c("Average.Total.GPHD", stepwise_results$Variable[-1])
flume2_total_stepwise_RF <- flume2_total_stepwise[, imp_variables]

# Random Forest on stepwise-selected variables
rforest.total.stepwise <- randomForest(Average.Total.GPHD ~ .,
                                       data = flume2_total_stepwise_RF,
                                       ntree = 300, na.action = na.exclude)
plot(rforest.total.stepwise, main = "Flume 2 Stepwise RF — error convergence")

rf_tot_stepwise <- rownames_to_column(data.frame(rforest.total.stepwise$importance), "Variables")
rf_tot_stepwise <- rf_tot_stepwise[order(rf_tot_stepwise$IncNodePurity, decreasing = TRUE), ]
plot_rf_importance(rforest.total.stepwise, title = "Flume 2 Stepwise — Variable Importance")

# PCA on stepwise-selected predictors (response excluded)
water_total_sw <- flume2_total_stepwise[, imp_variables[-1]]
water_total_sw <- na.exclude(water_total_sw)

pca_total_sw <- princomp(water_total_sw)
print(pca_total_sw)
plot(pca_total_sw, type = "l")   # Scree plot
summary(pca_total_sw)
pca_loadings_sw <- data.frame(pca_total_sw$loadings[, 1:4])
pca_loadings_sw

fviz_pca_var(pca_total_sw,
             col.var       = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title         = "Flume 2 Stepwise Total GPHD PCA",
             arrowsize     = 0.001,
             repel         = TRUE) +
  theme_bw() + labs(color = 'Contribution')

# Regression tree: train/test split (80/20) on stepwise-selected variables
set.seed(42)
water_total_sw2 <- flume2_total_stepwise_RF[, imp_variables]
ind         <- sample(2, nrow(water_total_sw2), replace = TRUE, prob = c(0.80, 0.20))
train_water <- water_total_sw2[ind == 1, ]
test_water  <- water_total_sw2[ind == 2, ]

tree_train <- tree(Average.Total.GPHD ~ ., data = train_water)
plot(tree_train, col = "black")
text(tree_train, pretty = 1, cex = 1, col = "black")
summary(tree_train)

testing_water <- data.frame(predict(tree_train, test_water))
df2 <- merge(testing_water, test_water[, 1:2], by = 0, all = TRUE)
df2 <- df2[, -4]  # drop duplicate row-name column from merge
colnames(df2) <- c("S.no", "tree.classified.TotalGPHD", "observed.TotalGPHD")

cor.model2 <- cor(df2[, 2], df2[, 3], use = "pairwise.complete.obs")
cor.model2

lm.model2 <- lm(df2[, 2] ~ df2[, 3])
summary(lm.model2)

# Empirical CDF: observed vs. tree-predicted Total GPHD
# png("eCDF_plot.png", units="in", width=5, height=5, res=300)
plot(ecdf(df2$observed.TotalGPHD), verticals = TRUE, do.points = FALSE,
     col = "black", lty = 1, lwd = 2,
     xlab = "Total GPHD", ylab = "F(Total GPHD)", main = "")
plot(ecdf(df2$tree.classified.TotalGPHD), verticals = TRUE, lty = 2, lwd = 2,
     do.points = FALSE, add = TRUE, col = "red")
legend("bottomright", legend = c("Observed", "Tree classified"),
       col = c("black", "red"), lty = 1:2, cex = 0.75, box.lty = 0)
# dev.off()


# ==============================================================================
# SECTION 11: FLUME 2 — NORMALITY TESTS, SIGNIFICANCE TESTING & EDA PLOTS
# ==============================================================================

# ------------------------------------------------------------------------------
# 11.a Normality assessment — QQ plots and Shapiro-Wilk tests
# ------------------------------------------------------------------------------
# Water use distributions are right-skewed; log transformation is assessed.
# Shapiro-Wilk: H0 = normal distribution (reject if p < 0.05).
# QQ plots shown for raw and log-transformed indoor, outdoor, and total GPHD.
# Reference: https://statsandr.com/blog/do-my-data-follow-a-normal-distribution

a1 <- ggqqplot(flume2_household_total$Average.Total.GPHD,          title = "Avg. Total GPHD")
a2 <- ggqqplot(log(flume2_household_total$Average.Total.GPHD),     title = "Log(Avg. Total GPHD)")
a3 <- ggqqplot(flume2_household_indoor$Average.Indoor.GPHD,        title = "Avg. Indoor GPHD")
a4 <- ggqqplot(log(flume2_household_indoor$Average.Indoor.GPHD),   title = "Log(Avg. Indoor GPHD)")
a5 <- ggqqplot(flume2_household_outdoor$Average.Outdoor.GPHD,      title = "Avg. Outdoor GPHD")
a6 <- ggqqplot(log(flume2_household_outdoor$Average.Outdoor.GPHD), title = "Log(Avg. Outdoor GPHD)")
grid.arrange(a1, a2, a3, a4, a5, a6, ncol = 2, nrow = 3, top = "Normality Assessment — Raw vs. Log")

# Shapiro-Wilk tests (p < 0.05 = non-normal; log transformation normalizes the data)
shapiro.test(flume2_household_total$Average.Total.GPHD)
shapiro.test(log(flume2_household_total$Average.Total.GPHD))
shapiro.test(flume2_household_indoor$Average.Indoor.GPHD)
shapiro.test(log(flume2_household_indoor$Average.Indoor.GPHD))
shapiro.test(flume2_household_outdoor$Average.Outdoor.GPHD)
shapiro.test(log(flume2_household_outdoor$Average.Outdoor.GPHD))


# ------------------------------------------------------------------------------
# 11.b EDA boxplots — water use by household classification
# ------------------------------------------------------------------------------
# Boxplots visualize distributional differences across irrigation type/frequency,
# pool status, and efficiency classifications. Draft-quality plots (cc*, dd*)
# use filled colors and larger axis labels for publication figures.

# Irrigation frequency — total, indoor, outdoor GPHD
a1 <- ggplot(flume2_household_wateruse, aes(x = IrrigationFrequency, y = Average.Total.GPHD,   group = IrrigationFrequency)) + geom_boxplot() + theme_classic2() + ylim(0, 2000)
a2 <- ggplot(flume2_household_indoor,   aes(x = IrrigationFrequency, y = Average.Indoor.GPHD,  group = IrrigationFrequency)) + geom_boxplot() + theme_classic2() + ylim(0, 500)
a3 <- ggplot(flume2_household_outdoor,  aes(x = IrrigationFrequency, y = Average.Outdoor.GPHD, group = IrrigationFrequency)) + geom_boxplot() + theme_classic2() + ylim(0, 2500)
grid.arrange(a1, a2, a3, ncol = 3)

# Irrigation type — total, indoor, outdoor GPHD
b1 <- ggplot(flume2_household_wateruse, aes(x = IrrigationType, y = Average.Total.GPHD,   group = IrrigationType)) + geom_boxplot() + theme_classic2() + ylim(0, 2000)
b2 <- ggplot(flume2_household_indoor,   aes(x = IrrigationType, y = Average.Indoor.GPHD,  group = IrrigationType)) + geom_boxplot() + theme_classic2() + ylim(0, 500)
b3 <- ggplot(flume2_household_outdoor,  aes(x = IrrigationType, y = Average.Outdoor.GPHD, group = IrrigationType)) + geom_boxplot() + theme_classic2() + ylim(0, 2500)
grid.arrange(b1, b2, b3, ncol = 3)

# Pool status — exploratory (GPHD) and draft-quality (GPCD, colored fill)
c1 <- ggplot(flume2_household_wateruse, aes(x = HasPool, y = Average.Total.GPHD,   group = HasPool)) + geom_boxplot() + theme_classic2() + ylim(0, 2000)
c2 <- ggplot(flume2_household_indoor,   aes(x = HasPool, y = Average.Indoor.GPHD,  group = HasPool)) + geom_boxplot() + theme_classic2() + ylim(0, 1000)
c3 <- ggplot(flume2_household_outdoor,  aes(x = HasPool, y = Average.Outdoor.GPHD, group = HasPool)) + geom_boxplot() + theme_classic2() + ylim(0, 2000)
grid.arrange(c1, c2, c3, ncol = 3)

pub_theme <- theme(axis.title = element_text(color = "black", size = 13, face = "bold"),
                   axis.text  = element_text(color = "black", size = 12))

cc1 <- ggplot(flume2_household_wateruse, aes(x = HasPool, y = Average.Total.GPCD,   fill = HasPool)) +
  scale_fill_manual(values = c("green", "red")) + guides(fill = "none") +
  geom_boxplot() + theme_classic2() + ylim(0, 500) +
  xlab("Has Pool") + ylab("Avg. Total Water Use\n[Gallons per person per day]") + pub_theme

cc2 <- ggplot(flume2_household_wateruse, aes(x = HasPool, y = Average.Indoor.GPCD,  fill = HasPool)) +
  scale_fill_manual(values = c("green", "red")) + guides(fill = "none") +
  geom_boxplot() + theme_classic2() + ylim(0, 200) +
  xlab("Has Pool") + ylab("Avg. Indoor Water Use\n[Gallons per person per day]") + pub_theme

cc3 <- ggplot(flume2_household_wateruse, aes(x = HasPool, y = Average.Outdoor.GPCD, fill = HasPool)) +
  scale_fill_manual(values = c("green", "red")) + guides(fill = "none") +
  geom_boxplot() + theme_classic2() + ylim(0, 500) +
  xlab("Has Pool") + ylab("Avg. Outdoor Water Use\n[Gallons per person per day]") + pub_theme

grid.arrange(cc2, cc3, cc1, ncol = 3)

# Efficiency classifications — log-transformed GPHD (exploratory)
d1 <- ggplot(flume2_household_total,   aes(x = HOME_Efficiency,       y = log(Average.Total.GPHD),   group = HOME_Efficiency))       + geom_boxplot() + theme_classic2() + ylim(3, 9)
d2 <- ggplot(flume2_household_indoor,  aes(x = INDOOR_Efficiency,     y = log(Average.Indoor.GPHD),  group = INDOOR_Efficiency))     + geom_boxplot() + theme_classic2() + ylim(3, 9)
d3 <- ggplot(flume2_household_outdoor, aes(x = Irrigation_Efficiency, y = Outdoor_Gal.per.Sq.ft,     group = Irrigation_Efficiency)) + geom_boxplot() + theme_classic2() + ylim(0, 50) + ylab("Avg. annual outdoor water use [Gal/Sq.ft]")
grid.arrange(d1, d2, d3, ncol = 3)

# Efficiency classifications — draft-quality (GPCD, colored fill)
dd2 <- ggplot(flume2_household_wateruse, aes(x = INDOOR_Efficiency, y = Average.Indoor.GPCD, fill = INDOOR_Efficiency)) +
  scale_fill_manual(values = c("red", "green")) + guides(fill = "none") +
  geom_boxplot() + theme_classic2() + ylim(0, 150) +
  xlab("Household Efficiency") + ylab("Avg. Indoor Water Use\n[Gallons per person per day]") + pub_theme

dd3 <- ggplot(flume2_household_outdoor, aes(x = INDOOR_Efficiency, y = Outdoor_Gal.per.Sq.ft, fill = INDOOR_Efficiency)) +
  scale_fill_manual(values = c("red", "green")) + guides(fill = "none") +
  geom_boxplot() + theme_classic2() + ylim(0, 50) +
  xlab("Household Efficiency") + ylab("Avg. Outdoor Water Use\n[Gallons per sq.ft.]") + pub_theme

dd4 <- ggplot(flume2_household_outdoor, aes(x = Irrigation_Efficiency, y = Outdoor_Gal.per.Sq.ft, group = Irrigation_Efficiency)) +
  geom_boxplot() + theme_classic2() + ylim(0, 50) +
  xlab("Irrigation Efficiency") + ylab("Avg. Outdoor Water Use\n[Gal/Sq.ft]")

grid.arrange(dd2, dd3, ncol = 2)


# ------------------------------------------------------------------------------
# 11.c Non-parametric significance tests — Kruskal-Wallis and Dunn post-hoc
# ------------------------------------------------------------------------------
# Kruskal-Wallis tests are used in place of ANOVA due to non-normal distributions
# (confirmed in 11.a). Dunn tests with Holm correction are applied for pairwise
# comparisons when Kruskal-Wallis is significant.
# Reference: https://statsandr.com/blog/kruskal-wallis-test-nonparametric-version-anova/
#
# Tests are grouped by grouping variable: HasPool, HOME/INDOOR/Irrigation efficiency,
# IrrigationType, and IrrigationFrequency across indoor, outdoor, and total use.

# HasPool
kruskal.test(Average.Total.GPHD   ~ HasPool, data = flume2_household_total)
kruskal.test(Average.Indoor.GPHD  ~ HasPool, data = flume2_household_indoor)
kruskal.test(Average.Outdoor.GPHD ~ HasPool, data = flume2_household_outdoor)

# Efficiency classifications
kruskal.test(Median.Total.GPHD      ~ HOME_Efficiency,       data = flume2_household_wateruse)
kruskal.test(Average.Total.GPHD     ~ HOME_Efficiency,       data = flume2_household_total)
kruskal.test(Average.Indoor.GPHD    ~ INDOOR_Efficiency,     data = flume2_household_indoor)
kruskal.test(Average.Indoor.GPCD    ~ INDOOR_Efficiency,     data = flume2_household_indoor)
kruskal.test(Outdoor_Gal.per.Sq.ft  ~ INDOOR_Efficiency,     data = flume2_household_outdoor)
kruskal.test(Average.Outdoor.GPHD   ~ Irrigation_Efficiency, data = flume2_household_wateruse)
kruskal.test(Average.Outdoor.GPHD   ~ Irrigation_Efficiency, data = flume2_household_outdoor)

# Dunn post-hoc tests (Holm correction) — efficiency classifications
dunnTest(Average.Indoor.GPCD    ~ INDOOR_Efficiency, data = flume2_household_indoor,  method = "holm")
dunnTest(Average.Indoor.GPHD    ~ INDOOR_Efficiency, data = flume2_household_indoor,  method = "holm")
dunnTest(Average.Total.GPHD     ~ HOME_Efficiency,   data = flume2_household_total,   method = "holm")

# IrrigationType and IrrigationFrequency
kruskal.test(Average.Total.GPHD   ~ IrrigationType,       data = flume2_household_total)
kruskal.test(Average.Total.GPHD   ~ IrrigationFrequency,  data = flume2_household_total)
kruskal.test(Average.Total.GPHD   ~ HasPool,              data = flume2_household_total)
kruskal.test(Average.Indoor.GPHD  ~ IrrigationType,       data = flume2_household_indoor)
kruskal.test(Average.Indoor.GPHD  ~ IrrigationFrequency,  data = flume2_household_indoor)
kruskal.test(Average.Indoor.GPCD  ~ HasPool,              data = flume2_household_indoor)
kruskal.test(Average.Outdoor.GPHD ~ IrrigationType,       data = flume2_household_outdoor)
kruskal.test(Average.Outdoor.GPHD ~ IrrigationFrequency,  data = flume2_household_outdoor)
kruskal.test(Average.Outdoor.GPCD ~ HasPool,              data = flume2_household_outdoor)

total.irritype  <- dunnTest(Average.Total.GPHD ~ IrrigationType,      data = flume2_household_total,   method = "holm")
total.irrifreq  <- dunnTest(Average.Total.GPHD ~ IrrigationFrequency, data = flume2_household_total,   method = "holm")
indoor.irritype <- dunnTest(Average.Indoor.GPHD ~ IrrigationType,     data = flume2_household_indoor,  method = "holm")
indoor.irrifreq <- dunnTest(Average.Indoor.GPHD ~ IrrigationFrequency,data = flume2_household_indoor,  method = "holm")
outdoor.irritype <- dunnTest(Average.Outdoor.GPHD ~ IrrigationType,   data = flume2_household_outdoor, method = "holm")
outdoor.irrifreq <- dunnTest(Average.Outdoor.GPHD ~ IrrigationFrequency, data = flume2_household_outdoor, method = "holm")

# Compile pairwise Dunn test results into summary tables
signif.irritype <- data.frame(
  classification  = indoor.irritype$res$Comparison,
  Indoor.P.adj    = round(indoor.irritype$res$P.adj,  5),
  Outdoor.P.adj   = round(outdoor.irritype$res$P.adj, 5),
  Total.P.adj     = round(total.irritype$res$P.adj,   5)
)
signif.irrifreq <- data.frame(
  classification  = indoor.irrifreq$res$Comparison,
  Indoor.P.adj    = round(indoor.irrifreq$res$P.adj,  5),
  Outdoor.P.adj   = round(outdoor.irrifreq$res$P.adj, 5),
  Total.P.adj     = round(total.irrifreq$res$P.adj,   5)
)


# ------------------------------------------------------------------------------
# 11.d ANOVA with log transformation — IrrigationType, IrrigationFrequency, HasPool
# ------------------------------------------------------------------------------
# One-way ANOVA on log-transformed GPHD confirms results from Kruskal-Wallis
# under approximate normality after transformation. TukeyHSD provides pairwise
# comparisons.

# Total GPHD
a <- aov(log(Average.Total.GPHD) ~ IrrigationType,      data = flume2_household_total);  summary(a); TukeyHSD(a)
b <- aov(log(Average.Total.GPHD) ~ IrrigationFrequency, data = flume2_household_total);  summary(b); TukeyHSD(b)
summary(aov(log(Average.Total.GPHD) ~ HasPool,           data = flume2_household_total))

# Indoor GPHD
c <- aov(log(Average.Indoor.GPHD) ~ IrrigationType,      data = flume2_household_indoor); summary(c); TukeyHSD(c)
d <- aov(log(Average.Indoor.GPHD) ~ IrrigationFrequency, data = flume2_household_indoor); summary(d); TukeyHSD(d)
summary(aov(log(Average.Indoor.GPHD) ~ HasPool,           data = flume2_household_indoor))

# Outdoor GPHD
e <- aov(log(Average.Outdoor.GPHD) ~ IrrigationType,      data = flume2_household_outdoor); summary(e); TukeyHSD(e)
f <- aov(log(Average.Outdoor.GPHD) ~ IrrigationFrequency, data = flume2_household_outdoor); summary(f); TukeyHSD(f)
summary(aov(log(Average.Outdoor.GPHD) ~ HasPool,           data = flume2_household_outdoor))


# ------------------------------------------------------------------------------
# 11.e Grouped descriptive statistics
# ------------------------------------------------------------------------------
# describeBy() from the psych package produces grouped summary statistics
# (mean, median, sd, min, max, skew, etc.) for water use variables stratified
# by irrigation frequency, efficiency classification, and occupancy.

# Total use (GPHD + GPCD) by irrigation frequency
stats.by.irrifreq <- describeBy(flume2_household_wateruse[, c(2, 4, 6, 8, 10, 12)],
                                flume2_household_wateruse$IrrigationFrequency, mat = TRUE)

# Home value by home efficiency classification
stats.homevalue.by.efficiency <- describeBy(flume2_household_wateruse$HomeValue_USD,
                                            flume2_household_wateruse$HOME_Efficiency, mat = TRUE)

# Indoor + outdoor + total GPHD by indoor efficiency
stats.by.indoor.efficiency <- describeBy(flume2_household_wateruse[, c(2, 6, 10)],
                                         flume2_household_wateruse$INDOOR_Efficiency, mat = TRUE)

# Indoor GPCD by indoor efficiency
stats.indoorGPCD.by.efficiency <- describeBy(flume2_household_indoor$Average.Indoor.GPCD,
                                             flume2_household_indoor$INDOOR_Efficiency, mat = TRUE)
