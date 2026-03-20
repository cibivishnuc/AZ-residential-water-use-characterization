# Arizona Residential Water Use Characterization
Exploratory analysis of residential water use across Arizona using Flume smart meter data, Water Year 2022

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![Language: R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)

---

## Overview

This repository contains the R analysis code for a study of residential indoor and outdoor water use across Arizona using data from Flume smart water meters. The analysis covers **Water Year 2022 (October 1, 2021 – September 30, 2022)** and integrates household-level water use data with building characteristics, demographic information, and daily weather observations from the [Arizona Meteorological Network (AZMET)](https://azmet.arizona.edu/).

The study examines what household, demographic, and climate factors drive indoor and outdoor residential water use, with a particular focus on households equipped with Flume 2 devices, which provide fixture-level disaggregation data (toilet, shower, faucet, laundry, etc.). Fixture efficiency classifications are derived within the script using unsupervised clustering (PAM) on per-fixture water use metrics.

---

## Repository Structure

```
├── AZ_data_analysis-characterization.R                 # Main analysis script (self-contained)
├── data/
│   ├── AZ_Households.xlsx                            # Household demographics and building characteristics
│   ├── AZ Water Use.csv                              # Daily indoor/outdoor water use per household (GPHD)
│   ├── AZ_FLUME_Zipcode_AZMET-stations.csv           # Zip code to nearest AZMET station mapping
│   ├── AZ_Daily_Weather_Precip_ET_Temp_2021-22.csv   # Daily weather from AZMET stations
│   └── az disag.xlsx                                 # Flume 2 fixture disaggregation data
└── README.md
```

---

## Data Description

| File | Description | Key Variables |
|---|---|---|
| `AZ_Households.xlsx` | 1,258 household records with building and demographic info | `Location_ID`, `City`, `County`, `PostalCode`, `YearBuilt`, `HomeSize_Sq.Ft`, `LotSize_Sq.Ft`, `HomeValue_USD`, `NumResidents`, `NumBathrooms`, `HasPool`, `IrrigationType`, `IrrigationFrequency` |
| `AZ Water Use.csv` | Daily indoor and outdoor water use in long format | `Location_ID`, `Date`, `Measure.Names` (Indoor/Outdoor GPHD), `Measure.Values` |
| `AZ_FLUME_Zipcode_AZMET-stations.csv` | Maps each zip code to its nearest AZMET weather station by distance | `PostalCode`, `AZMET_FID`, `Station_name`, `Station_distance_to_zipcode(KM)` |
| `AZ_Daily_Weather_Precip_ET_Temp_2021-22.csv` | Daily precipitation (in), Penman ET (in), and max air temperature (°F) per AZMET station | `Date`, `AZMET_Station`, `AZMET_FID`, `DAY_Precip`, `DAY_Penman`, `AirTempMax.DegF.` |
| `az disag.xlsx` | Flume 2 fixture-level average volume (gal), duration (min), and flow rate (GPM) | `Location_ID`, `Fixture`, `Avg. Volume (gal)`, `Avg. Duration (min)`, `Avg. Flow Rate (GPM)` |

**Units:**
- **GPHD** — Gallons Per Household per Day
- **GPCD** — Gallons Per Capita per Day
- **ET** — Evapotranspiration (Penman method), inches
- **Water Year** — October 1 through September 30

**Known data notes:**
- `Location_ID = 16003` (City = "Tbd", PostalCode = 66666) is excluded throughout the analysis due to missing geographic information.
- A small number of raw GPHD records have negative values (meter artifacts); these are set to zero before analysis.
- `NumResidents` values differ between `AZ_Households.xlsx` and `az disag.xlsx` for Flume 2 households. The disaggregation file value is used as the authoritative source for all Flume 2 analyses.

---

## Analysis Pipeline

This is **script 3** in a larger analysis pipeline. It is fully self-contained — all processing steps including fixture efficiency classification run within a single script.

Sections 7–11 focus exclusively on households equipped with the Flume 2 device — the newer generation of Flume smart meter available as of 2022. Compared to Flume 1, the Flume 2 offers improved metering accuracy and provides fixture-level disaggregation of water use (toilet, showerhead, irrigation, faucet, etc.), enabling analysis beyond whole-home totals. Sections 1–6 analyze daily indoor and outdoor water use aggregated across all Flume-monitored households regardless of device version; Sections 7–11 restrict to the Flume 2 subset to take advantage of this higher-resolution data, classifying individual appliance efficiency and linking fixture-level behavior to household characteristics, climate signals, and overall consumption patterns.

### Section Map

| Section | Description |
|---|---|
| **1** | Load and clean household characteristics; fill missing county names; map AZMET stations; group households by city, zip, county, and weather station |
| **2** | Load and reshape daily water use; join weather data; compute GPCD; build `wateruse_wide` master daily dataset |
| **3** | Indoor water use analysis (all AZ): filtering, daily/household summaries, MLR models (GPHD and GPCD) by county |
| **4** | Outdoor water use analysis (all AZ): filtering, summaries, MLR models stratified by pool status |
| **5** | Total water use analysis (all AZ): summaries, MLR models, extended models with monthly weather predictors |
| **6** | Variable importance (Random Forest), PCA, and regression trees for all-AZ households |
| **7** | Flume 2 data ingestion, fixture efficiency classification, and household/daily water use dataset construction
| **8** | Flume 2 time series visualizations: daily median use, weekly water use vs. climate dual-axis panels |
| **9** | Flume 2 MLR models: daily/weekly weather-driven and household-level regression |
| **10** | Flume 2 RF variable importance, PCA, stepwise regression (forward/backward/both), and regression tree with train/test validation |
| **11** | Normality tests, EDA boxplots, Kruskal-Wallis and Dunn post-hoc tests, ANOVA after log transformation, grouped descriptive statistics |

---

## Methods Summary

### Water Use Filtering
Daily records are filtered to remove near-zero readings before computing household-level averages:
- Indoor GPHD ≥ 10 gallons/day
- Outdoor GPHD ≥ 10 gallons/day
- Total GPHD ≥ 20 gallons/day

For Flume 2 analyses, an additional criterion of **≥ 90 active device days** is applied to exclude households with very short monitoring periods.

### Fixture Efficiency Classification (Section 7.b.ii)
Each Flume 2 household's fixtures are classified as Low or High efficiency using **Partitioning Around Medoids (PAM)** clustering on per-fixture average water use metrics. The optimal number of clusters (k=2 for most fixtures, k=4 for landscape size) is selected via the silhouette method. Both k-means and PAM are fit and compared; PAM is used as the authoritative classification due to its robustness to outliers and deterministic behavior across runs.

Fixtures classified: toilet (gal/flush), showerhead (GPM), clothes washer (gal/cycle), dishwasher (gal/cycle), irrigation (gal/event and GPM), faucet (GPM), leak (gal/event), home value (USD), and landscape size (sq.ft).

### Multiple Linear Regression (MLR)
MLR models are run separately for indoor, outdoor, and total GPHD and GPCD. Models are stratified by:
- All Arizona households
- Maricopa County, Pima County, and all other counties (indoor analysis)
- Pool vs. no-pool households (outdoor and total analysis)

Extended models incorporate monthly precipitation, ET, and max temperature from each household's nearest AZMET station as additional predictors.

### Machine Learning
**Random Forest** models rank predictors by node impurity (IncNodePurity) for indoor, outdoor, and total use. Results guide predictor selection for stepwise regression.

**Stepwise regression** (AIC-based, forward / backward / bidirectional) is applied to the Flume 2 total GPHD dataset after removing predictors with >20% missing values.

### Multivariate Analysis
**PCA** (via `princomp()`) is run on household-level predictor matrices. Factor variables are converted to numeric prior to analysis; complete cases only are used.

### Statistical Testing
Given right-skewed water use distributions (confirmed by Shapiro-Wilk tests), non-parametric tests are used:
- **Kruskal-Wallis** for group comparisons (HasPool, efficiency classifications, irrigation type/frequency)
- **Dunn test** (Holm correction) for pairwise post-hoc comparisons
- **One-way ANOVA** on log-transformed data as a confirmatory approach

---

## Requirements

### R Version
R ≥ 4.0.0 recommended.

### Required Packages

```r
# Data import & manipulation
readxl, lubridate, dplyr, tidyr, tibble

# Summary statistics
psych

# Visualization
ggplot2, ggpubr, gridExtra, patchwork, corrplot

# Time series
zoo

# Clustering (Section 7b)
cluster, factoextra

# Machine learning & multivariate analysis
randomForest, tree

# Missing data
VIM

# Statistical tests
FSA
```

Install all required packages at once:

```r
install.packages(c(
  "readxl", "lubridate", "dplyr", "tidyr", "tibble",
  "psych", "ggplot2", "ggpubr", "gridExtra", "patchwork", "corrplot",
  "zoo", "cluster", "factoextra", "randomForest", "tree", "VIM", "FSA"
))
```

---

## Getting Started

1. Clone or download this repository.
2. Place all input data files in a single folder.
3. Open `3_explore_AZ_data.R` and update the `setwd()` path at the top of the script:

```r
setwd("path/to/your/data/folder")  # <-- update before running
```

4. Run the script section by section. Each section is self-contained after its upstream dependencies have been executed. It is not recommended to source the entire script at once, as several sections produce interactive plots and exploratory model outputs intended for step-by-step inspection.

5. Sections 1–6 (all-Arizona analyses) can be run independently. Sections 7–11 (Flume 2 analyses) require Sections 1–5 to have been run first — specifically the objects `wateruse_wide`, `monthly_ET`, `monthly_precip`, and `monthly_TempMax` must exist in the environment.

---

## Key Outputs

The script produces the following files when the relevant `write.csv()` lines are uncommented:

| File | Section | Contents |
|---|---|---|
| `AZ_FLUME_household_zipcodes.csv` | 1 | Completed zip code to county name lookup |
| `AZ_Wateruse_Wide.csv` | 2 | Master daily water use dataset with weather and household info joined |
| `AZ_daily_indoorGPHD.csv` | 3 | Wide-format daily indoor GPHD (households as columns) |
| `AZ_daily_indoorGPCD.csv` | 3 | Wide-format daily indoor GPCD |
| `AZ_daily_outdoorGPHD.csv` | 4 | Wide-format daily outdoor GPHD |
| `AZ_daily_TotalGPHD.csv` | 5 | Wide-format daily total GPHD |
| `AZ_daily_TotalGPCD.csv` | 5 | Wide-format daily total GPCD |
| `Flume2_AZ_Households_Info.csv` | 7 | Flume 2 household info before efficiency classification |
| `Appliance_Silhouette_scores.png` | 7b | Combined silhouette score plot for all fixtures |
| `Flume2_AZ_Household_DailyWaterUse_RAW.csv` | 7.d | Flume 2 raw daily water use per household |
| `Flume2_AZ_Household_Filtered_Avg_Wateruse.csv` | 7.f | Flume 2 per-household filtered average water use |
| `Flume2_AZ_Daily_Avg_WaterUse.csv` | 7.h | Flume 2 daily mean/median water use across all households |
| `Flume2_AZ_Household_DailyWateruse_Info_RAW.csv` | 7.i | Daily household-level dataset with efficiency and weather info |
| `Flume2_Weekly_Avg_Wateruse.csv` | 8 | Weekly mean water use and climate variables |

---

## Citation

If you use this code or data in your work, please cite:

> Chinnasamy, C.V. (2023). *Arizona Residential Water Use Analysis: Flume Smart Meter Study*. Colorado State University. GitHub. [repository URL]

This work is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

---

## Contact

For questions about the data or analysis, please open an issue in this repository or contact Cibi Vishnu Chinnasamy at cibivishnuc@gmail.com.
