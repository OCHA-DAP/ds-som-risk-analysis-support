# Somalia Risk Analysis Support

## Overview

This repository contains analytical workflows supporting the humanitarian risk analysis for Somalia's Humanitarian Needs Overview (HNO) and Humanitarian Response Plan (HNRP) reporting.

## Background

### Timeline of Risk Analysis Support

1. **Late 2022**: An in-country risk analysis workshop was held, with results included in the [2023 Somalia HNO Report](https://www.humanitarianresponse.info/en/operations/somalia/document/somalia-humanitarian-needs-overview-2023) (p. 50-57)

2. **Late 2023**: The Centre for Humanitarian Data (CHD) provided supporting analysis to help update the risk analysis section for the [2024 Somalia HNRP Report](https://www.humanitarianresponse.info/en/operations/somalia/document/somalia-humanitarian-response-plan-2024) (p. 19-20)

3. **Late 2024**: CHD conducted a light risk analysis to inform risk analysis discussions for the [2025 HNRP report](https://www.humanitarianresponse.info/en/operations/somalia/document/somalia-humanitarian-response-plan-2025)

4. **November 2025**: CHD is providing analysis to inform discussions around the risk analysis assessment in the **2026 HNRP report**

## Risk Analysis Themes

The current analysis focuses on the following themes:

- **Conflict** - Analysis of armed conflict events and trends
- **Cholera** - Disease outbreak monitoring and risk assessment (** no cholera data received for 2026 update)
- **Flooding** - Flood exposure and impact analysis
- **Drought** - Drought risk assessment and monitoring


## Key updated scripts (2025)

```
analysis/01c_Conflict_ACLED__slides_update2026_HNRP.Rmd
analysis/06_seas5_comparison.R
```

## Repository Structure

```
analysis/        # Main analysis scripts and reports
exploration/     # Exploratory analysis notebooks
data-raw/        # scripts that produce raw data inputs
data/            $ minimal local data used in development (gitignored)
R/               # reusable functions
_targets.R       # r `{targets}` pipeline used for seasonal flood exposure
```



## Key Analyses

- Conflict analysis using ACLED data
- Cholera outbreak analysis - only done in 2023 for 2024 HNRP with limited data
no new data received since.
- Drought monitoring using SEAS5 forecasts
- Flood exposure assessment in 2023 for 2024 HNRP
- Flood outlook implications from seasonal SEAS5 forecasts.
- Seasonal forecast comparisons

## Developer notes

### python not needed

- python was only used to download IRI forecast for experimental purposes in 
the beginning of project (data-raw/IRI_prob.py).
- python dependencies can be ignored as of latest version of repo.

### Targets setup

The [`{targets}`](https://github.com/ropensci/targets) package is used to manage the flood exposure analysis workflow in [_targets.R](_targets.R). Targets creates a pipeline that tracks dependencies between data processing steps, automatically re-running only the parts that need updating when inputs change.

**To run the flood analysis pipeline:**

```r
# Install targets if needed
install.packages("targets")

# View the pipeline structure
targets::tar_visnetwork()

# Run the entire pipeline
targets::tar_make()

# Load specific results
targets::tar_load(gt_adm1_range_table)  # Load Admin 1 table
targets::tar_load(df_adm2_exposure_ranges)  # Load Admin 2 exposure data
```

The pipeline processes FloodScan data, WorldPop estimates, and OCHA population data to calculate flood exposure statistics at Admin 1 and Admin 2 levels. See [R/tar_flood_exposure.R](R/tar_flood_exposure.R) for the custom functions used in the pipeline.



## TBD

- Changes to ACLED API have broken some of original scripts - these could be
retrofit to new authentication method as done in updated `analysis/01c_Conflict_ACLED__slides_update2026_HNRP.Rmd`
but we would need to make sure no results change.
