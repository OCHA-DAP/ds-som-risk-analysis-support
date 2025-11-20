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


## TBD

- Changes to ACLED API have broken some of original scripts - these could be
retrofit to new authentication method as done in updated `analysis/01c_Conflict_ACLED__slides_update2026_HNRP.Rmd`
but we would need to make sure no results change.
