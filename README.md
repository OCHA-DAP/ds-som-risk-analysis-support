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

**FloodScan now comes from blob, not the shared drive.** The pipeline originally
read a static NetCDF snapshot from `AA_DATA_DIR`
(`aer_sfed_area_300s_19980112_20221231_v05r01.nc`). That file ends 2022-12-31,
which is why the seasonal analysis stopped at OND 2022: no year filter was ever
applied, the record simply ran out, and no newer snapshot was downloaded to the
shared drive.

It now reads the daily FloodScan COGs that
[`ds-floodscan-ingest`](https://github.com/OCHA-DAP/ds-floodscan-ingest) keeps
current on blob:

```
prod / container `raster`
  floodscan/daily/v5/processed/aer_area_300s_v<YYYY-MM-DD>_v05r01.tif
```

Requires `DSCI_AZ_BLOB_PROD_SAS` (the same variable `ocha_stratus` uses). See
[R/utils_floodscan_blob.R](R/utils_floodscan_blob.R).

Notes on the switch:

- The `fs_latest_date` target is checked on every run, so new FloodScan data is
  picked up automatically. Downstream targets only rebuild when a new
  **complete** MAM or OND season becomes available. A part-finished season is
  excluded rather than compared against complete ones.
- Each seasonal maximum is cached as a small GeoTIFF under
  `data/floodscan_seasonal_max/` (gitignored), so only new seasons are
  downloaded on later runs. The first run fetches ~5,200 daily COGs and takes
  roughly 20 minutes.
- The blob route reproduces the NetCDF values exactly (verified cell-for-cell on
  MAM 2022: max abs diff 0 across all 20,500 cells).
- **Georeferencing was deliberately corrected, not preserved.** The old
  `fs_to_raster()` used cell centres as the extent corners, giving a grid
  resolution of 0.08267 rather than the true 0.08333: a half-cell offset. The
  blob route uses true cell edges. Consequence: pre-2023 exposure figures shift
  slightly against the numbers published for the 2024 HNRP. This was reviewed
  and the correct grid was chosen over bit-identical continuity, since matching
  the old output would mean reproducing a known error. Anyone reconciling
  against the 2024 table should expect small differences on the historical
  seasons and not treat them as a change in risk.
- The legacy snapshot is still tracked as `fp_fs_legacy`. Pass it instead of
  `fs_latest_date` to the exposure targets to reproduce the earlier results.

**WorldPop now comes from blob too.** The population raster was
`som_ppp_2020_1km_Aggregated_UNadj.tif` on the shared drive, so flood years
through 2026 were being multiplied against a 2020 population. `fp_wp` now clips
the current global count raster on blob:

```
dev / container `raster`
  worldpop/pop_count/global_pop_2026_CN_1km_R2025A_UA_v1.tif
```

Requires `DSCI_AZ_BLOB_DEV_SAS`. See
[R/utils_worldpop_blob.R](R/utils_worldpop_blob.R). Notes:

- It is a COG, so only the Somalia window is read, not the 324 MB global file.
  The clip is cached at `data/worldpop/som_pop_2026_1km_R2025A_UA.tif` (~1 MB).
- The raster is **masked to adm0**, not just cropped to the bbox. The global
  product is not clipped to any border, and Somalia's bounding box takes in
  heavily populated parts of eastern Ethiopia and north-eastern Kenya: a bbox
  crop alone reports 39.2M people against Somalia instead of 20.0M.
- Somalia totals: 15.20M (2018 blob), 15.89M (2020 shared drive, previous
  input), 20.00M (2026 blob, current input).
- Same 0.008333 deg resolution as the old country raster, but a slightly
  different grid origin, so cell boundaries do not coincide exactly.
- Only 2018 and 2026 are published under this release. `fp_wp_legacy` still
  tracks the old shared-drive raster.
- The `_24` suffix on 57 column names (`pop_24`, `ond_pop_exposed_24_min`, ...)
  is now a misnomer: those hold whatever year `fp_wp` points at. Left as-is
  deliberately, since they are internal and the gt labels carry no year.

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
- Somalia flood exposure is also computed daily, independently of this repo, by
`ds-floodexposure-monitoring` (blob: `ds-floodexposure-monitoring/processed/flood_exposure/som/`,
adm2 rollup in `.../tabular/som_adm_flood_exposure.parquet`). That is a
monitoring product and uses its own method; it has never been reconciled with
the two methods here.
