# AI-Driven Structural Breaks in U.S. Electricity Demand

Final project repository for `ENV 797: Time Series Analysis for Energy and Environment Applications`.

## Project Overview

This project studies whether post-2022 electricity demand patterns in `TEX (ERCOT)` show stronger structural change than `MISO`, using daily demand, weather controls, and time-series forecasting models.

The final report is:

- `Wei_Oh_Loo_ProjectReport_S26.rmd`
- `Wei_Oh_Loo_ProjectReport_S26.pdf`

## Repository Structure

- `Wei_Oh_Loo_ProjectReport_S26.rmd`: source file for the final report
- `Wei_Oh_Loo_ProjectReport_S26.pdf`: compiled final report
- `data/`: demand and weather input files used in the analysis
- `archive/`: earlier exploratory scripts and older report drafts

## Data Files

The report uses the following files in `data/`:

- `Region_TEX.xlsx`
- `TEX_Weather.csv`
- `MISO.xlsx`
- `MISO_Weather.csv`

## Reproducibility

To compile the final report, render:

```r
rmarkdown::render("Wei_Oh_Loo_ProjectReport_S26.rmd")
```

The report expects these R packages:

- `tidyverse`
- `readxl`
- `readr`
- `lubridate`
- `forecast`
- `knitr`
- `kableExtra`
- `strucchange`
- `scales`

PDF output is rendered with `xelatex`.

## Notes

- The main analysis and final results are in `Wei_Oh_Loo_ProjectReport_S26.rmd`.
- Files in `archive/` are kept for reference and are not part of the final report workflow.
