# Road Accident Patterns in India - R Shiny Dashboard

## Run
1. Install packages (once):
   `install.packages(c("shiny","bslib","dplyr","tidyr","readr","lubridate","ggplot2","plotly","DT","scales"))`
2. From the folder that contains `road_accident_dashboard/`:
   `shiny::runApp("road_accident_dashboard")`  (or open `app.R` in RStudio and click **Run App**)

Tested with R 4.3.3, shiny 1.8.0, bslib 0.6.1, plotly 4.10.4, ggplot2 3.4.4, DT 0.31.

## Files
- `app.R` - dashboard UI + server
- `scripts/data_prep.R` - import, validation checks, cleaning, derived fields
- `scripts/export_cleaned_data.R` - optional: writes `data/cleaned_road_accidents.csv` and `data/cleaning_log.csv`
- `data/indian_roads_dataset.csv` - source data (20,000 rows, 24 columns)

## Notes
- The OpenStreetMap basemap needs internet; use "Offline plot (no basemap)" otherwise.
- 2025 covers only 1 Jan - 15 Apr, so it is flagged as partial throughout.
- Counts are not risk rates (no traffic-volume data); associations do not prove causation.
