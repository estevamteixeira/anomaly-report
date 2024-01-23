knitr::opts_chunk$set(
  echo = FALSE, 
  message = FALSE, 
  warning = FALSE,
  fig.pos = "H", 
  out.extra = "",
  out.width = '100%',
  fig.align = 'center',
  eval.after = "fig.cap")

pckgs <- c('tidyr', 'knitr', 'scales','janitor',
           'kableExtra', 'data.table','dplyr',
           'forcats','lubridate', 'anytime',
           'htmltools', 'plotly','purrr', 'fontawesome',
           'cancensus','leaflet','leafsync','htmlwidgets',
           'htmltools','tmap','DT')
# for (pp in `pckgs`) { if (!require(pp)) install.packages(pp); library(pp, character.only = T)  }

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pckgs, character.only = TRUE)
## data input

# add data ----
# anomaly ----
anom <- arrow::read_parquet("./data/Anomaly.parquet", as_data_frame = TRUE) %>%
  mutate(Diag = ifelse(tolower(substr(Diags,2,2)) %in% "r" |
                         toupper(Diags) %in% "Q999",
                       toupper(Diags),
                       gsub(".*?(Q\\d+\\.?\\d*).*", "\\1", cat)),
         Diag = gsub("\\.", "", Diag)
  )

## Births ----

# birth <- arrow::read_parquet("./data/Birth.parquet", as_data_frame = TRUE) %>% 
#   filter(between(BrthYear, 2012, 2022))

## Shape file ----

cd_shp <-  sf::read_sf("H:/RCP/RCP_Data/TeixeiEC/Anomalies/anomaly-app-overview/anomaly-app-mob/scans/data/NSC_cd.shp") %>%
  select(GeoUID, name, geometry)


## Download btn for `reactable` table
csvDownloadButton <- function(tableId, label = "Download (.csv)", filename = paste0(tableId,".csv")) {
  htmltools::tags$button(
    tagList(
      fontawesome::fa("download"), label
    ),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", tableId, filename),
    style = "
    background-color: #EBF3F2;
    color: #00706E;
    font-size: 14px;
    font-weight: bold;
    border: none;
    border-radius: 5px;
    cursor: pointer;"
  )
}
