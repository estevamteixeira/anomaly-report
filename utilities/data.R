knitr::opts_chunk$set(
  echo = FALSE, 
  message = FALSE, 
  warning = FALSE,
  fig.pos = "h", 
  out.extra = "",
  out.width = '90%',
  fig.align = 'center',
  eval.after = "fig.cap",
  fig.caption= TRUE)

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
  ) %>% 
  filter(between(Birth_Year, 2012, 2022))

## Births ----

# birth <- arrow::read_parquet("./data/Birth.parquet", as_data_frame = TRUE) %>% 
#   filter(between(BrthYear, 2012, 2022))

## Shape file ----

cd_shp <-  sf::read_sf("H:/RCP/RCP_Data/TeixeiEC/Anomalies/anomaly-app-overview/anomaly-app-mob/scans/data/NSC_cd.shp") %>%
  select(GeoUID, name, geometry)


