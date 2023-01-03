source("utilities/plotNSCDMap.R")

# filter data
dta <- anom_grp_map[grepl(tolower("q00"),
                          tolower(qcode)) ] %>%
  .[,
    `:=` (total_anom = ifelse(total_anom < 5,
                              "< 5",
                              as.character(scales::comma(total_anom, accuracy = 1))),
          total_lvb = ifelse(total_lvb < 5,
                             "< 5",
                             as.character(scales::comma(total_lvb, accuracy = 1))))
  ] %>% 
  tidyr::complete(CD_UID = sort(unique(cd_names$CD_UID)),
                  nesting(cat, qcode, qcodecat, time_cat, time_lab)
  ) %>% 
  merge(cd_names,
        by = c("CD_UID")) %>% 
  setDT()

nplots <- sort(unique(dta$time_lab))

dta_map <- merge(cd_shp,
                 dta,
                 by.x = c("GeoUID"),
                 by.y = c("CD_UID"),
                 all.x = TRUE)[,
                               c("GeoUID", "name","cat","cd_type","time_lab",
                                 "total_anom","total_lvb", "rate","geometry")
                 ][,
                   `:=` (rate = data.table::fifelse(!rate %in% 0,
                                                    rate,
                                                    NA_integer_))] |> 
  sf::st_sf() |>
  sf::st_transform(4326)

# build maps

m1 <- plot_map(dta_map %>% filter(time_lab %in% nplots[1]))
m2 <- plot_map(dta_map %>% filter(time_lab %in% nplots[2]))
m3 <- plot_map(dta_map %>% filter(time_lab %in% nplots[3]))

latticeView(m1,m2,m3,
            ncol = 3)
