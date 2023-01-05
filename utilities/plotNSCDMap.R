plot_map <- function(data){
  
  library(tmap)
  
  dta <- data
 
  pal <- colorBin(c("#cce2e2", "#00706e", "#002423"),
                  domain = dta[["rate"]]
                  #bins = intv)
  )
  
  txt <- dta %>% 
    dplyr::group_by(time_lab) %>% 
    dplyr::filter(rate > as.numeric(quantile(rate, probs  = 0.65, na.rm = TRUE))) %>% 
    dplyr::ungroup() |>
    dplyr::mutate(name = gsub(" ", "\n", name)) |>
    sf::st_sf() |>
    sf::st_transform(4326)
    
  
map <- tm_shape(dta) +
    tm_fill("rate",
            style = "pretty",
            # style = "fixed",
            # breaks = br,
            title = "", 
            palette = c("#cce2e2", "#00706e", "#002423"),
            # legend.format = list(fun = scales::label_percent()),
            # legend.show = FALSE,
            border.col = "white",
            colorNA = "#808080",
            textNA = "Not informed",
            # popup.vars = dta[["hover.vars"]],
            # popup.format = dta[["hover.format"]]
    ) +
    tm_borders(lwd = 0.01, col = "white") +
  tm_layout(bg.color = NA,
            fontface = "bold",
            frame = NA,
            panel.label.bg.color = NA,
            panel.label.size = 1,
            panel.label.fontface = "bold",
            frame.lwd = NA,
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0),
            between.margin = 0,
            asp = 0,
            legend.position = c(0.65, 1),
            # legend.just = c("left","center"),
            legend.outside.position = "bottom",
            # legend.title.size = 0.7,
            legend.text.size = 0.5
            # legend.outside.size = 0.8
  ) +
    tm_facets(by = "time_lab",
              nrow = 2,
              sync = TRUE,
              ) +
   tm_shape(txt) +
   tm_facets(by = "time_lab",
             nrow = 2,
             sync = TRUE) +
    tm_text("name",
            size = .7,
            fontface = "bold"
            # col = "white"
            # bg.color = "white"
            # # size = "area",
            # legend.size.show = FALSE,
            # auto.placement = FALSE
            )
  
 return(map)
}
