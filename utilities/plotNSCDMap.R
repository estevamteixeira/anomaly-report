plot_map <- function(data){
  
  library(tmap)
  
  dta <- data
 
  pal <- colorBin(c("#cce2e2", "#00706e", "#002423"),
                  domain = dta[["rate"]]
                  #bins = intv)
  )
  
  txt <- dta %>% 
    dplyr::group_by(time_lab) %>% 
    dplyr::filter(rate > as.numeric(quantile(rate, probs  = 0.75, na.rm = TRUE))) %>% 
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
            border.col = "white",
            colorNA = "#808080",
            textNA = "Not informed",
            # popup.vars = dta[["hover.vars"]],
            # popup.format = dta[["hover.format"]]
    ) +
    tm_borders(lwd = 0.01, col = "white") +
    tm_facets(by = "time_lab",
              nrow = 2, sync = TRUE) +
    tm_layout(bg.color = "#DDDDDD",
              fontface = "bold",
              frame = NA,
              panel.label.bg.color = NA,
              panel.label.size = 1.5,
              panel.label.fontface = "bold",
              frame.lwd = NA,
              inner.margins = c(0,0,0,0),
              outer.margins = c(0,0,0,0),
              legend.position = c("center","center"),
              legend.just = c("center","center"),
              # legend.title.size = 0.7,
              legend.text.size = 0.8
              # legend.outside.size = 0.8
    ) +
   tm_shape(txt) +
   tm_facets(by = "time_lab",
             nrow = 2, sync = TRUE) +
    tm_text("name",
            size = 1,
            fontface = "bold",
            col = NA,
            # size = "area",
            legend.size.show = FALSE,
            auto.placement = FALSE
            )
  
 return(map)
}
