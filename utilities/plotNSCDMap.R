plot_map <- function(data){
  
  library(tmap)
  
  dta <- data
 
  pal <- colorBin(c("#cce2e2", "#00706e", "#002423"),
                  domain = dta[["rate"]]
                  #bins = intv)
  )
  
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
    tm_layout(title = unique(dta[["time_lab"]]),
              title.size = 0.9,
              bg.color = "#DDDDDD",
              fontface = "bold",
              frame = "white",
              panel.label.bg.color = NA,
              # frame.lwd = NA,
              inner.margins = c(0,0,0,0),
              outer.margins = c(0,0,0,0),
              legend.position = c("right","bottom"),
              legend.just = c("right","bottom"),
              legend.title.size = 0.7,
              legend.text.size = 0.6,
              legend.outside.size = 0.6
    ) 
    # tm_text("name",
    #         size = 0.65,
    #         fontface = "bold",
    #         # col = "black",
    #         # size = "area",
    #         legend.size.show = FALSE,
    #         auto.placement = FALSE
    #         )
  
 return(map)
}
