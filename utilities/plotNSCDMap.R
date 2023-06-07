# HTML ---------
plot_map <- function(data){
  
  library("tmap")
  
  NS <- data
  
  pal <- colorBin(c("#cce2e2", "#00706e", "#002423"),
                  domain = NS[["rate"]]
                  #bins = intv
  )
  
  ## hover text
  NS$txt <- ifelse(is.na(NS[["rate"]]) | NS[["rate"]] == 0,
                    paste(
                      # "<b>",stringr::str_to_title(NS[[which(grepl("name",tolower(names(NS))))]]),"</b>",
                      "<br>No information provided"
                    ),
                    paste(
                      # "<b>",stringr::str_to_title(NS[[which(grepl("name",tolower(names(NS))))]]),"</b>",
                      "<br>Reported occurences:", NS[[which(grepl("total_anom",names(NS)))]],
                      "<br>Prevalence (*cases per 1,000 total births):" , scales::comma(NS[["rate"]], accuracy = 0.1)
                    )
  )
  
  
  map <- tm_shape(NS) +
    tm_fill("rate",
            style = "pretty",
            # style = "fixed",
            # breaks = br,
            title = "", 
            palette = c("#cce2e2", "#00706e", "#002423"),
            # legend.format = list(fun = scales::label_percent()),
            legend.show = FALSE,
            border.col = "white",
            colorNA = "grey75",
            textNA = "Not informed",
            popup.vars = c(" "="txt"), ## we included " " to remove the name of the column that contains the popuu values
            popup.format = list(html.escape = FALSE) # to use HTML sintaxe
    ) +
    tm_borders(lwd = 1, col = "white") +
    tm_facets(by = "BrthYear",
              nrow = 2,
              sync = TRUE,
    ) +
    tm_layout(bg.color = "#DDDDDD",
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
              # legend.show = FALSE,
              #legend.position = c("left","center"),
              legend.just = c("right","bottom"),
              legend.title.size = 0.8,
              legend.text.size = 0.6,
              # legend.outside.size = 0.8
    ) +
    tm_view(view.legend.position = NA) +
    tm_legend(outside=TRUE) +
    tmap_options(legend.position = c("right","bottom"),
                 basemaps = NULL,
                 overlays = NULL)
  
  
  return(map)
}

## PDF ---------
plot_map_pdf <- function(data){
  
  library(tmap)
  
  dta <- data
 
  pal <- colorBin(c("#cce2e2", "#00706e", "#002423"),
                  domain = dta[["rate"]]
                  #bins = intv)
  )
  
  # txt <- dta |>
  #   dplyr::mutate(txt_color = ifelse(rate > as.numeric(quantile(rate, probs  = 0.95, na.rm = TRUE)),
  #                                    "white",
  #                                    "black")) |>
  #   dplyr::group_by(BrthYear) |>
  #   # dplyr::filter(BrthYear_geo > as.numeric(quantile(BrthYear_geo, probs  = 0.65, na.rm = TRUE))) %>% 
  #   dplyr::slice_max(count_anom, n = 5) |>
  #   dplyr::ungroup() |>
  #   dplyr::mutate(name = gsub(" ", "\n", name)) |>
  #   sf::st_sf() |>
  #   sf::st_transform(4326)
    
  
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
            colorNA = "grey75",
            textNA = "Not informed",
            # popup."rate"s = dta[["hover."rate"s"]],
            # popup.format = dta[["hover.format"]]
    ) +
  tm_borders(lwd = 0.1, col = "white") +
  tm_facets(by = "BrthYear",
              nrow = 2,
              sync = TRUE,
              ) +
   # tm_shape(txt) +
   # tm_facets(by = "BrthYear",
   #           nrow = 1,
   #           sync = TRUE) +
    # tm_text("name",
    #         size = 0.8,
    #         fontface = "bold",
    #         remove.overlap = TRUE,
    #         #shadow = TRUE,
    #         col = "txt_color"
    #         # bg.color = "white"
    #         # # size = "area",
    #         # legend.size.show = FALSE,
    #         # auto.placement = FALSE
    #         ) +
  # tm_facets(by = "BrthYear",
  #           nrow = 2,
  #           sync = TRUE,
  # ) +
  tm_layout(bg.color = NA, #DDDDDD
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
            # legend.show = FALSE,
            legend.position = c("left","center"),
            legend.just = c("left","center"),
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            # legend.outside.size = 0.8
  ) +
  tm_view(view.legend.position = c("left","center")) +
  tm_legend(outside=TRUE)
  
  
 return(map)
}
