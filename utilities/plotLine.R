plot_line <- function(data, var, palt = "Dark2"){
  require(dplyr)
  require(ggplot2)
  require(plotly)
  require(RColorBrewer)
  
  # gets dataset
  dta <- data
  var <- unlist(var)
  
  # transform the categories variable into factor
  dta[["cat"]] <- factor(dta[["cat"]])
  
  # get number of different factor levels for color
  nfac <- length(levels(dta[["cat"]]))
  
  # assign colors to factors automatically
  # for name options please visit: https://colorbrewer2.org
  
  if(nfac < 2){
    pal <- c("#008D8B")
  } else if(nfac < 3){
    
    pal <- c("#008D8B",
             "#FE5D26")
  } else {
    if(any(tolower(dta[["cat"]]) %in% c("spina bifida"))){
      colors <- brewer.pal(n = nfac,
                        name = palt)
      pal <- c("Anencephaly" = colors[1],
               "Encephalocele" = colors[2],
               "Spina bifida" = colors[3])
      } else if(any(tolower(dta[["cat"]]) %in% c("cleft palate only"))){
        colors <- brewer.pal(n = nfac,
                             name = palt)
        pal <- c("Cleft palate only" = colors[1],
                 "Cleft lip only" = colors[2],
                 "Cleft palate with cleft lip" = colors[3])
      } else if(any(tolower(dta[["cat"]]) %in% c("anotia/microtia"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Anophtalmos/Microphtalmos" = colors[1],
               "Anotia/Microtia" = colors[2],
               "Choanal atresia" = colors[3])
      } else if(any(tolower(dta[["cat"]]) %in% c("microcephaly"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Microcephaly" = "#1B9E77",
               "Congenital hydrocephalus" = "#D95F02",
               "Arhinencephaly/Holoprosencephaly" = "#7570B3")
      } else if(any(tolower(dta[["cat"]]) %in% c("commom truncus"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Commom truncus" = colors[1],
               "Transposition of great vessels" = colors[2],
               "Atrioventricular septal defect" = colors[3],
               "Tetralogy of Fallot" = colors[4],
               "Hypoplastic left heart syndrome" = colors[5],
               "Coarctation of aorta" = colors[6])
      } else if(any(tolower(dta[["cat"]]) %in% c("hirschsprung disease"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Oesophageal atresia/stenosis, tracheoesophageal fistula" = colors[1],
               "Small intestine absence/atresia/stenosis" = colors[2],
               "Ano-rectal absence/atresia/stenosis" = colors[3],
               "Hirschsprung disease" = colors[4],
               "Atresia of bile ducts" = colors[5])
      } else if(any(tolower(dta[["cat"]]) %in% c("hypospadias"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Cryptorchidism/undescended testicles" = colors[1],
               "Hypospadias" = colors[2],
               "Indeterminate sex" = colors[3],
               "Epispadias" = colors[4])
      } else if(any(tolower(dta[["cat"]]) %in% c("renal agenesis"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Renal agenesis" = colors[1],
               "Cystic kidney" = colors[2],
               "Bladder and cloacal exstrophy" = colors[3],
               "Lower urinary tract obstruction" = colors[4])
      } else if(any(tolower(dta[["cat"]]) %in% c("gastroschisis"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Omphalocele/Exomphalos" = colors[1],
               "Gastroschisis" = colors[2],
               "Diaphragmatic hernia" = colors[3])
      } else if(any(tolower(dta[["cat"]]) %in% c("down syndrome"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Down Syndrome" = colors[1],
               "Trisomy 13 - Patau" = colors[2],
               "Trisomy 18 - Edwards" = colors[3],
               "Turner syndrome" = colors[4])
      }
    }
  
  
  
  plotly::plot_ly(data = dta[order(BrthYear)],
                  x = ~BrthYear,
                  y = ~dta[[var]],
                  type = "scatter",
                  mode = "lines",
                  # linetype = ~cat,
                  color = ~cat,
                  colors = pal,
                  symbol = ~cat,
                  hovertemplate = ~paste(
                    "<b>", cat, "-", BrthYear, "</b>",
                    "<br> Total reported cases:",
                    ifelse(total_anom < 5,
                           "< 5",
                           as.character(scales::comma(total_anom,
                                                      accuracy = 1))),
                    "<br> Total births:",
                    ifelse(total_lvb_yr < 5,
                           "< 5",
                           as.character(scales::comma(total_lvb_yr,
                                                      accuracy = 1))),
                    "<br> Prevalence (*cases per 1,000 total births):",
                    scales::comma(rate,
                                  accuracy = 0.01),
                    "<extra></extra>" # removes the trace name from the hover text
                  ),
                  line = list(color = pal),
                  marker = list(col = pal)
                  ) %>%
    plotly::style(hoverlabel = list(
      bgcolor  = "black",
      bordercolor = "transparent",
      font = list(
        color = "white",
        size = 14,
        face = "bold"
      )
    )) %>%
    plotly::layout(
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        y = -0.35, x = 0.5,
        yanchor = "middle", xanchor = "center",
        font = list(
          size = 12,
          face = "bold"
        )
      ),
      xaxis = list(
        title = list(
          text = "Year",
          face = "bold",
          size = 14
        ),
        tickfonts = list(
          face = "bold",
          size = 12
        )
      ),
      yaxis = list(
        title = list(
          text = ifelse(var %in% "rate",
                        "Prevalence (*cases per 1,000 total births)",
                        "Total reported cases"),
          face = "bold",
          size = 14
        ),
        tickfonts = list(
          face = "bold",
          size = 12
        ),
        rangemode = "tozero"
      )
    )
}

## maternal age ================

plot_line_matage <- function(data, var, palt = "Dark2"){
  
  require(dplyr)
  require(ggplot2)
  require(plotly)
  require(RColorBrewer)
  
  # gets dataset
  dta <- data
  var <- unlist(var)
  
  # transform the categories variable into factor
  # dta[["matage"]] <- factor(dta[["matage"]])
  dta[["matage_format"]] <- factor(dta[["matage_format"]],
                                   levels = c("< 25", #"20-24", "25-29",
                                              "25-34",# "35-39", "40-44",
                                              "\u2265 35", "-1"))
  
  # get number of different factor levels for color
  nfac <- length(levels(dta[["matage_format"]]))
  
  # assign colors to factors automatically
  # for name options please visit: https://colorbrewer2.org
  
  if(nfac < 2){
    pal <- c("#008D8B")
  } else if(nfac < 3){
    
    pal <- c("#008D8B",
             "#FE5D26")
  } else {
    if(any(tolower(dta[["matage_format"]]) %in% c("< 25"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("< 25" = colors[1],
               "25-34" = colors[2],
               "\u2265 35" = colors[3]
               # "30-34" = colors[4],
               # "35-39" = colors[5],
               # "40-44" = colors[6],
               # "\u2265 45" = colors[7]
               )
    }
  }
  
  
  
  plotly::plot_ly(data = dta,
                  x = ~BrthYear,
                  y = ~dta[[var]],
                  type = "scatter",
                  mode = "lines+markers",
                  # linetype = ~matage_format,
                  color = ~matage_format,
                  colors = pal,
                  symbol = ~matage_format,
                  hovertemplate = ~paste(
                    "<b>", matage_format, "-", BrthYear, "</b>",
                    "<br> Total reported cases:",
                    ifelse(total_anom < 5,
                           "< 5",
                           as.character(scales::comma(total_anom,
                                                      accuracy = 1))),
                    "<br> Total births:",
                    ifelse(total_lvb_yr < 5,
                           "< 5",
                           as.character(scales::comma(total_lvb_yr,
                                                      accuracy = 1))),
                    "<br> Prevalence (*cases per 1,000 total births):",
                    scales::comma(rate,
                                  accuracy = 0.01),
                    "<extra></extra>" # removes the trace name from the hover text
                  ),
                  line = list(color = pal),
                  marker = list(col = pal)
  ) %>%
    plotly::style(hoverlabel = list(
      bgcolor  = "black",
      bordercolor = "transparent",
      font = list(
        color = "white",
        size = 14,
        face = "bold"
      )
    )) %>%
    plotly::layout(
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        y = -0.35, x = 0.5,
        yanchor = "middle", xanchor = "center",
        font = list(
          size = 12,
          face = "bold"
        )
      ),
      xaxis = list(
        title = list(
          text = "Year",
          face = "bold",
          size = 14
        ),
        tickfonts = list(
          face = "bold",
          size = 12
        )
      ),
      yaxis = list(
        title = list(
          text = ifelse(var %in% "rate",
                        "Prevalence (*cases per 1,000 total births)",
                        "Total reported cases"),
          face = "bold",
          size = 14
        ),
        tickfonts = list(
          face = "bold",
          size = 12
        ),
        rangemode = "tozero"
      )
    )
}

##---------- PDF ---------

plot_line_pdf <- function(data, var, palt = "Dark2"){
  require(dplyr)
  require(ggplot2)
  require(ggrepel)
  require(plotly)
  require(RColorBrewer)
  
  # gets dataset
  dta <- data
  var <- unlist(var)
  
  # transform the categories variable into factor
  dta[["cat"]] <- factor(dta[["cat"]])
  
  # get number of different factor levels for color
  nfac <- length(levels(dta[["cat"]]))
  
  # assign colors to factors automatically
  # for name options please visit: https://colorbrewer2.org
  
  if(nfac < 2){
    pal <- c("#008D8B")
  } else if(nfac < 3){
    if (any(grepl("fit", levels(dta[["cat"]])))){
      pal <- ifelse(grepl("fit", levels(dta[["cat"]])),
                    "black",
                    "#008D8B")
    } else{
      pal <- c("#008D8B",
               "black")
    }
    
  } else {
    if(any(tolower(dta[["cat"]]) %in% c("spina bifida"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Anencephaly" = colors[1],
               "Encephalocele" = colors[2],
               "Spina bifida" = colors[3])
    } else if(any(tolower(dta[["cat"]]) %in% c("cleft palate only"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Cleft palate only" = colors[1],
               "Cleft lip only" = colors[2],
               "Cleft palate with cleft lip" = colors[3])
    } else if(any(tolower(dta[["cat"]]) %in% c("anotia/microtia"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Anophtalmos/Microphtalmos" = colors[1],
               "Anotia/Microtia" = colors[2],
               "Choanal atresia" = colors[3])
    } else if(any(tolower(dta[["cat"]]) %in% c("microcephaly"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Microcephaly" = colors[1],
               "Congenital hydrocephalus" = colors[2],
               "Arhinencephaly/Holoprosencephaly" = colors[3])
    } else if(any(tolower(dta[["cat"]]) %in% c("commom truncus"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Commom truncus" = colors[1],
               "Transposition of great vessels" = colors[2],
               "Atrioventricular septal defect" = colors[3],
               "Tetralogy of Fallot" = colors[4],
               "Hypoplastic left heart syndrome" = colors[5],
               "Coarctation of aorta" = colors[6])
    } else if(any(tolower(dta[["cat"]]) %in% c("hirschsprung disease"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Oesophageal atresia/stenosis, tracheoesophageal fistula" = colors[1],
               "Small intestine absence/atresia/stenosis" = colors[2],
               "Ano-rectal absence/atresia/stenosis" = colors[3],
               "Hirschsprung disease" = colors[4],
               "Atresia of bile ducts" = colors[5])
    } else if(any(tolower(dta[["cat"]]) %in% c("hypospadias"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Cryptorchidism/undescended testicles" = colors[1],
               "Hypospadias" = colors[2],
               "Indeterminate sex and pseudohermaphroditism" = colors[3],
               "Epispadias" = colors[4])
    } else if(any(tolower(dta[["cat"]]) %in% c("renal agenesis"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Renal agenesis" = colors[1],
               "Cystic kidney" = colors[2],
               "Bladder and cloacal exstroph" = colors[3],
               "Lower urinary tract obstruction" = colors[4])
    } else if(any(tolower(dta[["cat"]]) %in% c("gastroschisis"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Omphalocele/Exomphalos" = colors[1],
               "Gastroschisis" = colors[2])
    } else if(any(tolower(dta[["cat"]]) %in% c("down syndrome"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Down Syndrome" = colors[1],
               "Trisomy 13 - Patau" = colors[2],
               "Trisomy 18 - Edwards" = colors[3],
               "Turner syndrome" = colors[4])
    }
  }
  
  plot <- ggplot(dta,
         aes(x = BrthYear,
             y = dta[[var]],
             group = dta[["cat"]])) + 
    geom_point(aes(color = dta[["cat"]],
                   fill = dta[["cat"]],
                   shape = dta[["cat"]]),
               alpha = 0.65) + 
    geom_line(aes(color = dta[["cat"]])) + 
    scale_color_manual(name='',
                       values = pal,
                       limits = names(pal)) +
    scale_fill_manual(name = "",
                      values = pal,
                      limits = names(pal)) +
    scale_shape_manual(name = "",
                       values = c(21,22,23,24,25,1,2,4)) +
    theme_classic() +
    theme(strip.text.x = element_text( face = "bold", size = 10),
          strip.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(face="bold", size = 10),
          axis.text.x = element_text(face="bold", size = 8, color = "black"),
          axis.text.y = element_text(face="bold", size = 8, color = "black"),
          title = element_text(face="bold", size = 10, hjust = 0.5),
          plot.title = element_text(hjust = 0.5),
          # legend
          legend.position = "bottom",
          legend.text = element_text(face="bold", size = 8),
          legend.title = element_blank(),
          legend.justification = "center") + 
    scale_y_continuous(labels = scales::comma_format( accuracy = .1),
                       breaks = pretty,
                       limits = c(0,NA)) +
    labs(y = 'Prevalence (*cases per 1,000 total births)',
         x = "Year") +
    guides(color =  guide_legend(nrol = ifelse(nfac > 3, 
                                               2, 1)))

  return(plot)
}

## maternal age -----
plot_line_pdf_matage <- function(data, var, palt = "Dark2"){
  require(dplyr)
  require(ggplot2)
  require(ggrepel)
  require(plotly)
  require(RColorBrewer)
  
  # gets dataset
  dta <- data 
  
  var <- unlist(var)
  
  # transform the categories variable into factor
  # dta[["matage"]] <- factor(dta[["matage"]])
  # dta[["matage_format"]] <- factor(dta[["matage_format"]],
  #                                  levels = c("< 20",
  #                                             "20-24",
  #                                             "25-29",
  #                                             "30-34",
  #                                             "35-39",
  #                                             "40-44",
  #                                             "\u2265 45"))
  
  # get number of different factor levels for color
  nfac <- length(unique(dta[["matage_format"]]))
  
  # assign colors to factors automatically
  # for name options please visit: https://colorbrewer2.org
  
  if(nfac < 2){
    pal <- c("#008D8B")
  } else if(nfac < 3){
    
    pal <- c("#008D8B",
             "#FE5D26")
  } else {
    if(any(tolower(dta[["matage_format"]]) %in% c("< 25"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- pal <- c("< 25" = colors[1],
                      "25-34" = colors[2],
                      "\u2265 35" = colors[3]
                      # "30-34" = colors[4],
                      # "35-39" = colors[5],
                      # "40-44" = colors[6],
                      # "\u2265 45" = colors[7]
      )
      
      pal <- pal[!is.na(pal)]
    }
  }
  
  
  plot <- ggplot(dta,
                 aes(x = BrthYear,
                     y = dta[[var]],
                     group = dta[["matage_format"]])) + 
    geom_point(aes(color = dta[["matage_format"]],
                   fill = dta[["matage_format"]],
                   shape = dta[["matage_format"]]),
               alpha = 0.65) + 
    geom_line(aes(color = dta[["matage_format"]])) + 
    scale_color_manual(name='',
                       values = pal,
                       limits = names(pal)) +
    scale_fill_manual(name = "",
                      values = pal,
                      limits = names(pal)) +
    scale_shape_manual(name = "",
                       values = c(21,22,23,24,25,1,2,4)) +
    theme_classic() +
    theme(strip.text.x = element_text( face = "bold", size = 10),
          strip.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(face="bold", size = 10),
          axis.text.x = element_text(face="bold", size = 8, color = "black"),
          axis.text.y = element_text(face="bold", size = 8, color = "black"),
          title = element_text(face="bold", size = 10, hjust = 0.5),
          plot.title = element_text(hjust = 0.5),
          # legend
          legend.position = "bottom",
          legend.text = element_text(face="bold", size = 8),
          legend.title = element_blank(),
          legend.justification = "center") + 
    scale_y_continuous(labels = scales::comma_format( accuracy = .1),
                       breaks = pretty,
                       limits = c(0,NA)) +
    labs(y = 'Prevalence (*cases per 1,000 total births)',
         x = "Year") +
    guides(color =  guide_legend(nrol = ifelse(nfac > 3, 
                                               2, 1)))
  
  return(plot)
}