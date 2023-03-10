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
      } else if(any(tolower(dta[["cat"]]) %in% c("cleft palate"))){
        colors <- brewer.pal(n = nfac,
                             name = palt)
        pal <- c("Cleft palate" = colors[1],
                 "Cleft lip" = colors[2],
                 "Cleft palate with cleft lip" = colors[3])
      } else if(any(tolower(dta[["cat"]]) %in% c("anotia/microtia"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Anophtalmos/Microphtalmos" = colors[1],
               "Anotia/Microtia" = colors[2])
      } else if(any(tolower(dta[["cat"]]) %in% c("microcephaly"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Microcephaly" = "#1B9E77",
               "Congenital hydrocephalus" = "#D95F02",
               "Arhinencephaly/Holoprosencephal" = "#7570B3")
      } else if(any(tolower(dta[["cat"]]) %in% c("commom truncus"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Commom truncus" = colors[1],
               "Discordant ventriculoarterial connection" = colors[2],
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
  
  
  
  plotly::plot_ly(data = dta[order(BrthYear)],
                  x = ~BrthYear,
                  y = ~.data[[var]],
                  type = "scatter",
                  mode = "lines",
                  linetype = ~cat,
                  # color = ~cat,
                  colors = pal,
                  # symbol = ~cat,
                  hovertemplate = ~paste(
                    "<b>", cat, "-", BrthYear, "</b>",
                    "<br> Total reported cases:",
                    ifelse(total_anom < 5,
                           "< 5",
                           as.character(scales::comma(total_anom,
                                                      accuracy = 1))),
                    "<br> Total births:",
                    ifelse(total_lvb < 5,
                           "< 5",
                           as.character(scales::comma(total_lvb,
                                                      accuracy = 1))),
                    "<br> Prevalence (*cases per 1,000 total births):",
                    scales::comma(rate,
                                  accuracy = 0.01),
                    "<extra></extra>" # removes the trace name from the hover text
                  ),
                  line = list(color = pal)
                  # marker = list(col = pal)
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
  dta[["matage"]] <- factor(dta[["matage"]])
  dta[["matage_format"]] <- factor(dta[["matage_format"]],
                                   levels = c("age < 20",
                                              "20 ??? age < 25",
                                              "25 ??? age < 30",
                                              "30 ??? age < 35",
                                              "35 ??? age < 40",
                                              "age ??? 40"))
  
  # get number of different factor levels for color
  nfac <- length(levels(dta[["matage"]]))
  
  # assign colors to factors automatically
  # for name options please visit: https://colorbrewer2.org
  
  if(nfac < 2){
    pal <- c("#008D8B")
  } else if(nfac < 3){
    
    pal <- c("#008D8B",
             "#FE5D26")
  } else {
    if(any(tolower(dta[["matage"]]) %in% c("6"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("age < 20" = colors[1],
               "20 ??? age < 25" = colors[2],
               "25 ??? age < 30" = colors[3],
               "30 ??? age < 35" = colors[4],
               "35 ??? age < 40" = colors[5],
               "age ??? 40" = colors[6])
    }
  }
  
  
  
  plotly::plot_ly(data = dta1,
                  x = ~year_cat,
                  y = ~.data[[var]],
                  type = "scatter",
                  mode = "lines+markers",
                  # linetype = ~matage_format,
                  color = ~matage_format,
                  colors = pal,
                  # symbol = ~cat,
                  hovertemplate = ~paste(
                    "<b>", matage_format, "-", year_cat, "</b>",
                    "<br> Total reported cases:",
                    ifelse(total_anom_yc < 5,
                           "< 5",
                           as.character(scales::comma(total_anom_yc,
                                                      accuracy = 1))),
                    "<br> Total births:",
                    ifelse(total_lvb_yc < 5,
                           "< 5",
                           as.character(scales::comma(total_lvb_yc,
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
