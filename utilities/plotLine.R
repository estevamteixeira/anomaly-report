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
      } else if(any(tolower(dta[["cat"]]) %in% c("Anotia/Microtia"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Anophtalmos/Microphtalmos" = colors[1],
               "Anotia/Microtia" = colors[2])
      } else if(any(tolower(dta[["cat"]]) %in% c("Microcephaly"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Microcephaly" = colors[1],
               "Congenital hydrocephalus" = colors[2],
               "Arhinencephaly/Holoprosencephal" = colors[3])
      } else if(any(tolower(dta[["cat"]]) %in% c("Commom truncus"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Commom truncus" = colors[1],
               "Discordant ventriculoarterial connection" = colors[2],
               "Atrioventricular septal defect" = colors[3],
               "Tetralogy of Fallot" = colors[4],
               "Hypoplastic left heart syndrome" = colors[5])
      } else if(any(tolower(dta[["cat"]]) %in% c("Hirschsprung disease"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Oesophageal atresia/stenosis, tracheoesophageal fistula" = colors[1],
               "Small intestine absence/atresia/stenosis" = colors[2],
               "Ano-rectal absence/atresia/stenosis" = colors[3],
               "Hirschsprung disease" = colors[4],
               "Atresia of bile ducts" = colors[5])
      } else if(any(tolower(dta[["cat"]]) %in% c("Hypospadias"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Cryptorchidism/undescended testicles" = colors[1],
               "Hypospadias" = colors[2],
               "Indeterminate sex and pseudohermaphroditism" = colors[3],
               "Epispadias" = colors[4])
      } else if(any(tolower(dta[["cat"]]) %in% c("Renal agenesis"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Renal agenesis" = colors[1],
               "Cystic kidney" = colors[2],
               "Bladder and cloacal exstroph" = colors[3],
               "Lower urinary tract obstruction" = colors[4])
      } else if(any(tolower(dta[["cat"]]) %in% c("Gastroschisis"))){
      colors <- brewer.pal(n = nfac,
                           name = palt)
      pal <- c("Omphalocele/Exomphalos" = colors[1],
               "Gastroschisis" = colors[2])
      } else if(any(tolower(dta[["cat"]]) %in% c("Down Syndrome"))){
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
                  color = ~cat,
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

