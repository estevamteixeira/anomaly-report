modeling_trend <- function(data, value, alpha = 0.05,
                           thres = 2){
  
  val <- unlist(value)
  dta <- data
  dta <- dta[grepl(tolower(val),
                   tolower(qcode)) ]
  
  theta = var(dta$total_anom,
              na.rm = TRUE)/mean(dta$total_anom,
                                 na.rm = TRUE)
  
  if(var(dta$total_anom,
         na.rm = TRUE)/mean(dta$total_anom,
                            na.rm = TRUE) > thres){
    mod <- MASS::glm.nb(total_anom ~ BrthYear ,
                        offset(log(total_lvb/1000)),
                        data = dta)
  } else{
    mod <- glm(total_anom ~ BrthYear,
               family = quasipoisson,
               offset = log(total_lvb/1000),
               data = dta)
  }
  
  return(data.frame(
    coef = scales::comma(coef(summary(mod))[2,1], accuracy = 0.001),
    anom = unique(dta$cat),
    n = paste0(scales::comma(nrow(dta),accuracy = 1),
              "<br>",
              "(",
              min(dta$BrthYear, na.rm = TRUE),
              "-",
              max(dta$BrthYear, na.rm = TRUE),")"),
    trend = ifelse(coef(summary(mod))[2,4] >= alpha,
                   paste0("No significant change ",
                          "(",
                          fontawesome::fa(name = "arrow-right"),
                          ")"),
            ifelse(coef(summary(mod))[2,4] < alpha &
                   coef(summary(mod))[2,1] < 0,
                   paste0("Decreasing ",
                          "(",
                          fontawesome::fa(name = "arrow-down"),
                          ")"),
                   paste0("Increasing ",
                          "(",
                          fontawesome::fa(name = "arrow-up"),
                          ")"))),
    statistic = scales::comma((coef(summary(mod))[2,3])^2, accuracy = 0.01),
    pvalue = ifelse(coef(summary(mod))[2,4] < 0.0001,
                    "< 0.0001",
                    scales::comma(coef(summary(mod))[2,4], accuracy = 0.01)
    )))
}

modeling_anal <- function(data, value, alpha = 0.05,
                          thres = 1.5){
  
  val <- unlist(value)
  dta <- data
  dta <- dta[grepl(tolower(val),
                   tolower(qcode)) ]
  
  if(var(dta[["total_anom"]],
         na.rm = TRUE)/mean(dta[["total_anom"]],
                            na.rm = TRUE) > thres){
    mod <- MASS::glm.nb(total_anom ~ BrthYear ,
                        offset(log(total_lvb/1000)),
                        data = dta)
    ## dataset for trend line
    xy <- data.frame(BrthYear = mod[["model"]][["BrthYear"]],
                     cat = unique(dta[["cat"]]),
                     total_anom = mod[["fitted.values"]],
                     total_lvb = exp(mod[["model"]][["(weights)"]])*1000,
                     rate = mod[["fitted.values"]]/exp(mod[["model"]][["(weights)"]]))
  } else{
    mod <- glm(total_anom ~ BrthYear,
               family = quasipoisson,
               offset = log(total_lvb/1000),
               data = dta)
    ## dataset for trend line
    xy <- data.frame(BrthYear = mod[["model"]][["BrthYear"]],
                     cat = unique(dta[["cat"]]),
                     total_anom = mod[["fitted.values"]],
                     total_lvb = exp(mod[["model"]][["(offset)"]])*1000,
                     rate = mod[["fitted.values"]]/exp(mod[["model"]][["(offset)"]]))
  }
  
  return(xy)
}
