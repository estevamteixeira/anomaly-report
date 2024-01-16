modeling_trend <- function(data,
                           value,
                           alpha = 0.05,
                           thres = NULL,
                           disp.test = TRUE) {
  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot(is.character(value))
  stopifnot(is.numeric(alpha) && alpha > 0 && alpha < 1)
  stopifnot(is.null(thres) || is.numeric(thres))
  stopifnot(is.logical(disp.test))
  
  # Filter the data based on the value
  val <- tolower(value)
  filtered_data <- subset(data, tolower(Diag) %in% val)
  
  # Calculate theta value
  theta <- var(filtered_data %>%
                 select(CaseID,Birth_Year) %>%
                 group_by(Birth_Year) %>% 
                 mutate(total = n()) %>% 
                 select(Birth_Year, total) %>% 
                 distinct() %>% 
                 pull(), na.rm = TRUE) / mean(filtered_data %>%
                                                select(CaseID,Birth_Year) %>%
                                                group_by(Birth_Year) %>% 
                                                mutate(total = n()) %>% 
                                                select(Birth_Year, total) %>% 
                                                distinct() %>% 
                                                pull(), na.rm = TRUE)
  
  # Perform dispersion test if specified
  test <- NULL
  if (disp.test) {
    if (!requireNamespace("AER", quietly = TRUE))
      stop("The 'AER' package is required for dispersion test. Please install it.")
    test <- AER::dispersiontest(
      glm(total ~ Birth_Year,
          family = poisson,
          offset = log(count_brth_yr/10000),
          data = filtered_data %>%
            select(CaseID,Birth_Year,count_brth_yr) %>%
            group_by(Birth_Year) %>% 
            mutate(total = n()) %>% 
            select(Birth_Year, total, count_brth_yr) %>% 
            distinct())
    )[["p.value"]]
  }
  
  # Check thresholds and perform the appropriate modeling
  if (any(theta > thres) || any(test < alpha)) {
    mod <- MASS::glm.nb(total ~ Birth_Year + offset(log(count_brth_yr/10000)),
                        data = filtered_data %>%
                          select(CaseID,Birth_Year,count_brth_yr) %>%
                          group_by(Birth_Year) %>% 
                          mutate(total = n()) %>% 
                          select(Birth_Year, total, count_brth_yr) %>% 
                          distinct())
  } else {
    mod <- glm(total ~ Birth_Year,
               family = poisson,
               offset = log(count_brth_yr/10000),
               data = filtered_data %>%
                 select(CaseID,Birth_Year,count_brth_yr) %>%
                 group_by(Birth_Year) %>% 
                 mutate(total = n()) %>% 
                 select(Birth_Year, total, count_brth_yr) %>% 
                 distinct())
  }
  
  # Prepare the result dataframe
  result <- data.frame(
    coef = coef(summary(mod))[2, 1],
    std = coef(summary(mod))[2, 2],
    rate = exp(coef(summary(mod))[2, 1]),
    anom = paste0(unique(filtered_data$cat), "<br>",
                  "<span style='font-size:12px'> ICD-10: ", unique(filtered_data$Diag), "</span>"),
    n = paste0(scales::comma(nrow(filtered_data), accuracy = 1), "<br>",
               "<span style='font-size:12px'>(", min(filtered_data$Birth_Year, na.rm = TRUE),
               "-", max(filtered_data$Birth_Year, na.rm = TRUE), ")</span>"),
    trend = ifelse(coef(summary(mod))[2, 4] >= alpha,
                   paste0("No significant change ", "<br>",
                          "(", fontawesome::fa(name = "arrow-right"), ")"),
                   ifelse(coef(summary(mod))[2, 4] < alpha & coef(summary(mod))[2, 1] < 0,
                          paste0("Decreasing ", "<br>",
                                 "(", fontawesome::fa(name = "arrow-down"), ")"),
                          paste0("Increasing ", "<br>",
                                 "(", fontawesome::fa(name = "arrow-up"), ")"))),
    statistic = scales::comma((coef(summary(mod))[2, 3])^2, accuracy = 0.01),
    pvalue = coef(summary(mod))[2, 4]
  )
  
  return(result)
}


modeling_anal <- function(data,
                          value,
                          alpha = 0.05,
                          thres = NULL,
                          disp.test = TRUE){
  
  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot(is.character(value))
  stopifnot(is.numeric(alpha) && alpha > 0 && alpha < 1)
  stopifnot(is.null(thres) || is.numeric(thres))
  stopifnot(is.logical(disp.test))
  
  # Filter the data based on the value
  val <- tolower(value)
  filtered_data <- subset(data, tolower(Diag) %in% val)
  
  # Calculate theta value
  theta <- var(filtered_data %>%
                 select(CaseID,Birth_Year) %>%
                 group_by(Birth_Year) %>% 
                 mutate(total = n()) %>% 
                 select(Birth_Year, total) %>% 
                 distinct() %>% 
                 pull(), na.rm = TRUE) / mean(filtered_data %>%
                                                select(CaseID,Birth_Year) %>%
                                                group_by(Birth_Year) %>% 
                                                mutate(total = n()) %>% 
                                                select(Birth_Year, total) %>% 
                                                distinct() %>% 
                                                pull(), na.rm = TRUE)
  
  # Perform dispersion test if specified
  test <- NULL
  if (disp.test) {
    if (!requireNamespace("AER", quietly = TRUE))
      stop("The 'AER' package is required for dispersion test. Please install it.")
    test <- AER::dispersiontest(
      glm(total ~ Birth_Year,
          family = poisson,
          offset = log(count_brth_yr/10000),
          data = filtered_data %>%
            select(CaseID,Birth_Year,count_brth_yr) %>%
            group_by(Birth_Year) %>% 
            mutate(total = n()) %>% 
            select(Birth_Year, total, count_brth_yr) %>% 
            distinct())
    )[["p.value"]]
  }
  
  if (any(theta > thres) || any(test < alpha)) {
    mod <- MASS::glm.nb(total ~ Birth_Year + offset(log(count_brth_yr/10000)),
                        data = filtered_data %>%
                          select(CaseID,Birth_Year,count_brth_yr) %>%
                          group_by(Birth_Year) %>% 
                          mutate(total = n()) %>% 
                          select(Birth_Year, total, count_brth_yr) %>% 
                          distinct())
    
    ## dataset for trend line
    xy <- data.frame(Birth_Year = mod[["model"]][["Birth_Year"]],
                     cat = unique(filtered_data[["cat"]]),
                     Diag = unique(filtered_data[["Diag"]]),
                     total = mod[["fitted.values"]],
                     count_brth_yr = exp(mod[["model"]][["offset(log(count_brth_yr/10000))"]])*10000,
                     rate = mod[["fitted.values"]]/exp(mod[["model"]][["offset(log(count_brth_yr/10000))"]]),
                     model = "nb")
  } else{
    mod <- glm(total ~ Birth_Year,
               family = poisson,
               offset = log(count_brth_yr/10000),
               data = filtered_data %>%
                 select(CaseID,Birth_Year,count_brth_yr) %>%
                 group_by(Birth_Year) %>% 
                 mutate(total = n()) %>% 
                 select(Birth_Year, total, count_brth_yr) %>% 
                 distinct())
    ## dataset for trend line
    xy <- data.frame(Birth_Year = mod[["model"]][["Birth_Year"]],
                     cat = unique(filtered_data[["cat"]]),
                     Diag = unique(filtered_data[["Diag"]]),
                     total = mod[["fitted.values"]],
                     count_brth_yr = exp(mod[["model"]][["(offset)"]])*10000,
                     rate = mod[["fitted.values"]]/exp(mod[["model"]][["(offset)"]]),
                     model = "pois")
  }
  
  return(xy)
}
