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
  filtered_data <- data[grepl(val, tolower(data[["qcode"]])), ]
  
  # Calculate theta value
  theta <- var(filtered_data$total_anom, na.rm = TRUE) / mean(filtered_data$total_anom, na.rm = TRUE)
  
  # Perform dispersion test if specified
  test <- NULL
  if (disp.test) {
    if (!requireNamespace("AER", quietly = TRUE))
      stop("The 'AER' package is required for dispersion test. Please install it.")
    test <- AER::dispersiontest(
      glm(total_anom ~ BrthYear,
          family = poisson,
          offset = log(total_lvb_yr/1000),
          data = filtered_data)
    )[["p.value"]]
  }
  
  # Check thresholds and perform the appropriate modeling
  if (any(theta > thres) || any(test < alpha)) {
    mod <- MASS::glm.nb(total_anom ~ BrthYear,
                        offset(log(total_lvb_yr/1000)),
                        data = filtered_data)
  } else {
    mod <- glm(total_anom ~ BrthYear,
               family = poisson,
               offset = log(total_lvb_yr/1000),
               data = filtered_data)
  }
  
  # Prepare the result dataframe
  result <- data.frame(
    coef = coef(summary(mod))[2, 1],
    std = coef(summary(mod))[2, 2],
    rate = exp(coef(summary(mod))[2, 1]),
    anom = paste0(unique(filtered_data$cat), "<br>",
                  "<span style='font-size:12px'> ICD-10: ", unique(filtered_data$qcodecat), "</span>"),
    n = paste0(scales::comma(nrow(filtered_data), accuracy = 1), "<br>",
               "<span style='font-size:12px'>(", min(filtered_data$BrthYear, na.rm = TRUE),
               "-", max(filtered_data$BrthYear, na.rm = TRUE), ")</span>"),
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
  filtered_data <- data[grepl(val, tolower(data[["qcode"]])), ]
  
  # Calculate theta value
  theta <- var(filtered_data$total_anom, na.rm = TRUE) / mean(filtered_data$total_anom, na.rm = TRUE)
  
  # Perform dispersion test if specified
  test <- NULL
  if (disp.test) {
    if (!requireNamespace("AER", quietly = TRUE))
      stop("The 'AER' package is required for dispersion test. Please install it.")
    test <- AER::dispersiontest(
      glm(total_anom ~ BrthYear,
          family = poisson,
          offset = log(total_lvb_yr/1000),
          data = filtered_data)
    )[["p.value"]]
  }
  
  if (any(theta > thres) || any(test < alpha)) {
    mod <- MASS::glm.nb(total_anom ~ BrthYear,
                        offset(log(total_lvb_yr/1000)),
                        data = filtered_data)
    
    ## dataset for trend line
    xy <- data.frame(BrthYear = mod[["model"]][["BrthYear"]],
                     cat = unique(filtered_data[["cat"]]),
                     qcode = unique(filtered_data[["qcode"]]),
                     qcodecat = unique(filtered_data[["qcodecat"]]),
                     total_anom = mod[["fitted.values"]],
                     total_lvb_yr = exp(mod[["model"]][["(weights)"]])*1000,
                     rate = mod[["fitted.values"]]/exp(mod[["model"]][["(weights)"]]),
                     model = "nb")
  } else{
    mod <- glm(total_anom ~ BrthYear,
               family = poisson,
               offset = log(total_lvb_yr/1000),
               data = filtered_data)
    ## dataset for trend line
    xy <- data.frame(BrthYear = mod[["model"]][["BrthYear"]],
                     cat = unique(filtered_data[["cat"]]),
                     qcode = unique(filtered_data[["qcode"]]),
                     qcodecat = unique(filtered_data[["qcodecat"]]),
                     total_anom = mod[["fitted.values"]],
                     total_lvb_yr = exp(mod[["model"]][["(offset)"]])*1000,
                     rate = mod[["fitted.values"]]/exp(mod[["model"]][["(offset)"]]),
                     model = "pois")
  }
  
  return(xy)
}
