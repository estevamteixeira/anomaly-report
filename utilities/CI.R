ci_rate <- function(data, alpha = 0.05){
  dta <- data

  if(min(tapply(dta[["total_anom"]],
            dta[["cat"]],
            function(x) min(x))) < 30){
    dta$lb <- qchisq(p = alpha/2,
                 df = 2*dta$total_anom)/2
    dta$ub <- qchisq(p = 1-alpha/2,
                 df = 2*(dta$total_anom+1))/2
    dta$ll <- 1000*dta$lb/dta$total_lvb_yr
    dta$ul <- 1000*dta$ub/dta$total_lvb_yr_yr
  } else{
    dta$ll <- 1000*dta$total_anom/dta$total_lvb_yr*(1-(1/(9*dta$total_anom)) + (qnorm(alpha/2)/3)*sqrt(1/dta$total_anom))^(1/3)
    dta$ul <- 1000*(dta$total_anom+1)/dta$total_lvb_yr*(1-(1/(9*(dta$total_anom+1))) + (qnorm(1-alpha/2)/3)*sqrt(1/(dta$total_anom+1)))^(1/3)
  }
  
  return(dta)
}