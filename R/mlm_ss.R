#' Generating Simple Slopes for Two-Way Interactions in Two-Level Multilevel Models
#'
#' This function generates a list of two data frames: first, significance tests for simple slopes in
#' multilevel model interactions; second, intercepts and slopes for plotting these simple slopes.
#' The calculations behind this function are identical to the online calculator found at:
#' http://www.quantpsy.org/interact/mlr2.htm. The associated paper with these calculations can
#' be found at: http://www.quantpsy.org/pubs/preacher_curran_bauer_2006.pdf. Currently, this function
#' has been verified to work for two cases ONLY: first, "Case 1" (as specified in the Preacher et al., 2006 paper),
#' second, "Case 3", when the moderator is at Level-2 and the focual predictor at Level-1. Make sure your
#' categorical variables are numeric.
#' 
#' @param model An lmer object created by the lme4 package. The formula of fixed effects you specify must follow a specific
#' pattern: "dv ~ focal*moderator + (...)", where "dv" is the dependent variable, "focal" is the focal predictor, and "moderator"
#' is the moderating variable. Currently, this methodology does not handle additional covariates.
#' @param x2_1 A numeric value of the moderator for which you would like the simple slope of the focal variable.
#' @param x2_2 A numeric value of the moderator for which you would like the simple slope of the focal variable. Optional.
#' @param x2_3 A numeric value of the moderator for which you would like the simple slope of the focal variable. Optional.
#' @export
mlm_ss <- function(model, x2_1, x2_2=NA, x2_3=NA) {
  
  # get inputs
  g0 <- summary(model)$coef[1,1]
  g1 <- summary(model)$coef[2,1]
  g2 <- summary(model)$coef[3,1]
  g3 <- summary(model)$coef[4,1]
  
  var_g0 <- vcov(model)[1,1]
  var_g1 <- vcov(model)[2,2]
  var_g2 <- vcov(model)[3,3]
  var_g3 <- vcov(model)[4,4]
  
  cov_g0_g2 <- vcov(model)[1,3]
  cov_g1_g3 <- vcov(model)[2,4]
  
  # calculate simple slopes
  w1_1 <- g1 + g3*x2_1
  w1_2 <- g1 + g3*x2_2
  w1_3 <- g1 + g3*x2_3
  
  # calculate simple intercepts
  w0_1 <- g0 + g2*x2_1
  w0_2 <- g0 + g2*x2_2
  w0_3 <- g0 + g2*x2_3
  
  # calculate standard errors
  se_w1_1 <- sqrt(var_g1 + 2*x2_1*cov_g1_g3 + var_g3*x2_1^2)
  se_w1_2 <- sqrt(var_g1 + 2*x2_2*cov_g1_g3 + var_g3*x2_2^2)
  se_w1_3 <- sqrt(var_g1 + 2*x2_3*cov_g1_g3 + var_g3*x2_3^2)
  
  # calculate z-scores
  z_w1_1 <- w1_1 / se_w1_1
  z_w1_2 <- w1_2 / se_w1_2
  z_w1_3 <- w1_3 / se_w1_3
  
  # calculate p-values
  p_w1_1 <- 2*pnorm(-abs(z_w1_1))
  p_w1_2 <- 2*pnorm(-abs(z_w1_2))
  p_w1_3 <- 2*pnorm(-abs(z_w1_3))
  
  # make output table
  simple_slopes <- data.frame(moderator_value=c(x2_1, x2_2, x2_3),
                              simple_slope=c(w1_1, w1_2, w1_3),
                              std_error=c(se_w1_1, se_w1_2, se_w1_3),
                              z=c(z_w1_1, z_w1_2, z_w1_3),
                              p=c(p_w1_1, p_w1_2, p_w1_3))
  
  # make plotting table
  ablines <- data.frame(moderator_value=c(x2_1, x2_2, x2_3),
                        simple_intercept=c(w0_1, w0_2, w0_3),
                        simple_slope=c(w1_1, w1_2, w1_3))
  
  # return output
  return(list(simple_slopes=simple_slopes[complete.cases(simple_slopes),], 
              ablines=ablines[complete.cases(ablines),]))
}
