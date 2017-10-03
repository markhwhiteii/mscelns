#' Produce t-test summary with means, standard deviations, statistics, Cohen's d, and confidence intervals
#'
#' This function generates a table that includes variable names, means and standard deviations for each condition,
#' t-value and p-value, degrees of freedom, and Cohen's d with confidence intervals.
#' 
#' @param data A data frame.
#' @param dvs A variable name (or list of variable names) that are to be used as dependent variables.
#' @param iv A variable name for the condition. The corresponding variable in the data.frame should be a factor with two levels.
#' @param varequal A boolean value for if variances should be assumed to be equal across conditions or not.
#' @examples
#' data <- data.frame(
#'   a = c(rnorm(50, 0, 2), rnorm(50, 1, 2)),
#'   b = c(rnorm(50, 0, 2), rnorm(50, 1, 2)),
#'   c = c(rnorm(50, 0, 2), rnorm(50, 1, 2)),
#'   d = factor(c(rep("cond_0", 50), rep("cond_1", 50)))
#' )
#' t_table(data = data, dvs = c("a", "b", "c"), iv = "d")
#' @export
t_table <- function(data, dvs, iv, varequal=TRUE) {
  data <- as.data.frame(data)
  rows <- length(dvs)
  
  results <- data.frame(variable=rep(NA,rows), cond1_m=rep(NA,rows),
                        cond1_sd=rep(NA,rows), cond2_m=rep(NA,rows),
                        cond2_sd=rep(NA,rows), t=rep(NA,rows), 
                        p_value=rep(NA,rows), df=rep(NA,rows), 
                        d=rep(NA,rows), d_lb=rep(NA,rows), d_ub=rep(NA,rows))
  
  for (i in 1:length(dvs)) {
    dv <- data[,dvs[i]]
    cond <- data[,iv]
    t_obj <- t.test(dv ~ cond, var.equal=varequal)
    
    results$variable[i] <- dvs[i]
  
    results$cond1_m[i] <- round(tapply(dv, cond, mean, na.rm=TRUE)[[1]],2)
    results$cond1_sd[i] <- round(tapply(dv, cond, sd, na.rm=TRUE)[[1]],2)
    results$cond2_m[i] <- round(tapply(dv, cond, mean, na.rm=TRUE)[[2]],2)
    results$cond2_sd[i] <- round(tapply(dv, cond, sd, na.rm=TRUE)[[2]],2)
    
    results$t[i] <- round(unname(t_obj$statistic),2)
    results$p_value[i] <- t_obj$p.value
    results$df[i] <- unname(t_obj$parameter)
    
    es <- MBESS::ci.smd(ncp=unname(t_obj$statistic), n.1=table(cond)[[1]], n.2=table(cond)[[2]])
    results$d[i] <- round(es$smd,2)
    results$d_lb[i] <- round(es$Lower.Conf.Limit.smd,2)
    results$d_ub[i] <- round(es$Upper.Conf.Limit.smd,2)
  }
  names(results)[2] <- paste0(levels(cond)[1], "_m")
  names(results)[3] <- paste0(levels(cond)[1], "_sd")
  names(results)[4] <- paste0(levels(cond)[2], "_m")
  names(results)[5] <- paste0(levels(cond)[2], "_sd")
  
  return(results)
}
