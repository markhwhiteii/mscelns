#' Get Rake Weights, Given Data and Target Population Margins
#' 
#' Get rake weights, given data.frame objects representing the actual data
#' as well as the population marginal targets that one wishes to weight to.
#' Raking will be done marginally across variables specified. 
#' Note that missing values are deleted listwise. Any cases having missing
#' values in any of the variable listed in the targets object will be removed.
#' 
#' @param data Individual-level data.frame. Each row is unit of observation,
#' each column is a variable. The names of the variables in targets must be
#' in the data verbatim. Note that NAs are treated with listwise deletion.
#' @param targets Data.frame, containing three columns. The first column
#' must be the names of the variables on which one wants to weight. The second
#' column must be all the values of those variables. The third column must be
#' the proportion population margins that these values of these variables
#' should be weighted to. The levels must match the levels in the data. See
#' the example targets object for an illustration.
#' @param na_warn Logical. Do you want to be warned when cases are removed
#' due to missing values?
#' @param ... Arguments passed to survey::rake
#' @examples
#' set.seed(1839)
#' n <- 200
#' gender <- c(rep("f", n * .3), rep("m", n * .7))
#' party <- c(rep("r", n * .4), rep("d", n * .6))
#' data <- data.frame(
#'   gender = gender[order(runif(n))],
#'   party = party[order(runif(n))]
#' )
#' targets <- data.frame(
#' variable = c("gender", "gender", "party", "party"),
#'   value = c("f", "m", "d", "r"),
#'   pop_margin = .5
#' )
#' get_weights(data, targets)
#' @export
get_weights <- function(data, targets, na_warn = TRUE, ...) {
  
  if (!is.double(targets[[3]])) {
    stop("targets third column must be double, containing population margins.")
  }
  
  if (ncol(targets) != 3) {
    stop("targets must have three and only three columns.")
  }
  
  test <- setdiff(as.character(targets[[1]]), colnames(data))
  if (length(test) > 0) {
    stop(paste(test, collapse = ", "), " are not columns in data.")
  }
  
  targets <- targets[order(targets[[1]]), ]
  t_vars <- as.character(unique(targets[[1]]))
  test <- nrow(data)
  data <- data[complete.cases(data[, t_vars]), t_vars]
  n_obs <- nrow(data)
  
  if ((test != n_obs) & na_warn) {
    warning(test - n_obs, " cases removed due to missing values.")
  }
  
  if (any(!sapply(data, is.factor))) {
    data <- as.data.frame(lapply(data, as.factor))
  }
  
  dat_levels <- lapply(data, levels)
  test <- lapply(names(dat_levels), function(x) {
    setdiff(dat_levels[[x]], as.character(targets[targets[[1]] == x, 2]))
  })
  if (any(sapply(test, length) > 0)) {
    names(test) <- names(dat_levels)
    test <- test[sapply(test, length) > 0]
    err <- unlist(lapply(names(test), function(x) {
      paste0(x, ": ", test[[x]], "\n")
    }))
    stop("Values appear in data that are not in population targets:\n", err)
  }
  
  if (length(t_vars) != sum(targets[[3]])) {
    sums <- tapply(targets[[3]], targets[[1]], sum)
    sums <- sums[sums != 1]
    err <- paste0(names(sums), ": ", round(sums, 3), "\n")
    stop("Some of your population targets do not sum to 100%:\n", err)
  }
  
  targets <- lapply(t_vars, function(x) {
    tmp <- targets[targets[[1]] == x, 2:3]
    names(tmp) <- c(x, "Freq")
    tmp$Freq <- tmp$Freq * n_obs
    tmp
  })
  
  formulas <- lapply(t_vars, function(x) as.formula(paste0("~ ", x)))
  result <- invisible(suppressWarnings(survey::svydesign(~ 1, data = data)))
  result <- survey::rake(result, formulas, targets, ...)
  
  return(weights(result))
  
}
