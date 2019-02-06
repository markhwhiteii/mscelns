#' Make Cross Tabulations, Possibly with Weights, and Return as Tidy Data Frame
#' 
#' Behaves very similarly to the descr::crosstab function, but it does not round
#' weighted group sizes to the nearest whole number.  Other CrossTab functions 
#' did not return values in a tidy data frame, so I adapted a dplyr/tidyr pipe 
#' chain to do so.
#' 
#' @param data A data.frame or tibble.
#' @param out Output variable name, as string.
#' @param preds Vector of predictor variable names, as string.
#' @param wt Weight variable name, as string.
#' @param form Format of resulting tibble. t for tidy (data analysis) or p for 
#' pretty (presentation).
#' @return A tibble of counts and proportions of out, perhaps at numerous levels
#' or combinations of levels of preds.
#' @examples 
#' set.seed(1839)
#' n <- 500
#' w <- rgamma(n, 1)
#' data <- data.frame(
#'   y = sample(letters[1:2], n, TRUE),
#'   x1 = sample(letters[5:8], n, TRUE),
#'   x2 = sample(letters[9:12], n, TRUE),
#'   w = w + (1 - mean(w))
#' )
#'
#' cross_tab(data, "y", c("x1", "x2"), "w", "p")
#' @import dplyr
#' @export
cross_tab <- function(data, out, preds = NULL, wt = NULL, form = c("t", "p")) {
  
  if (is.null(preds)) {
    result <- data %>% 
    {
      if (is.null(wt)) count_(., out) 
      else count_(., out, wt = wt) 
    } %>% 
      mutate(prop = n / sum(n))
  }
  
  if (!is.null(preds)) {
    result <- data %>% 
    {
      if (is.null(wt)) count_(., c(out, preds)) 
      else count_(., c(out, preds), wt = wt) 
    } %>% 
      group_by_(.dots = preds) %>% 
      mutate(prop = n / sum(n), n = sum(n))
  }
  
  if (match.arg(form) == "p" & !is.null(preds)) {
    result <- result %>% 
      tidyr::spread_(out, "prop", fill = 0)
  }
  
  return(ungroup(result))
  
}
