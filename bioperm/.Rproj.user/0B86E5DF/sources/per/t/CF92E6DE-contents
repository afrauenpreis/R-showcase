#' Calculates the p-values for the permuted data.frame.
#'
#' @param data The data table
#' @param class The taxonomic level being tested on
#' @param n the number of permutations
#'
#' @return A data.table containing p-values for the permuted data arranged in ascending order by p-val
#'
#' @import data.table
#'
#' @export
#'

calculate_pvals_dt <- function(data, taxon, n){
  data <- data[, `:=` (p_val = (sum(abs(treat_diff) >= abs(treat_diff[1]))-1)/n,
                       treat_diff = treat_diff[1],
                       `x1-x2` = `x1-x2`[1]), by = {{taxon}}]
}
