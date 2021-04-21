#' Master function that gets p-values for permuted data.
#'
#' @param data The data frame
#' @param taxon The taxonomic level being tested on
#' @param n the number of permutations
#' @param ... vector of treatments to be compared to one another
#'
#' @import dplyr
#' @import dtplyr
#' @import magrittr
#'
#' @return A dataframe containing p-values for the permuted data arranged in ascending order by p-val
#'
#' @export
#'
get_perm_pvals <- function(data, taxon, n, ...){
  col_vars <- quos(...)
  k = length(col_vars)
  pval_list <- vector(mode = "list", length = (k * (k - 1) / 2))
  idx = 1 #length is set to the number of total pairwise comparisons
  for(i in 1:(k-1)){          #iterates across list of treatments
    for(l in (i+1):k){    #iterates across list of treatments again
      pval_list[idx] <- data %>%
        get_differences(col1 = col_vars[i], col2 = col_vars[l]) %>%      #calculates differences between two treatments
        calculate_pvals({{taxon}}, n)                                    #calculates p-values for each comparison
      idx <- idx + 1
    }
  }
  pval_data <- rbind.fill(pval_list)      #combines all of the dataframes
  return(pval_data)
}
