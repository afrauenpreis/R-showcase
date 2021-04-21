#' Pulls out and combine permutations from the permutation dataframe made by the permutations function by Rsample.
#'
#' @param data The permutations data frame
#'
#' @return A dataframe the permuted data with permutation ID labels
#'
#' @import plyr
#' @import dplyr
#'
#' @export
#'
add_Perm_ID <- function(perm_dat){
  perm_list <- vector(mode = "list", length = nrow(perm_dat))
  for(i in 1:nrow(perm_dat)){
    perm_list[[i]] <- perm_dat$splits[[i]] %>%
      analysis() %>%
      mutate(Perm = perm_dat$id[i]) %>%
      mutate_if(is.numeric, ~replace_na(., 0)) %>%              # replaces NA values with 0
      group_by(Class, season, treatment, Perm) %>%
      dplyr::summarise(Count = mean(Count))
  }
  new_perm <- rbind.fill(perm_list) %>%
    pivot_wider(names_from = treatment, values_from = Count)
  new_perm <- new_perm[order(new_perm$Perm),]
  return(new_perm)
}
