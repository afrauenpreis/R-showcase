#' Calculates the differences between two columns with data.table.
#'
#' @param data The data frame
#' @param col1 the column to be subtracted from
#' @param col2 the column to be subtracted by
#' the function will do col1 - col2
#'
#' @import data.table
#' @importFrom glue glue
#'
#' @return A data.table containing the differences between column i and l
#'
#' @export
#'
get_differences_dt <- function(data, col1, col2){
  
  diffs <- (data %>% pull({{col1}})) - (data %>% pull({{col2}}))
  
  labels <- glue("{col1}-{col2}")
  
  print(is.data.table(data))
  
  data <- data[, treat_diff := 10 ]
  data <- data[, `x1-x2` := "test"]
}
