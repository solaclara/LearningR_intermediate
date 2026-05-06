
#' Read into nurses data
#'
#' @param file_path 
#' @param max_rows 
#'
#' @returns A table
#' 
#'
#' @examples
read <- function(file_path, max_rows = 100) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}