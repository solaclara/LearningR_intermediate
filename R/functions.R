
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

#' Read all data
#'
#' @param filename 
#'
#' @returns readall
#' 
#'
read_all <- function(filename) {
  files <- here("data-raw/nurses-stress/") %>%
    fs::dir_ls(regexp = "HR.csv.gz", recurse = TRUE)
  
  data <- files %>%
    purrr::map(read) %>%
    purrr::list_rbind(names_to = "file_path_id")
  return(data)}

#' Get participant id
#'
#' @param ID 
#'
#' @returns ID
#' @export
#'
#' @examples
get_participant_id <- function(data){
  data_with_id <- data %>% 
  dplyr::mutate(
    id= stringr::str_extract(
      file_path_id,
      "/stress/[:alnum:]{2}/")%>% 
      stringr::str_remove("/stress/") %>% 
      stringr::str_remove("/"),
    .before = file_path_id) %>% 
    dplyr::select(-file_path_id)
  return(data_with_id)}

#' Summarizing data
#'
#' @param data 
#'
#' @returns

summarise_by_datetime <- function (data) {
  summarised_data <- data %>% 
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) %>%
    dplyr::summarise(
      dplyr::across(tidyselect::where(is.numeric), base::list(mean = mean, sd = sd, median = median)),
      .by = c(id, collection_datetime))
  return(summarised_data)
  
}



