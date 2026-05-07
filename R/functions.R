
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

#' Tidy data
#'
#' @param Tidy data 
#'
#' @returns Clean data 
#' 
#'
#'
tidy_survey_dates <- function(data) {
  tidied<- data %>% 
    dplyr::mutate(
      date = lubridate::mdy(date),
      start_datetime = lubridate::as_datetime(paste(date, start_time)),
      end_datetime = lubridate::as_datetime(paste(date, end_time)),
      datetime_id = start_datetime,
      .before = start_time
    ) |>
    dplyr::select(-c(date, start_time, end_time, duration))
  return(tidied)}

#' Pivot longer
#'
#' @param data 
#'
#' @returns Pivot longer

survey_to_long <- function(data) {
  longer <- data %>%
    dplyr::select(id, datetime_id, start_datetime, end_datetime) %>%
    tidyr::pivot_longer(c(start_datetime, end_datetime), names_to = NULL, values_to = "collection_datetime") %>%
    dplyr::group_by(pick(-collection_datetime)) %>%
    tidyr::complete(collection_datetime = seq(min(collection_datetime), max(collection_datetime), by = 60)) %>%
    dplyr::ungroup()
  return(longer)
}

