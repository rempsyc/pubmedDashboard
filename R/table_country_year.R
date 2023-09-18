#' @title Generate table of journal paper percentages, by country and year
#' @param data The processed dataframe of data
#' @param datatable Whether to output a [DT::datatable] HTML table widget
#'  instead of a regular dataframe (defaults to TRUE).
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#' pubmed_query_string <- paste(
#'   "passion [Title/Abstract]",
#'   "AND Dualistic Model of Passion [Text Word]"
#' )
#'
#' save_process_pubmed_batch(
#'   pubmed_query_string,
#'   year_low = 2023,
#'   year_high = 2023
#' )
#' data <- read_bind_all_data()
#' table_country_year(data)
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

table_country_year <- function(data, datatable = TRUE) {
  x <- data %>%
    dplyr::group_by(.data$year, .data$country) %>%
    dplyr::filter(!is.na(.data$continent)) %>%
    dplyr::count(name = "Papers") %>%
    dplyr::mutate(percentage = as.numeric(round(.data$Papers / get_year_papers(data, .data$year) * 100, 2)))

  df_country_year_missing <- data %>%
    dplyr::filter(is.na(.data$country)) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::count(.data$year, name = "Papers") %>%
    dplyr::arrange(dplyr::desc(.data$year), dplyr::desc(.data$Papers)) %>%
    dplyr::left_join(by = "year", data %>%
      dplyr::group_by(.data$year) %>%
      dplyr::count(.data$year, name = "all_papers") %>%
      dplyr::arrange(dplyr::desc(.data$year))) %>%
    dplyr::mutate(
      percentage = round(.data$Papers / .data$all_papers * 100, 2),
      country = "Missing*"
    ) %>%
    dplyr::select(-"all_papers")

  x <- x %>%
    dplyr::ungroup() %>%
    dplyr::add_row(df_country_year_missing) %>%
    dplyr::arrange(dplyr::desc(.data$year), dplyr::desc(.data$Papers))

  if (isTRUE(datatable)) {
    x <- DT::datatable(x,
      caption = "Journal paper percentages, by country and year"
    )
  }
  x
}
