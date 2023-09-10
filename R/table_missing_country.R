#' @title Generate table of journal paper percentages, by country
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
#' table_missing_country(data)
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

table_missing_country <- function(data, datatable = TRUE) {
  x <- data %>%
    dplyr::filter(is.na(.data$country)) %>%
    dplyr::select(-c("country_code":"region")) %>%
    dplyr::arrange(.data$address) %>%
    dplyr::mutate(doi = paste0("<a href='", .data$doi, "' target='_blank'>", .data$doi, "</a>"))

  if (isTRUE(datatable)) {
    x <- DT::datatable(x,
      extensions = "Responsive",
      options = list(iDisplayLength = 5),
      caption = "Journal paper percentages, by country",
      escape = FALSE
    )
  }
  x
}
