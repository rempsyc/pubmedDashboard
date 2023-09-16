#' @title Generate a dygraph of journal paper percentages, by country and year
#' @param data The processed dataframe of data
#' @param level Level of analysis, either country or continent
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
#'   year_low = 2021,
#'   year_high = 2023
#' )
#' data <- read_bind_all_data()
#' dygraph_year(data)
#' dygraph_year(data, "country")
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' unlink("articles_2021_2023.rds")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

dygraph_year <- function(data, level = "continent") {
  df_country_year <- data %>%
    dplyr::group_by(.data$year, .data[[level]]) %>%
    dplyr::filter(!is.na(.data[[level]])) %>%
    dplyr::count(name = "Papers") %>%
    dplyr::mutate(percentage = as.numeric(round(.data$Papers / get_year_papers(
      data, .data$year
    ) * 100, 2)))

  # Time series dygraph
  q <- df_country_year %>%
    dplyr::ungroup() %>%
    dplyr::select("year", dplyr::all_of(level), "percentage") %>%
    dplyr::mutate(year = as.Date(.data$year, "%Y")) %>%
    tidyr::pivot_wider(names_from = level, values_from = "percentage") %>%
    xts::as.xts()

  q %>%
    dygraphs::dygraph() %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyUnzoom() %>%
    dygraphs::dyCrosshair(direction = "vertical") %>%
    dygraphs::dyOptions(strokeWidth = 3)
}
