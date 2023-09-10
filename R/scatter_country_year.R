#' @title Generate table of journal paper percentages, by continent and year
#' @param data The processed dataframe of data
#' @param method Which method to use for the regression line, either "lm" (default) or "loess".
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
#'   year_low = 2018,
#'   year_high = 2020
#' )
#' data <- read_bind_all_data()
#' suppressWarnings(scatter_country_year(data))
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

scatter_country_year <- function(data, method = "lm") {
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

  df_country_year <- data %>%
    dplyr::group_by(.data$year, .data$country) %>%
    dplyr::filter(!is.na(.data$continent)) %>%
    dplyr::count(name = "Papers") %>%
    dplyr::mutate(percentage = as.numeric(round(.data$Papers / get_year_papers(
      data, .data$year
    ) * 100, 2)))

  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
  colours.country2 <- getPalette(length(unique(df_country_year$country)))

  df_country_year %>%
    dplyr::mutate(
      year = as.numeric(.data$year),
      country = as.factor(.data$country)
    ) %>%
    rempsyc::nice_scatter(
      predictor = "year",
      response = "percentage",
      group = "country",
      colours = colours.country2,
      method = "lm",
      groups.order = "decreasing",
      ytitle = "% of All Papers"
    ) %>%
    plotly::ggplotly(tooltip = c("x", "y"))
}

#' @noRd
get_year_papers <- function(data, year) {
  df_country_year_papers <- data %>%
    dplyr::filter(!is.na(.data$country)) %>%
    dplyr::count(.data$year, name = "Papers") %>%
    dplyr::arrange(dplyr::desc(.data$year), dplyr::desc(.data$Papers))

  df_country_year_papers[which(
    df_country_year_papers$year == year
  ), "Papers"]
}
