#' @title Generate table of journal paper percentages, by continent and year
#' @param data The processed dataframe of data
#' @param method Which method to use for the regression line, either "lm" (default) or "loess".
#' @param plotly Logical, whether to use plotly for dynamic data visualization.
#' @param citation Optionally, a citation to add as a footer.
#' @param citation_size Font size of the citation.
#' @param ... Further arguments passed to [rempsyc::nice_scatter]
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

scatter_country_year <- function(data,
                                 method = "lm",
                                 plotly = TRUE,
                                 citation = NULL,
                                 citation_size = 15,
                                 ...) {
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

  p <- df_country_year %>%
    dplyr::mutate(
      year = as.numeric(.data$year),
      country = as.factor(.data$country)
    ) %>%
    rempsyc::nice_scatter(
      predictor = "year",
      response = "percentage",
      group = "country",
      colours = colours.country2,
      method = method,
      groups.order = "decreasing",
      ytitle = "% of All Papers",
      ...
    )

  if (isTRUE(plotly)) {
    insight::check_if_installed("plotly")
    x <- plotly::ggplotly(tooltip = c("x", "y"))
    if (!is.null(citation)) {
      p <- plotly_citation(p, citation, citation_size = citation_size)
    }
  } else if (!is.null(citation)) {
    p <- gg_citation(p, citation, citation_size = citation_size)
  }

  p
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
