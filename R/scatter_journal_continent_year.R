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
#'   year_low = 2022,
#'   year_high = 2023
#' )
#' data <- read_bind_all_data()
#' suppressWarnings(scatter_journal_continent_year(data))
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

scatter_journal_continent_year <- function(data, method = "lm") {
  data <- data %>%
    dplyr::mutate(missing = sum(is.na(.data$continent)) / dplyr::n()) %>%
    dplyr::filter(!is.na(.data$continent)) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarize(
      `North America` = sum(.data$continent == "Northern America") / dplyr::n(),
      Europe = sum(.data$continent == "Europe") / dplyr::n(),
      Asia = sum(.data$continent == "Asia") / dplyr::n(),
      Oceania = sum(.data$continent == "Oceania") / dplyr::n(),
      `Latin America` = sum(.data$continent == "Latin America and the Caribbean") / dplyr::n(),
      Africa = sum(.data$continent == "Africa") / dplyr::n(),
    ) %>%
    dplyr::mutate(dplyr::across(2:6, ~ .x * 100)) %>%
    dplyr::arrange(.data$year) %>%
    tidyr::pivot_longer(-.data$year, names_to = "continent", values_to = "papers_percentage") %>%
    dplyr::mutate(
      year = as.numeric(.data$year), continent = factor(
        .data$continent,
        levels = continent_order(short = TRUE)
      ),
      papers_percentage = round(.data$papers_percentage)
    )

  colors <- suppressWarnings(RColorBrewer::brewer.pal(
    length(unique(data$continent)), "Set2"
  ))

  rempsyc::nice_scatter(
    data,
    predictor = "year",
    response = "papers_percentage",
    group = "continent",
    colours = colors,
    method = method,
    groups.order = "decreasing",
    ytitle = "% of All Papers"
  ) %>%
    plotly::ggplotly(tooltip = c("x", "y"))
}
