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
#'   year_low = 2022,
#'   year_high = 2023
#' )
#' data <- read_bind_all_data()
#' suppressWarnings(scatter_continent_year(data))
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

scatter_continent_year <- function(data, method = "lm", plotly = TRUE, citation, citation_size = 15, ...) {
  insight::check_if_installed("RColorBrewer")
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

  p <- rempsyc::nice_scatter(
    data,
    predictor = "year",
    response = "papers_percentage",
    group = "continent",
    colours = colors,
    method = method,
    groups.order = "decreasing",
    ytitle = "% of All Papers",
    ...
  )

  if (isTRUE(plotly)) {
    insight::check_if_installed("plotly")
    p <- plotly::ggplotly(tooltip = c("x", "y"))
    if (!is.null(citation)) {
      p <- plotly_citation(p, citation, citation_size = citation_size)
    }
  } else if (!is.null(citation)) {
    p <- gg_citation(p, citation, citation_size = citation_size)
  }

  p
}

plotly_citation <- function(x, citation, citation_size) {
  plotly::layout(
    x,
    annotations = list(
      # x = 2020,
      y = 100,
      text = citation2,
      showarrow = F,
      # xref = 'container',
      yref = 'container',
      xanchor = 'left',
      yanchor = 'top',
      # yshift = -1,
      # automargin = TRUE,
      # margin = list(b=90),
      font = list(size = citation_size))
  )
}
