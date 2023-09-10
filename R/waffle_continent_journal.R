#' @title Generate a waffle chart of journal paper percentages, by continent (each square = 1% of data)
#' @param data The processed dataframe of data
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
#' waffle_continent_journal(data)
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

waffle_continent_journal <- function(data) {
  x <- data %>%
    dplyr::mutate(missing = sum(is.na(.data$continent)) / dplyr::n()) %>%
    dplyr::filter(!is.na(.data$continent)) %>%
    dplyr::group_by(.data$journal) %>%
    dplyr::summarize(
      `North America` = sum(.data$continent == "Northern America") / dplyr::n(),
      Europe = sum(.data$continent == "Europe") / dplyr::n(),
      Asia = sum(.data$continent == "Asia") / dplyr::n(),
      Oceania = sum(.data$continent == "Oceania") / dplyr::n(),
      `Latin America` = sum(.data$continent == "Latin America and the Caribbean") / dplyr::n(),
      Africa = sum(.data$continent == "Africa") / dplyr::n(),
    ) %>%
    dplyr::mutate(dplyr::across(2:6, ~ .x * 100)) %>%
    dplyr::arrange("journal") %>%
    tidyr::pivot_longer(-.data$journal, names_to = "continent", values_to = "number") %>%
    dplyr::mutate(continent = factor(.data$continent, levels = continent_order(short = TRUE)))

  colors <- suppressWarnings(RColorBrewer::brewer.pal(
    length(unique(data$continent)), "Set2"
  ))

  x %>%
    ggplot2::ggplot(ggplot2::aes(fill = .data$continent, values = .data$number)) +
    waffle::geom_waffle(color = "white", size = 0.8, na.rm = TRUE) +
    ggplot2::facet_wrap(~journal) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 6) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 5)
    ) +
    ggplot2::scale_fill_manual(values = colors)
}
