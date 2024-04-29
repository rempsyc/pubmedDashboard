#' @title Generate a waffle chart of journal paper percentages, by continent (each square = 1% of data)
#' @param data The processed dataframe of data
#' @param citation Optionally, a citation to add as a footer.
#' @param citation_size Font size of the citation.
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
#' waffle_continent(data)
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

waffle_continent <- function(data, citation = NULL, citation_size = NULL) {
  insight::check_if_installed(c("waffle", "ggplot2"))
  x <- data %>%
    dplyr::mutate(missing = sum(is.na(.data$continent)) / dplyr::n()) %>%
    dplyr::filter(!is.na(.data$continent)) %>%
    dplyr::group_by(.data$continent) %>%
    dplyr::add_count(name = "Papers") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nrow = dplyr::n()) %>%
    dplyr::count(.data$continent, nrow, sort = TRUE, name = "Papers") %>%
    dplyr::mutate(
      continent = dplyr::case_match(
        .data$continent,
        continent_order()[1] ~ continent_order(short = TRUE)[1],
        continent_order()[5] ~ continent_order(short = TRUE)[5],
        .data$continent ~ .data$continent
      ),
      Percentage = .data$Papers / nrow * 100
    ) %>%
    dplyr::select(-c("nrow", "Papers")) %>%
    dplyr::rename_with(stringr::str_to_title, .cols = 1)

  p <- waffle::waffle(x, legend_pos = "right") +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 15))

  if (!is.null(citation)) {
    p <- gg_citation(p, citation, citation_size = citation_size)
  }

  p
}

gg_citation <- function(x, citation, citation_size) {
  insight::check_if_installed("ggtext")
  x +
    ggplot2::xlab(citation) +
    ggplot2::theme(axis.title.x = ggtext::element_markdown(
      hjust = 1, size = citation_size))
}
