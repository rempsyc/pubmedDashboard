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
#' waffle_country_journal(data)
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

waffle_country_journal <- function(data) {
  . <- NULL
  df_country_journal <- data %>%
    dplyr::filter(!is.na(.data$country)) %>%
    dplyr::group_by(.data$journal, .data$country) %>%
    dplyr::add_count(name = "Papers") %>%
    dplyr::mutate(
      percentage = .data$Papers / nrow(.),
      country = dplyr::case_when(
        .data$percentage < 0.01 ~ "Other",
        TRUE ~ .data$country
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::count(.data$journal, .data$country, sort = TRUE, name = "Papers") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      percentage = as.numeric(round(.data$Papers / get_journal_papers2(
        data, .data$journal
      ) * 100, 2)),
      country = as.factor(.data$country)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$journal), dplyr::desc(.data$Papers))

  colors <- suppressWarnings(RColorBrewer::brewer.pal(
    length(unique(data$continent)), "Set2"
  ))

  colours.country2 <- grDevices::colorRampPalette(colors)(length(unique(df_country_journal$country)))

  df_country_journal %>%
    ggplot2::ggplot(ggplot2::aes(fill = .data$country, values = .data$percentage)) +
    waffle::geom_waffle(color = "white", size = 0.8, na.rm = TRUE) +
    ggplot2::facet_wrap(~ .data$journal) +
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
    ggplot2::scale_fill_manual(values = colours.country2)
}

#' @noRd
get_journal_papers2 <- function(data, journal) {
  df_country_journal_missing2 <- data %>%
    dplyr::filter(!is.na(.data$country)) %>%
    dplyr::count(.data$journal, name = "Papers") %>%
    dplyr::arrange(dplyr::desc(.data$journal), dplyr::desc(.data$Papers))

  df_country_journal_missing2[which(
    df_country_journal_missing2$journal == journal
  ), "Papers"]
}
