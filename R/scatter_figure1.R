#' @title Generate table of journal paper percentages, by continent and year
#' @param data The processed dataframe of data
#' @param method Which method to use for the regression line, either "lm" (default) or "loess".
#' @param original If `TRUE`, attempts to mimic Arnett's (2008) Figure 1 in style.
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#' pubmed_query_string <- paste(
#'   "Developmental Psychology [Journal]",
#'   "OR Journal of Personality and Social Psychology [Journal]",
#'   "OR Journal of Abnormal Psychology [Journal]",
#'   "OR Journal of Family Psychology [Journal]",
#'   "OR Health Psychology [Journal]",
#'   "OR Journal of Educational Psychology [Journal]"
#' )
#'
#' save_process_pubmed_batch(
#'   pubmed_query_string,
#'   year_low = 2023,
#'   year_high = 2023
#' )
#' data <- read_bind_all_data()
#' scatter_figure1(data)
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @export

scatter_figure1 <- function(data, method = "lm", original = TRUE) {
  df_journal_year <- data %>%
    dplyr::filter(!is.na(.data$country)) %>%
    dplyr::group_by(.data$year, .data$journal) %>%
    dplyr::count()

  journal.order <- c("DP", "JPSP", "JAP", "JFP", "HP", "JEP")

  df_us_journal_year <- data %>%
    dplyr::filter(
      !is.na(.data$country),
      .data$country_code == "US"
    ) %>%
    dplyr::group_by(.data$year, .data$journal) %>%
    dplyr::count(name = "n_us") %>%
    dplyr::full_join(df_journal_year, by = dplyr::join_by("year", "journal")) %>%
    dplyr::mutate(
      percentage_american = round(.data$n_us / .data$n * 100, 2),
      year = as.numeric(.data$year),
      journal = tolower(.data$journal),
      journal = dplyr::case_match(
        .data$journal,
        "developmental psychology" ~ "DP",
        "journal of personality and social psychology" ~ "JPSP",
        "journal of abnormal psychology" ~ "JAP",
        "journal of family psychology" ~ "JFP",
        "health psychology" ~ "HP",
        "journal of educational psychology" ~ "JEP"
      ),
      journal = factor(.data$journal, levels = journal.order)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$journal))

  df_us_journal_year_temp <- df_us_journal_year %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarize(
      journal = "Total",
      n_us = sum(.data$n_us),
      n = sum(.data$n),
      percentage_american = round(.data$n_us / .data$n * 100, 2)
    )

  XData <- rbind(df_us_journal_year, df_us_journal_year_temp)

  if (original) {
    fig1 <- rempsyc::nice_scatter(
      XData,
      predictor = "year",
      response = "percentage_american",
      group = "journal",
      ymin = 30,
      ymax = 100,
      yby = 10,
      # colours = rep("black", 6),
      # has.points = FALSE,
      has.linetype = TRUE,
      has.shape = TRUE,
      has.line = FALSE,
      alpha = 1,
      method = method,
      ytitle = "% American First Authors"
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point(fill = "black", ggplot2::aes(size = .data$journal)) +
      ggplot2::scale_colour_manual(values = rep("black", 7), name = "") +
      ggplot2::scale_shape_manual(values = c(18, 17, 17, 23, 19, 20, 15), name = "") +
      ggplot2::scale_size_manual(values = c(2, 2, 4, 4, 4, 2, 4), name = "") +
      ggplot2::labs(fill = "", linetype = "") +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "black", linewidth = 0.5))

    fig1 <- plotly::ggplotly(fig1, tooltip = c("x", "y"))

    for (i in 1:length(fig1$x$data)) {
      if (!is.null(fig1$x$data[[i]]$name)) {
        fig1$x$data[[i]]$name <- gsub("\\(", "", stringr::str_split(fig1$x$data[[i]]$name, ",")[[1]][1])
      }
    }
  } else {
    fig1 <- rempsyc::nice_scatter(
      XData,
      predictor = "year",
      response = "percentage_american",
      group = "journal",
      method = "lm",
      ytitle = "% American First Authors"
    )
  }

  fig1
}
