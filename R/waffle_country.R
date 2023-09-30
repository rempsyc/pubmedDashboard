#' @title Generate a waffle plot made of country flags
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
#' waffle_country(data)
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' unlink("articles_2023_2023.rds")
#' setwd(.old_wd)
#' }
#' @importFrom rlang .data
#' @importFrom ggplot2 layer
#' @export
waffle_country <- function(data) {
  . <- NULL
  x <- data %>%
    dplyr::mutate(missing = sum(is.na(.data$country)) / dplyr::n()) %>%
    dplyr::filter(!is.na(.data$country)) %>%
    dplyr::group_by(.data$country) %>%
    dplyr::add_count(name = "Papers") %>%
    dplyr::mutate(
      Percentage = .data$Papers / nrow(.),
      country = dplyr::case_when(
        .data$Percentage < 0.02 ~ "Other",
        TRUE ~ .data$country
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nrow = dplyr::n()) %>%
    dplyr::count(.data$country, .data$nrow, sort = TRUE, name = "Papers") %>%
    dplyr::mutate(Percentage = .data$Papers / nrow * 100) %>%
    dplyr::select(-c("nrow", "Papers")) %>%
    dplyr::rename_with(stringr::str_to_title, .cols = 1)

  colors <- suppressWarnings(RColorBrewer::brewer.pal(
    length(unique(x$continent)), "Set2"
  ))

  colours.country <- grDevices::colorRampPalette(colors)(length(x$Country))

  my_prop <- x %>%
    dplyr::mutate(
      Country = countrycode::countrycode(.data$Country, "country.name", "genc2c"),
      Country = tolower(.data$Country)
    ) %>%
    dplyr::filter(!is.na(.data$Country))
  in_map_var <- lapply(seq_len(nrow(my_prop)), \(x) {
    rep(my_prop$Country[x], my_prop$Percentage[x])
  }) %>%
    unlist()
  waffle_country_internal(in_map_var)
}

#' @noRd
waffle_country_internal <- function(in_map_var, len_x = NA, na_flag = "ac") {
  in_map_var <- data.frame(country = in_map_var)
  my_prop <- in_map_var %>%
    dplyr::count(.data$country, sort = TRUE) %>%
    dplyr::mutate(n2 = round(.data$n / nrow(in_map_var) * 100))
  in_map_var <- lapply(seq_len(nrow(my_prop)), \(x) {
    rep(my_prop$country[x], my_prop$n2[x])
  }) %>%
    unlist()
  # work out grid dimensions
  var_count <- length(in_map_var)
  if (is.na(len_x)) {
    x_count <- ceiling(sqrt(var_count))
  } else {
    x_count <- len_x
  }
  y_count <- ceiling(var_count / x_count)
  # y_count <- 10
  grid_count <- x_count * y_count
  df <-
    data.frame(
      x = rep(1:y_count, each = x_count),
      y = rep(1:x_count, y_count),
      country = c(in_map_var, rep(na_flag, grid_count - var_count))
    )
  country_4legend <- unique(df$country)[unique(df$country) != na_flag]
  p <-
    ggplot2::ggplot(df, ggplot2::aes(.data$x, .data$y, country = .data$country)) +
    ggflags::geom_flag(size = 8.5) +
    ggflags::scale_country(breaks = country_4legend) +
    ggplot2::theme_void() +
    ggplot2::coord_equal() +
    ggplot2::theme(legend.position = "right")
  if (grid_count > var_count) {
    p <-
      p +
      ggplot2::geom_point(
        data = df[var_count:grid_count, ],
        ggplot2::aes(.data$x, .data$y), colour = "white", size = 10
      )
  }
  p
}
