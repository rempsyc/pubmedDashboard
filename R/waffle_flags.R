#' @title Generate a waffle plot made of country flags
#' @param in_map_var The data
#' @param len_x Length on the x-axis
#' @param na_flag Which flag to use for missing values
#' @export
waffle_flags <- function(in_map_var, len_x = NA, na_flag = "ac") {
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
  return(p)
}
