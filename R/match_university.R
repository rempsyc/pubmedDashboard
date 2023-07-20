#' @title Match list of universities to countries in a dataframe
#' @param data The dataframe to use for matching.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
match_university <- function(data) {
  data <- data %>%
    dplyr::mutate(
      university_old = .data$university,
      university = partial_vlookup(.data$university,
                                   pubmedDashboard::universities$university),
      university = ifelse(is.na(.data$university), partial_vlookup(
        .data$address, pubmedDashboard::universities$university),
        .data$university),
      .after = .data$university
    )
  data <- data %>%
    dplyr::left_join(.data$universities, by = "university", multiple = "first") %>%
    dplyr::relocate(.data$country_code, .after = "year")
  data
}
