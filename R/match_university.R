#' @export
match_university <- function(data) {
  data <- data %>%
    dplyr::mutate(
      university_old = university,
      university = partial_vlookup(university, universities$university),
      university = ifelse(is.na(university), partial_vlookup(address, universities$university), university),
      .after = university)
  data <- data %>%
    dplyr::left_join(universities, by = "university", multiple = "first") %>%
    dplyr::relocate(country_code, .after = year)
  data
}
