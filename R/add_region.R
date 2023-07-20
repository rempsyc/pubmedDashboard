#' @title Add regions
#' @param data The dataframe on which to add region.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
add_region <- function(data) {
  data <- data %>%
    dplyr::mutate(
      country = countrycode::countrycode(.data$country_code, "genc2c", "country.name"),
      country = ifelse(is.na(.data$country), get_country(.data$address), .data$country),
      country_code = ifelse(
        is.na(.data$country_code),
        countrycode::countrycode(.data$country, "country.name", "genc2c"),
        .data$country_code
      ),
      region = countrycode::countrycode(
        .data$country_code, "genc2c", "un.regionsub.name"
      ),
      continent = countrycode::countrycode(
        .data$country_code, "genc2c", "continent"
      ),
      continent = dplyr::case_when(
        .data$continent == "Americas" ~ .data$region,
        TRUE ~ .data$continent
      ),
      doi = paste0("https://doi.org/", .data$doi),
      .after = .data$country_code
    ) %>%
    dplyr::mutate(
      date = paste(.data$year, .data$month, .data$day, sep = "-"),
      date = lubridate::as_date(.data$date)
    )
  data
}
