#' @importFrom dplyr %>%
#' @export
add_region <- function(data) {
  data <- data %>%
    dplyr::mutate(country = countrycode::countrycode(country_code, "genc2c", "country.name"),
                  country = ifelse(is.na(country), get_country(address), country),
                  country_code = ifelse(
                    is.na(country_code),
                    countrycode::countrycode(country, "country.name", "genc2c"),
                    country_code),
                  region = countrycode::countrycode(
                    country_code, "genc2c", "un.regionsub.name"),
                  continent = countrycode::countrycode(
                    country_code, "genc2c", "continent"),
                  continent = dplyr::case_when(continent == "Americas" ~ region,
                                               TRUE ~ continent),
                  doi = paste0("https://doi.org/", doi),
                  .after = country_code) %>%
    dplyr::mutate(date = paste(year, month, day, sep = "-"),
                  date = lubridate::as_date(date))
  data
}
