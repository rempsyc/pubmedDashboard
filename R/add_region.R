#' @title Add regions to pubmedDashboard dataframe
#' @param data The dataframe on which to add region.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' d.fls <- batch_pubmed_download2(
#'   pubmed_query_string = paste(
#'     "passion [Title/Abstract]",
#'     "AND Dualistic Model of Passion [Text Word]",
#'     "AND ('2023/01/01' [Date - Publication] : '2023/12/31' [Date - Publication])"
#'   ),
#'   year_low = 2023,
#'   year_high = 2023
#' )
#' articles.df <- all_articles_to_df(d.fls)
#' articles.df2 <- add_affiliation(articles.df)
#' articles.df3 <- match_university(articles.df2)
#' articles.df4 <- add_region(articles.df3)
#' articles.df4[2, ]
#' }
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
      .after = "country_code"
    ) %>%
    dplyr::mutate(
      date = paste(.data$year, .data$month, .data$day, sep = "-"),
      date = lubridate::as_date(.data$date)
    )
  data
}
