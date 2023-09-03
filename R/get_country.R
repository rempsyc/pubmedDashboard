#' @title Get country
#' @param address The address to parse.
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' d.fls <- batch_pubmed_download2(
#'   pubmed_query_string = paste(
#'     "passion [Title/Abstract]",
#'     "AND Dualistic Model of Passion [Text Word]",
#'     "AND ('2023/01/01' [Date - Publication] : '2023/12/31' [Date - Publication])"
#'    ),
#'    year_low = 2023,
#'    year_high = 2023
#'  )
#' articles.df <- all_articles_to_df(d.fls)
#' articles.df2 <- add_affiliation(articles.df)
#' articles.df3 <- match_university(articles.df2)
#' get_country(articles.df3$address)
#' }
#' @export
get_country <- function(address) {
  if (inherits(address, "list")) {
    address <- address
  } else {
    address <- as.list(address)
  }
  get_country_internal <- function(address) {
    # Get country from countrycode
    country <- countrycode::countrycode(address, "country.name", "country.name", warn = TRUE)
    if (is.na(country)) { # Get country from countrycode list of countries
      country <- Ecfun::rgrep(tools::toTitleCase(tolower(pubmedDashboard::countries)),
        tools::toTitleCase(tolower(address)),
        value = TRUE, fixed = TRUE
      )[1]
      country <- dplyr::case_match(.data$country,
        "Scotland" ~ "UK",
        .default = .data$country
      )
      if (is.na(country)) { # Get country from countrycode list of US states
        state <- Ecfun::rgrep(tools::toTitleCase(tolower(pubmedDashboard::us_states$state.name)),
          tools::toTitleCase(tolower(address)),
          value = TRUE, fixed = TRUE
        )[1]
        country <- ifelse(state %in% pubmedDashboard::us_states$state.name, "USA", NA)
        if (is.na(country)) { # Get country from list of US states (abbreviations)
          state <- Ecfun::rgrep(pubmedDashboard::us_states$state.abb, address, value = TRUE, fixed = TRUE)[1]
          country <- ifelse(state %in% pubmedDashboard::us_states$state.abb, "USA", NA)
          if (is.na(country)) { # Get country from capital city name
            state <- Ecfun::rgrep(pubmedDashboard::world_capitals$name, address, value = TRUE, fixed = TRUE)[1]
            country <- ifelse(is.na(state), state,
              pubmedDashboard::world_capitals[pubmedDashboard::world_capitals$name == state, "country.etc"]
            )
          }
        }
      }
      if (!is.na(country)) {
        country <- countrycode::countrycode(country, "country.name", "country.name", warn = TRUE)
      }
    }
    country
  }
  out <- lapply(address, get_country_internal)
  unlist(out)
}
