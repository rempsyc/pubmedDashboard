#' @export
get_country <- function(address) {
  # Get country from countrycode
  country <- countrycode::countrycode(address, "country.name", "country.name", warn = TRUE)
  if (is.na(country)) { # Get country from countrycode list of countries
    country <- Ecfun::rgrep(tools::toTitleCase(tolower(countries)),
                            tools::toTitleCase(tolower(address)),
                            value = TRUE, fixed = TRUE)[1]
    country <- dplyr::case_match(country,
                                 "Scotland" ~ "UK",
                                 .default = country)
    if (is.na(country)) { # Get country from countrycode list of US states
      state <- Ecfun::rgrep(tools::toTitleCase(tolower(us_states$state.name)),
                            tools::toTitleCase(tolower(address)),
                            value = TRUE, fixed = TRUE)[1]
      country <- ifelse(state %in% us_states$state.name, "USA", NA)
      if (is.na(country)) { # Get country from list of US states (abbreviations)
        state <- Ecfun::rgrep(us_states$state.abb, address, value = TRUE, fixed = TRUE)[1]
        country <- ifelse(state %in% us_states$state.abb, "USA", NA)
        if (is.na(country)) { # Get country from capital city name
          state <- Ecfun::rgrep(world_capitals$name, address, value = TRUE, fixed = TRUE)[1]
          country <- ifelse(is.na(state), state,
                            world_capitals[world_capitals$name == state, "country.etc"])
        }
      }
    }
    if (!is.na(country)) {
      country <- countrycode::countrycode(country, "country.name", "country.name", warn = TRUE)
    }
  }
  country
}
