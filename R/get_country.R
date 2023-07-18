#' @export
get_country <- function(address, list.countries) {
  # Get country from countrycode
  country <- countrycode(address, "country.name", "country.name", warn = TRUE)
  if (is.na(country)) { # Get country from countrycode list of countries
    country <- rgrep(toTitleCase(tolower(list.countries)),
                     toTitleCase(tolower(address)),
                     value = TRUE, fixed = TRUE)[1]
    country <- case_match(country,
                          "Scotland" ~ "UK",
                          .default = country)
    if (is.na(country)) { # Get country from countrycode list of US states
      state <- rgrep(toTitleCase(tolower(cd$state.name)),
                     toTitleCase(tolower(address)),
                     value = TRUE, fixed = TRUE)[1]
      country <- ifelse(state %in% cd$state.name, "USA", NA)
      if (is.na(country)) { # Get country from list of US states (abbreviations)
        state <- rgrep(cd$state.abb, address, value = TRUE, fixed = TRUE)[1]
        country <- ifelse(state %in% cd$state.abb, "USA", NA)
        if (is.na(country)) { # Get country from capital city name
          state <- rgrep(world.cities_capitals$name, address, value = TRUE, fixed = TRUE)[1]
          country <- ifelse(is.na(state), state,
                            world.cities_capitals[world.cities_capitals$name == state, "country.etc"])
        }
      }
    }
    if (!is.na(country)) {
      country <- countrycode(country, "country.name", "country.name", warn = TRUE)
    }
  }
  country
}
