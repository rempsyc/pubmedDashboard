#' @title Clean dataframe, for names of journals and continents
#' @param data The processed dataframe of data
#' @export
clean_journals_continents <- function(data) {
  data <- data %>%
    dplyr::mutate(
      continent = factor(.data$continent, levels = continent_order()),
      journal = gsub(":.*", "", .data$journal),
      journal = tools::toTitleCase(.data$journal),
      journal = trimws(.data$journal),
      field = journal_field$field[match(.data$journal, journal_field$journal)],
      original_journal = .data$journal %in% journal_field$journal[1:6]
    )
}

#' @noRd
continent_order <- function(short = FALSE) {
  if (short) {
    x <- c("North America", "Europe", "Asia", "Oceania", "Latin America", "Africa")
  } else {
    x <- c("Northern America", "Europe", "Asia", "Oceania", "Latin America and the Caribbean", "Africa")
  }
  x
}
