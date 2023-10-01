#' @title Clean dataframe, for names of journals and continents
#' @param data The processed dataframe of data
#' @export
clean_journals_continents <- function(data) {
  data <- data %>%
    dplyr::mutate(
      field = pubmedDashboard::journal_field$field[match(
        .data$journal, pubmedDashboard::journal_field$journal)],
      original_journal = .data$journal %in% pubmedDashboard::journal_field$journal[1:6],
      continent = factor(.data$continent, levels = continent_order()),
      journal = clean_journal_names(.data$journal)
      # journal = gsub(":.*", "", .data$journal),
      # journal = tools::toTitleCase(.data$journal),
      # journal = trimws(.data$journal),
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

#' @noRd
clean_journal_names <- function(journal) {
  x <- gsub(":.*", "", journal)
  x <- gsub("[(].*", "", x)
  x <- tools::toTitleCase(x)
  trimws(x)
}
