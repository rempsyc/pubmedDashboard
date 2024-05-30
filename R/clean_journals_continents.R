#' @title Clean dataframe, for names of journals and continents
#' @param data The processed dataframe of data
#' @export
clean_journals_continents <- function(data) {
  data <- data %>%
    dplyr::mutate(
      original_journal = .data$journal %in% pubmedDashboard::journal_field$journal[1:6],
      journal = clean_journal_names(.data$journal),
      field = pubmedDashboard::journal_field$field[match(
        toupper(.data$journal), toupper(pubmedDashboard::journal_field$journal)
      )],
      field = ifelse(is.na(.data$field), pubmedDashboard::journal_field$field[match(
        toupper(.data$journal), toupper(pubmedDashboard::journal_field$journal_short))], .data$field),
      continent = factor(.data$continent, levels = continent_order())
    ) %>%
    dplyr::group_by(.data$journal) %>%
    dplyr::mutate(first_Year = min(.data$year), last_year = max(.data$year),
           year_range = paste0(.data$first_Year, "-", .data$last_year)) %>%
    dplyr::ungroup()
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
  x <- gsub("&amp;", "&", journal, fixed = TRUE)
  x <- gsub(" of the United States of America", "", x, fixed = TRUE)
  x <- gsub(":.*", "", x)
  x <- gsub("[(].*", "", x)
  x <- tools::toTitleCase(x)
  trimws(x)
}

#' @title Detect missing journals
#' @param data The processed dataframe of data
#' @export
detect_missing_journals <- function(data) {
  data.frame(journal = pubmedDashboard::journal_field$journal_short) %>%
    dplyr::mutate(found = toupper(pubmedDashboard::journal_field$journal_short) %in%
                    toupper(clean_journal_names(unique(data$journal))),
                  found = ifelse(.data$found == FALSE, toupper(pubmedDashboard::journal_field$journal_short) %in%
                                   toupper(clean_journal_names(unique(data$journal))), .data$found)) %>%
    dplyr::arrange(.data$found)
}

#' @title Count number of papers per journal, with year range
#' @param data The processed dataframe of data
#' @param datatable Whether to output a [DT::datatable] HTML table widget
#'  instead of a regular dataframe (defaults to TRUE).
#' @export
table_journal_count <- function(data, datatable = TRUE) {
  x <- dplyr::count(data, .data$journal, .data$field, .data$year_range, sort = TRUE) %>%
    dplyr::mutate(field = stringr::str_to_title(.data$field)) %>%
    as.data.frame() %>%
    dplyr::rename("year range" = "year_range") %>%
    dplyr::rename_with(stringr::str_to_title)
  if (isTRUE(datatable)) {
    insight::check_if_installed("DT")
    x <- DT::datatable(
      x,
      options = list(searching = TRUE, paging = TRUE),
      caption = "Count of journals, with year range")
  }
  x
}

