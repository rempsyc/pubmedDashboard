#' @title Clean dataframe, for names of journals and continents
#' @param data The processed dataframe of data
#' @export
clean_journals_continents <- function(data) {
  continent.order <- c("Northern America", "Europe", "Asia", "Oceania", "Latin America and the Caribbean", "Africa")
  continent.order.short <- c("North America", "Europe", "Asia", "Oceania", "Latin America", "Africa")
  data <- data %>%
    dplyr::mutate(
      continent = factor(.data$continent, levels = continent.order),
      journal = gsub(":.*", "", .data$journal),
      journal = tools::toTitleCase(.data$journal),
      journal = trimws(.data$journal)
    )
}
