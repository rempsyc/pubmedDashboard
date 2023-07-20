#' @title Add affiliations to a dataframe
#' @param data The dataframe on which to add affiliations (department and
#' university).
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
add_affiliation <- function(data) {
  data <- data %>%
    dplyr::mutate(
      department = get_affiliation(.data$address, "department"),
      university = get_affiliation(.data$address, "university")
    )
  data <- data %>%
    dplyr::select(
      dplyr::all_of(c(
        "journal", "year", "university", "department", "address",
        "lastname", "firstname", "month", "day", "jabbrv", "title",
        "doi", "pmid", "abstract"
    )))
  data
}
