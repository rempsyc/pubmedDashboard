#' @importFrom dplyr %>%
#' @export
add_affiliation <- function(data) {
  data <- data %>%
    dplyr::mutate(department = get_affiliation(address, "department"),
                  university = get_affiliation(address, "university"))
  data <- data %>%
    dplyr::select(journal, year, university, department, address, lastname, firstname,
           month, day, jabbrv, title, doi, pmid, abstract)
  data
}
