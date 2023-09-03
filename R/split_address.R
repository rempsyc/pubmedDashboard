#' @title Extracts correct component from a splitted affiliation address
#' @param splitted.address The address splitted with [stringr::str_split].
#' @param string The keyword to determine which string component to keep.
#' @examples
#' address <- c("Department of Psychology",
#'              " Cornell University",
#'              " Ithaca",
#'              " New York 14853-7601.")
#'
#' extract_split_address(address, "University")
#' extract_split_address(address, "Department")
#' @export
extract_split_address <- function(splitted.address, string) {
  ind <- purrr::map(splitted.address, ~ which(grepl(string, .x)))
  ind <- purrr::imap(splitted.address, \(x, idx) purrr::pluck(ind[[idx]])[1])
  ind <- purrr::imap(splitted.address, \(x, idx) purrr::pluck(x, ind[[idx]]))
  ind <- trimws(as.character(ind))
  ind <- replace(ind, ind == "NULL", NA)
  ind
}
