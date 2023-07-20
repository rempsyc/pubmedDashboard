#' @title Split affiliation address into separate strings
#' @param address The address to split.
#' @param string The character used to split the address.
#' @export
split_address <- function(address, string) {
  ind <- purrr::map(address, ~ which(grepl(string, .x)))
  ind <- purrr::imap(address, \(x, idx) purrr::pluck(ind[[idx]])[1])
  ind <- purrr::imap(address, \(x, idx) purrr::pluck(x, ind[[idx]]))
  ind <- trimws(as.character(ind))
  ind <- replace(ind, ind == "NULL", NA)
  ind
}
