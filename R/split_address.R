#' @export
split_address <- function(addr.split, string) {
  ind1 <- purrr::map(addr.split, ~which(grepl(string, .x)))
  ind1 <- purrr::imap(addr.split, \(x, idx) purrr::pluck(ind1[[idx]])[1])
  ind1 <- purrr::imap(addr.split, \(x, idx) purrr::pluck(x, ind1[[idx]]))
  ind1 <- trimws(as.character(ind1))
  ind1 <- replace(ind1, ind1 == "NULL", NA)
}
