#' @export
partial_vlookup <- function(pattern, lookup_vector) {
  out <- map_chr(pattern, \(x) {
    out <- grep(x, lookup_vector, value = TRUE, fixed = TRUE)[1]
    if (is.na(out)) {
      out <- Ecfun::rgrep(lookup_vector, x, value = TRUE, fixed = TRUE)[1]
    }
    out
  })
  out
}
