#' @title Like Excel vlookup/grep, both both ways
#' @param pattern The pattern to compare.
#' @param lookup_vector The dictionnary in which to look for the pattern.
#' @examples
#' address <- c(
#'   "Department of Psychology, Cornell University, Ithaca, New York 14853-7601.",
#'   "Dipartimento di Psicologia Generale, Università di Padova, Italy.",
#'   "Universität Mannheim, Federal Republic of Germany.",
#'   "Département de psychologie, Université du Québec à Montréal, Canada.")
#' partial_vlookup(address, universities$university)
#' uni <- c("Cornell University", "Università di Padova",
#'          "Universität Mannheim", "Université du Québec à Montréal")
#' partial_vlookup(uni, universities$university)
#' @export
partial_vlookup <- function(pattern, lookup_vector) {
  out <- purrr::map_chr(pattern, \(x) {
    out <- grep(x, lookup_vector, value = TRUE, fixed = TRUE)[1]
    if (is.na(out)) {
      out <- Ecfun::rgrep(lookup_vector, x, value = TRUE, fixed = TRUE)[1]
    }
    out
  })
  out
}
