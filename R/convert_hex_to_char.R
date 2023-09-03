#' @title Convert a Hex string to regular text
#' @param hex_string The address containg hex characters to convert.
#' @examples
#' address <- c(
#'   "D&#xe9;partement de Psychologie, Universit&#xe9; du Qu&#xe9;bec &#xe0; Montr&#xe9;al",
#'   "Department of Behavioural and Cognitive Sciences, University of Luxembourg"
#' )
#' convert_hex_to_char(address)
#'
#' @export
convert_hex_to_char <- function(hex_string) {
  out <- lapply(hex_string, convert_hex_to_char_internal)
  unlist(out)
}

convert_hex_to_char_internal <- function(hex_string2) {
  # Extract all hex codes from the string using stringi::stri_extract_all_regex
  hex_codes <- stringi::stri_extract_all_regex(hex_string2, "&#x[0-9a-fA-F]+;")[[1]]

  if (!all(is.na(hex_codes))) {
    # Convert hex codes to characters
    chars <- lapply(hex_codes, function(hex_code) {
      int_val <- strtoi(gsub("&#x(.*);", "\\1", hex_code), base = 16)
      char <- intToUtf8(int_val)
      return(char)
    })
    chars <- as.character(chars)

    # Replace hex codes with characters in the original string
    for (i in seq_along(hex_codes)) {
      hex_string2 <- sub(hex_codes[i], chars[i], hex_string2)
    }
  }
  hex_string2
}
