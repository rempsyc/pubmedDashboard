#' @title Get affiliations
#' @param address The address to parse.
#' @param info The information to extract, one of c("university", "department").
#' @examples
#' address <- c(
#'   "Department of Psychology, Cornell University, Ithaca, New York 14853-7601.",
#'   "Dipartimento di Psicologia Generale, Università di Padova, Italy.",
#'   "Universität Mannheim, Federal Republic of Germany.",
#'   "Département de psychologie, Universit&#xe9; du Québec à Montréal, Canada."
#' )
#'
#' get_affiliation(address, "department")
#' get_affiliation(address, "university")
#'
#' @export
get_affiliation <- function(address, info = "university") {
  if (!info %in% c("university", "department")) {
    stop("Argument 'info' must be one of: university, department")
  }
  address <- gsub(".", ",", address, fixed = TRUE)
  addr.split <- stringr::str_split(address, ",")
  if (info == "university") {
    string.uni <- "University"
    uni <- extract_split_address(addr.split, string.uni)
    string.uni2 <- paste(
      "University|Universit\u00e9|Universite|Universitat|Universit\u00e4t",
      "Universiteit|Universit\u00e0|College|School|Institute|Institut|Center|Centre",
      "CEMIC, CONICET|CNRS|INSEAD",
      sep = "|"
    )
    uni2 <- extract_split_address(addr.split, string.uni2)
    out <- ifelse(!is.na(uni), uni, uni2)
  } else if (info == "department") {
    string.dep <- paste(
      "Department|Departamento|Dipartimento|Departament|Departement",
      "D\u00e9partement|Faculty|Center|School|Unit|Institute|Institut|Centre",
      "Division|Unidad",
      sep = "|"
    )
    out <- extract_split_address(addr.split, string.dep)
  }
  out
}
