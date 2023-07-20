#' @title Get affiliations
#' @param address The address to parse.
#' @param info The information to extract, one of c("university", "department").
#' @export
get_affiliation <- function(address, info = "university") {
  if (!info %in% c("university", "department")) {
    stop("Argument 'info' must be one of: university, department")
  }
  address <- gsub(".", ",", address, fixed = TRUE)
  addr.split <- stringr::str_split(address, ",")
  if (info == "university") {
    string.uni <- "University"
    uni <- split_address(addr.split, string.uni)
    string.uni2 <- paste(
      "University|Universit\u00e9|Universite|Universitat|Universit\u00e4t",
      "Universiteit|Universit\u00e0|College|School|Institute|Institut|Center|Centre",
      "CEMIC, CONICET|CNRS|INSEAD",
      sep = "|"
    )
    uni2 <- split_address(addr.split, string.uni2)
    out <- ifelse(!is.na(uni), uni, uni2)
  } else if (info == "department") {
    string.dep <- paste(
      "Department|Departamento|Dipartimento|Departament|Departement",
      "D\u00e9partement|Faculty|Center|School|Unit|Institute|Institut|Centre",
      "Division|Unidad",
      sep = "|"
    )
    out <- split_address(addr.split, string.dep)
  }
  out
}
