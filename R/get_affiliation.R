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
      "University|Université|Universite|Universitat|Universität|Universiteit",
      "Università|College|School|Institute|Institut|Center|Centre",
      "CEMIC, CONICET|CNRS|INSEAD", sep = "|")
    uni2 <- split_address(addr.split, string.uni2)
    out <- ifelse(!is.na(uni), uni, uni2)
  } else if (info == "department") {
    string.dep <- paste(
      "Department|Departamento|Dipartimento|Departament|Departement",
      "Département|Faculty|Center|School|Unit|Institute|Institut|Centre",
      "Division|Unidad", sep = "|")
    out <- split_address(addr.split, string.dep)
  }
  out
}
