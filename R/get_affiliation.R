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
    string.uni2 <- "University|Université|Universite|Universitat|Universität|Universiteit|College|School|Institute|Institut|Center|Centre|CEMIC, CONICET|CNRS|INSEAD"
    uni2 <- split_address(addr.split, string.uni2)
    out <- ifelse(!is.na(uni), uni, uni2)
  } else if (info == "department") {
    string.dep <- "Department|Departamento|Departament|Departement|Faculty|Center|School|Unit|Institute|Institut|Centre|Division|Unidad"
    out <- split_address(addr.split, string.dep)
  }
  out
}
