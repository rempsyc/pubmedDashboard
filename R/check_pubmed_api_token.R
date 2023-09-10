#' @title Check whether the PubMed API token is of the correct length and valid
#' @param API_TOKEN_PUBMED The PubMed API token
#' @examples
#' API_TOKEN_PUBMED <- c(
#'   "GNZuCGNZuCGNZuCGNZuCGNZuCtwed3twed32"
#' )
#'
#' check_pubmed_api_token(API_TOKEN_PUBMED)
#' @export
check_pubmed_api_token <- function(API_TOKEN_PUBMED) {
  if (API_TOKEN_PUBMED == "") stop(
    "API_TOKEN_PUBMED is an empty string. Terminating workflow.")
  else if (nchar(API_TOKEN_PUBMED) != 36) stop(
    "API_TOKEN_PUBMED is not 36 characters-long. Terminating workflow.")
  else print("API TOKEN OK")
}
