#' @title Download PubMed data with query string
#' @param pubmed_query_string The PubMed query string.
#' @param year_low The year the data should start.
#' @param year_high The year the data should end.
#' @param data_folder Where to save the data.
#' @param batch_size The download batch size.
#' @param api_key The api key for faster processing (optional).
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#' pubmed_query_string <- paste(
#'   "passion [Title/Abstract]",
#'   "AND Dualistic Model of Passion [Text Word]"
#' )
#'
#' batch_pubmed_download2(
#'   pubmed_query_string,
#'   year_low = 2023,
#'   year_high = 2023
#' )
#' }
#' \dontshow{
#' setwd(.old_wd)
#' }
#' @export
batch_pubmed_download2 <- function(pubmed_query_string,
                                   year_low = 2023,
                                   year_high = 2030,
                                   data_folder = "data",
                                   batch_size = 5000,
                                   api_key = NULL) {
  if (!dir.exists(data_folder)) {
    dir.create(data_folder)
    }

  easyPubMed::batch_pubmed_download(
    pubmed_query_string = paste(
      pubmed_query_string,
      paste0(
        "AND ('", year_low, "/01/01'[Date - Publication] : '",
        year_high, "/12/31'[Date - Publication])"
      )
    ),
    dest_file_prefix = paste0(data_folder, "/easyPubMed_data_"),
    api_key = api_key,
    batch_size = batch_size
  )
}
