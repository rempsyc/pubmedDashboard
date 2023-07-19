#' @export
batch_pubmed_download2 <- function(pubmed_query_string,
                                   year_low = 2023,
                                   year_high = 2030,
                                   data_folder = "data",
                                   batch_size = 5000,
                                   api_key = NULL) {
  easyPubMed::batch_pubmed_download(
    pubmed_query_string = paste(
      pubmed_query_string,
      paste0("AND ('", year_low, "/01/01'[Date - Publication] : '",
             year_high, "/12/31'[Date - Publication])")),
    dest_file_prefix = paste0(data_folder, "/easyPubMed_data_"),
    api_key = api_key,
    batch_size = batch_size)
}
