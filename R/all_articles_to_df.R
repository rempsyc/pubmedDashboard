#' @title Convert list of PubMed XLM files to dataframe
#' @param d.fls The list of XLM PubMed data.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
all_articles_to_df <- function(d.fls) {
  lapply(seq_along(d.fls), function(x) {
    list.articles <- easyPubMed::articles_to_list(d.fls[[x]])
    list.articles.df <- lapply(list.articles, article_to_df2)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(.data$pmid, .keep_all = TRUE)
}
