#' @importFrom dplyr %>%
#' @export
all_articles_to_df <- function(d.fls){
  lapply(seq_along(d.fls), function(x) {
    list.articles <- easyPubMed::articles_to_list(d.fls[[x]])
    list.articles.df <- lapply(list.articles, article_to_df2)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(pmid, .keep_all = TRUE)
}
