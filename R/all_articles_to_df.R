#' @title Convert list of PubMed XLM files to dataframe
#' @param d.fls The list of XLM PubMed data.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' pubmed_query_string <- paste(
#'     "passion [Title/Abstract]",
#'     "AND Dualistic Model of Passion [Text Word]",
#'     "AND ('2023/01/01' [Date - Publication] : '2023/12/31' [Date - Publication])"
#'    )
#' d.fls <- batch_pubmed_download2(
#'   pubmed_query_string = pubmed_query_string,
#'    year_low = 2023,
#'    year_high = 2023
#'  )
#' articles.df <- all_articles_to_df(d.fls)
#' articles.df2[2, ]
#' }
#' @export
all_articles_to_df <- function(d.fls) {
  lapply(seq_along(d.fls), function(x) {
    list.articles <- easyPubMed::articles_to_list(d.fls[[x]])
    list.articles.df <- lapply(list.articles, article_to_df2)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(.data$pmid, .keep_all = TRUE)
}
