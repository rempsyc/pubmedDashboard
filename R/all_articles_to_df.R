#' @export
all_articles_to_df <- function(d.fls){
  y <- lapply(seq_along(d.fls), function(x) {
    list.articles <- easyPubMed::articles_to_list(d.fls[x])
    list.articles.df <- lapply(list.articles, article_to_df2)
    articles.df <- do.call(rbind, list.articles.df)
  })
  z <- do.call(rbind, y)
  filter(z, !duplicated(pmid))
}
