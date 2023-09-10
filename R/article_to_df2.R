#' @title Convert PubMed XLM files to dataframe
#' @param pubmedArticle The PubMed data, in XLM form.
#' @param autofill Autofill the affiliation address when missing.
#' @param max_chars Maximum number of characters for the address.
#' @param getKeywords Whether to extract keywords as well.
#' @param getAuthors Whether to extract the author information.
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#' dami_query <- paste(
#'   "passion [Title/Abstract]",
#'   "AND Dualistic Model of Passion [Text Word]",
#'   "AND ('2023/01/01' [Date - Publication] : '2023/12/31' [Date - Publication])"
#' )
#' dami_on_pubmed <- easyPubMed::get_pubmed_ids(dami_query)
#' dami_abstracts_xml <- easyPubMed::fetch_pubmed_data(dami_on_pubmed)
#' dami_abstracts_list <- easyPubMed::articles_to_list(dami_abstracts_xml)
#' article_to_df2(pubmedArticle = dami_abstracts_list[[2]], autofill = FALSE)[1, ]
#' }
#' \dontshow{
#' unlink("easyPubMed_data_01.txt")
#' setwd(.old_wd)
#' }
#' @export
article_to_df2 <- function(pubmedArticle, autofill = FALSE, max_chars = 500,
                           getKeywords = FALSE, getAuthors = TRUE) {
  options(warn = -1)
  if (!inherits(pubmedArticle, "character") | regexpr(
    "(<PubmedArticle)(.+)(\\/PubmedArticle>)",
    pubmedArticle
  ) < 0) {
    message("An error occurred")
    return(NULL)
  }
  if (!is.numeric(max_chars)) {
    max_chars <- 500
  } else if (max_chars < 0) {
    max_chars <- -1
  }
  tryCatch({
    tmp.article <- easyPubMed::custom_grep(
      xml_data = pubmedArticle,
      tag = "PubmedArticle", format = "char"
    )
    if (is.null(tmp.article)) {
      message("An error occurred")
      return(NULL)
    }
    tmp.title <- easyPubMed::custom_grep(
      xml_data = tmp.article, tag = "ArticleTitle",
      format = "char"
    )
    if (length(tmp.title) > 1) {
      tmp.title <- paste(tmp.title, collapse = " ", sep = " ")
    } else if (length(tmp.title) < 1) {
      tmp.title <- NA
    }
    tmp.abstract <- easyPubMed::custom_grep(
      xml_data = tmp.article,
      tag = "AbstractText", format = "char"
    )
    if (length(tmp.abstract) > 1) {
      tmp.abstract <- paste(tmp.abstract,
        collapse = " ",
        sep = " "
      )
      if (max_chars >= 0) {
        tmp.abstract <- gsub("</{0,1}i>", "", tmp.abstract,
          ignore.case = T
        )
        tmp.abstract <- gsub("</{0,1}b>", "", tmp.abstract,
          ignore.case = T
        )
        tmp.abstract <- gsub("</{0,1}sub>", "", tmp.abstract,
          ignore.case = T
        )
        tmp.abstract <- gsub("</{0,1}exp>", "", tmp.abstract,
          ignore.case = T
        )
        tmp.abstract <- substr(tmp.abstract, 0, max_chars)
      }
    } else if (length(tmp.abstract) < 1) {
      tmp.abstract <- NA
    } else {
      if (max_chars >= 0) {
        tmp.abstract <- substr(tmp.abstract, 0, max_chars)
        tmp.abstract <- gsub("</{0,1}i>", "", tmp.abstract,
          ignore.case = T
        )
        tmp.abstract <- gsub("</{0,1}b>", "", tmp.abstract,
          ignore.case = T
        )
        tmp.abstract <- gsub("</{0,1}sub>", "", tmp.abstract,
          ignore.case = T
        )
        tmp.abstract <- gsub("</{0,1}exp>", "", tmp.abstract,
          ignore.case = T
        )
      }
    }
    my.dateType <- c(
      "DateCompleted", "DateCreated", "DateRevised",
      "PubDate"
    )
    sel.dateType <- which(sapply(my.dateType, (function(xi) {
      regexpr(xi, tmp.article) > 0
    })))
    if (length(sel.dateType) < 1) {
      tmp.date <- c(Year = NA, Month = NA, Day = NA)
    } else {
      sel.dateType <- sel.dateType[1]
      tmp.date <- easyPubMed::custom_grep(
        xml_data = tmp.article,
        tag = my.dateType[sel.dateType], format = "char"
      )
      tmp.date <- sapply(c("Year", "Month", "Day"), (function(tt) {
        tdat.el <- easyPubMed::custom_grep(
          xml_data = tmp.date,
          tag = tt, format = "char"
        )
        ifelse(is.null(tdat.el), NA, tdat.el[1])
      }))
    }
    tmp.paperID <- easyPubMed::custom_grep(
      xml_data = tmp.article, tag = "ArticleIdList",
      format = "char"
    )
    if (is.null(tmp.paperID)) {
      message("An error occurred")
      return(NULL)
    } else {
      tmp.paperID <- gsub("[[:space:]]", "", tmp.paperID[1])
    }
    tmp.PMID <- gsub(
      "^(.*ArticleIdIdType=\\\"pubmed\\\")([[:space:]]|[[:alnum:]]){0,20}>",
      "", tmp.paperID
    )
    tmp.PMID <- gsub("<.*$", "", tmp.PMID)
    tmp.DOI <- gsub(
      "^(.*ArticleIdIdType=\\\"doi\\\")([[:space:]]|[[:alnum:]]){0,20}>",
      "", tmp.paperID
    )
    tmp.DOI <- gsub("<.*$", "", tmp.DOI)
    tmp.jabbrv <- easyPubMed::custom_grep(
      xml_data = tmp.article, tag = "ISOAbbreviation",
      format = "char"
    )
    tmp.jabbrv <- ifelse(is.null(tmp.jabbrv), NA, tmp.jabbrv)
    tmp.journal <- easyPubMed::custom_grep(
      xml_data = tmp.article, tag = "Title",
      format = "char"
    )
    tmp.journal <- ifelse(is.null(tmp.journal), NA, tmp.journal)
    tmp.keys <- tryCatch(
      {
        if (getKeywords) {
          tmp.keys <- easyPubMed::custom_grep(
            xml_data = tmp.article,
            tag = "Keyword", format = "char"
          )
          tmp.mesh <- easyPubMed::custom_grep(
            xml_data = tmp.article,
            tag = "MeshHeading", format = "char"
          )
          if (length(tmp.mesh) > 0) {
            tmp.mesh <- sapply(tmp.mesh, function(xxm) {
              easyPubMed::custom_grep(
                xml_data = xxm, tag = "DescriptorName",
                format = "char"
              )
            })
          }
          tmp.keys <- c(tmp.keys, tmp.mesh)
          if (length(tmp.keys) > 1) {
            tmp.keys <- paste(tmp.keys, collapse = "; ")
          } else if (length(tmp.keys) < 1) {
            tmp.keys <- NA
          }
        } else {
          NA
        }
      },
      error = function(e) {
        NA
      }
    )
    tmp.resout <- c(
      pmid = tmp.PMID, doi = tmp.DOI, title = tmp.title,
      abstract = tmp.abstract, year = as.vector(tmp.date[1]),
      month = as.vector(tmp.date[2]), day = as.vector(tmp.date[3]),
      jabbrv = tmp.jabbrv, journal = tmp.journal, keywords = tmp.keys
    )
    tmp.authors <- easyPubMed::custom_grep(
      xml_data = tmp.article, tag = "AuthorList",
      format = "char"
    )
    if (length(tmp.authors) < 1 | !getAuthors) {
      final.mat <- data.frame(
        rbind(c(tmp.resout,
          lastname = NA,
          firstname = NA, address = NA, email = NA
        )),
        stringsAsFactors = FALSE
      )
    } else {
      author.list <- easyPubMed::custom_grep(
        xml_data = tmp.authors,
        tag = "Author", format = "char"
      )
      final.mat <- do.call(rbind, lapply(
        author.list,
        (function(al) {
          tmp.lastnm <- easyPubMed::custom_grep(
            xml_data = al, tag = "LastName",
            format = "char"
          )
          tmp.firstnm <- easyPubMed::custom_grep(
            xml_data = al,
            tag = "ForeName", format = "char"
          )
          tmp.email <- regexpr(
            "([[:alnum:]]|\\.|\\-\\_){3,200}@([[:alnum:]]|\\.|\\-\\_){3,200}(\\.)([[:alnum:]]){2,6}",
            al
          )
          if (tmp.email > 0) {
            tmp.email <- substr(al, tmp.email, tmp.email +
              attributes(tmp.email)$match.length - 1)
          } else {
            tmp.email <- NA
          }
          if (regexpr("Affiliation", al) > 0) {
            tmp.add <- easyPubMed::custom_grep(al, "Affiliation",
              format = "char"
            )[1]
            tmp.add <- tmp.add
          } else {
            tmp.add <- NA
          }
          c(tmp.resout,
            lastname = tmp.lastnm, firstname = tmp.firstnm,
            address = tmp.add, email = tmp.email
          )
        })
      ))
      rownames(final.mat) <- NULL
      final.mat <- data.frame(final.mat, stringsAsFactors = FALSE)
      DESELECT <- is.na(final.mat$lastname) | is.na(final.mat$firstname)
      if (length(DESELECT) > 0 & sum(DESELECT) > 0) {
        final.mat <- final.mat[!DESELECT, ]
      }
      if (autofill) {
        tmp.address <- final.mat[, "address"]
        na.pos <- is.na(tmp.address)
        if (sum(na.pos) != length(tmp.address)) {
          tmp.list <- lapply(tmp.address, function(x) {
            x
          })
          cur.add <- tmp.list[[(which(!na.pos)[1])]]
          for (i in 1:length(na.pos)) {
            if (na.pos[i]) {
              tmp.list[[i]] <- cur.add
            } else {
              cur.add <- tmp.list[[i]]
            }
          }
          final.mat[, "address"] <- do.call(c, tmp.list)
        }
      }
    }
    if (ncol(final.mat) != 14) {
      final.mat <- NULL
    }
  }, error = function(e) {
    NULL
  }, finally = {
    options(warn = 0)
    return(final.mat)
  })
}
