## code to prepare `journal_field` dataset goes here

psychology <- c(
  "Developmental psychology",
  "Journal of personality and social psychology",
  "Journal of abnormal psychology",
  "Journal of family psychology : JFP : journal of the Division of Family Psychology of the American Psychological Association (Division 43)",
  "Health psychology : official journal of the Division of Health Psychology, American Psychological Association",
  "Journal of educational psychology",
  # Above journals are the original
  "Journal of experimental social psychology",
  "Collabra. Psychology",
  "Journal of experimental psychology. General",
  "The Journal of applied psychology",
  "Psychological methods",
  "Advances in methods and practices in psychological science",
  "Psychological science"
)

economics <- c(
  "Journal Of Economic Psychology",
  "Journal of experimental and behavioral economics",
  "Experimental economics",
  "Journal of development economics",
  "World development",
  "Quarterly journal of economics",
  "Econometrica : journal of the Econometric Society",
  "Behavioral public policy"
)

general <- c(
  "Nature human behaviour",
  "Plos one",
  "Science (New York, N.Y.)",
  "Nature"
)

journal_field <- data.frame(
  journal = c(psychology, economics, general)
)

journal_field$journal_short <- clean_journal_names(journal_field$journal)
# journal_field$journal_short <- gsub(":.*", "", journal_field$journal)
# journal_field$journal_short <- gsub("[(].*", "", journal_field$journal_short)
# journal_field$journal_short <- tools::toTitleCase(journal_field$journal_short)
# journal_field$journal_short <- trimws(journal_field$journal_short)

journal_field$field <- ifelse(
  journal_field$journal %in% psychology, "psychology", ifelse(
    journal_field$journal %in% economics, "economics", ifelse(
      journal_field$journal %in% general, "general", NA)))
journal_field$original_journal <- journal_field$journal %in% psychology[1:6]

usethis::use_data(journal_field, overwrite = TRUE)

