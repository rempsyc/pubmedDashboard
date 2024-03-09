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
  "Psychological science",
  "Child development",
  "Developmental science",
  "Personality and social psychology bulletin"

)

economics <- c(
  "Journal Of economic psychology",
  "Journal of experimental and behavioral economics",
  "Experimental economics",
  "Journal of development economics",
  "World development",
  "Quarterly journal of economics",
  "Econometrica : journal of the Econometric Society",
  "Behavioral public policy",
  "African development review",
  "African journal of agricultural and resource economics",
  "African journal of economic and management studies",
  "African journal of economic policy",
  "American economic journal. Applied economics",
  "American economic journal. Economic policy",
  "American economic journal. Macroeconomics",
  "American economic review",
  "American economic review: Insights",
  "Economic development and cultural change",
  "Economic journal (London, England)",
  "Journal of African economies",
  "Journal of African development",
  "Journal of human development and capabilities",
  "Journal of development effectiveness",
  "Journal of development studies",
  "Journal of economic growth",
  "Journal of labor economics",
  "Journal of political economy",
  "Journal of public economics",
  "Review of African political economy",
  "Review of development economics",
  "Review of economic studies",
  "Review of international political economy",
  "South African journal of economics",
  "World bank economic review",
  "World bank Research observer",
  "World development perspectives"
)

general <- c(
  "Nature human behaviour",
  "Plos one",
  "Science (New York, N.Y.)",
  "Nature",
  "Proceedings of the national academy of sciences"
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
      journal_field$journal %in% general, "general", NA
    )
  )
)
journal_field$original_journal <- journal_field$journal %in% psychology[1:6]

usethis::use_data(journal_field, overwrite = TRUE)
