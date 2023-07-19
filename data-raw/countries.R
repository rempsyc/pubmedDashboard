## code to prepare `countries` dataset goes here

countries <- c(unique(countrycode::countryname_dict$country.name.en), "USA", "Korea", "UK", "Scotland")

usethis::use_data(countries, overwrite = TRUE)

