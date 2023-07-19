## code to prepare `us_states` dataset goes here

us_states <- countrycode::get_dictionary("us_states")

usethis::use_data(us_states, overwrite = TRUE)

