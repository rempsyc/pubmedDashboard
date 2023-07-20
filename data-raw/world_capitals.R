## code to prepare `world_capitals` dataset goes here

world_capitals <- maps::world.cities %>%
  dplyr::filter(capital != 0)

usethis::use_data(world_capitals, overwrite = TRUE)
