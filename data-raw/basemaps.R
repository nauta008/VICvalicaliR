## code to prepare `DATASET` dataset goes here

countries <- rnaturalearth::ne_download(scale = 10,type="countries", returnclass = "sf")
rivers <- rnaturalearth::ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf")

usethis::use_data(countries,rivers, overwrite = TRUE, internal = TRUE)
