## code to prepare `addsToCart` dataset goes here

addsToCart <- read.csv(system.file("raw_data/DataAnalyst_Ecom_data_addsToCart.csv", package = 'ixisDemo'))
usethis::use_data(addsToCart, overwrite = TRUE)
