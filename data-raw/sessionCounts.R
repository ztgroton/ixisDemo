## code to prepare `sessionCounts` dataset goes here

sessionCounts <- read.csv(system.file("raw_data/DataAnalyst_Ecom_data_sessionCounts.csv", package = 'ixisDemo'))
usethis::use_data(sessionCounts, overwrite = TRUE)
