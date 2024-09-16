## code to prepare `DATASET` dataset goes here

montpellier=read.csv("ArAr/data-raw/montpellier.csv",header=T,sep=";",dec=".",row.name=1)
usethis::use_data(montpellier, overwrite = TRUE)

albinia=read.csv("ArAr/data-raw/albinia.csv",header=T,sep=";",dec=".",row.name=1)
usethis::use_data(albinia, overwrite = TRUE)
