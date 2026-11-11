install.packages("tidyverse")
install.packages("jsonlite")
install.packages("data.table")
install.packages("tigris")
install.packages("sf")
install.packages("janitor")

library(tidyverse)
library(jsonlite)
library(data.table)
library(tigris)
library(sf)
library(janitor)

business_file <- "data/raw/yelp_academic_dataset_business.json"

business <- jsonlite::stream_in(
  file(business_file, open = "r"),
  verbose = FALSE
)

glimpse(business)
dim(business)

new_orleans_business <- business %>%
  filter(
    city == "New Orleans",
    state == "LA",
    str_detect(categories, regex("Restaurants", ignore_case = TRUE))
  )

dim(new_orleans_business)

head(new_orleans_business)

colnames(new_orleans_business)

saveRDS(
  new_orleans_business,
  "data/new_orleans/new_orleans_business.rds",
  compress = "gzip"
)

new_orleans_business <- readRDS(
  "data/new_orleans/new_orleans_business.rds"
)

dim(new_orleans_business)

restaurant_ids <- new_orleans_business$business_id






