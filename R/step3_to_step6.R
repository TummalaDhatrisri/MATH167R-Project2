dir.create("output", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("data/boundaries",
           recursive = TRUE,
           showWarnings = FALSE)

# Separate categories into individual rows

category_data <- new_orleans_business %>%
  select(business_id, name, categories) %>%
  filter(!is.na(categories)) %>%
  separate_rows(categories, sep = ",") %>%
  mutate(
    categories = str_trim(categories)
  )

# Count restaurants in each category

category_counts <- category_data %>%
  count(categories, sort = TRUE)

# Display the 30 most common categories

head(category_counts, 30)

# Select our three categories

selected_categories <- c(
  "Seafood",
  "Cajun/Creole",
  "Southern"
)

# Count each selected category

selected_category_counts <- category_counts %>%
  filter(categories %in% selected_categories)

print(selected_category_counts)

# Find businesses that have multiple selected categories

category_overlap <- category_data %>%
  filter(categories %in% selected_categories) %>%
  group_by(business_id, name) %>%
  summarise(
    number_of_categories = n_distinct(categories),
    categories_found = paste(
      sort(unique(categories)),
      collapse = ", "
    ),
    .groups = "drop"
  )

# Count the different combinations

overlap_summary <- category_overlap %>%
  count(number_of_categories, sort = TRUE)

print(overlap_summary)

# Select restaurants matching at least one category

selected_restaurants <- new_orleans_business %>%
  filter(
    str_detect(
      categories,
      "Seafood|Cajun/Creole|Southern"
    )
  )

# Check the number of selected restaurants

nrow(selected_restaurants)

# Check for duplicate businesses

n_distinct(selected_restaurants$business_id)

saveRDS(
  selected_restaurants,
  "data/new_orleans/selected_restaurants.rds"
)

write_csv(
  selected_category_counts,
  "output/selected_category_counts.csv"
)

write_csv(
  overlap_summary,
  "output/category_overlap.csv"
)

# Get the IDs of our selected restaurants

selected_ids <- selected_restaurants$business_id

# Create an empty list for matching review chunks

review_chunks <- list()

# Open the review JSON file

review_connection <- file(
  "data/raw/yelp_academic_dataset_review.json",
  open = "r"
)

# Read the review data in chunks

jsonlite::stream_in(
  review_connection,
  pagesize = 10000,
  handler = function(x) {
    
    matching_reviews <- x %>%
      filter(business_id %in% selected_ids) %>%
      select(
        review_id,
        business_id,
        user_id,
        stars,
        date,
        useful,
        funny,
        cool
      )
    
    if (nrow(matching_reviews) > 0) {
      review_chunks[[length(review_chunks) + 1]] <<-
        matching_reviews
    }
  }
)

close(review_connection)

# Combine matching reviews

selected_reviews <- bind_rows(review_chunks)

# Check results

dim(selected_reviews)

head(selected_reviews)

saveRDS(
  selected_reviews,
  "data/new_orleans/selected_reviews.rds",
  compress = "gzip"
)

restaurant_reviews <- selected_reviews %>%
  left_join(
    selected_restaurants %>%
      select(
        business_id,
        name,
        stars,
        review_count,
        latitude,
        longitude
      ),
    by = "business_id"
  )

# Check the joined dataset

glimpse(restaurant_reviews)

review_summary <- restaurant_reviews %>%
  group_by(business_id, name) %>%
  summarise(
    total_reviews = n(),
    average_review_rating = mean(stars.x, na.rm = TRUE),
    .groups = "drop"
  )

print(head(review_summary))

saveRDS(
  restaurant_reviews,
  "data/new_orleans/restaurant_reviews.rds"
)

write_csv(
  review_summary,
  "output/review_summary.csv"
)

# Download Louisiana place boundaries

places <- tigris::places(
  state = "LA",
  year = 2020,
  cb = FALSE
)

# Check the available places

places %>%
  st_drop_geometry() %>%
  select(NAME, GEOID) %>%
  head()

st_crs(restaurant_points)
st_crs(new_orleans_place)

new_orleans_place <- places %>%
  filter(str_detect(NAME, regex("New Orleans", ignore_case = TRUE)))

print(new_orleans_place)

# Convert restaurants into geographic points

restaurant_points <- selected_restaurants %>%
  filter(
    !is.na(latitude),
    !is.na(longitude)
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )

# Make boundary CRS match restaurant CRS

new_orleans_place <- st_transform(
  new_orleans_place,
  st_crs(restaurant_points)
)

# Perform spatial join

restaurants_with_places <- st_join(
  restaurant_points,
  new_orleans_place,
  join = st_within,
  left = TRUE
)

# Compare Yelp city with official place name

place_comparison <- restaurants_with_places %>%
  st_drop_geometry() %>%
  count(city, NAME, sort = TRUE)

print(place_comparison)

# Count unmatched restaurants

sum(is.na(restaurants_with_places$NAME))

# Save results

saveRDS(
  restaurants_with_places,
  "data/new_orleans/restaurants_with_places.rds"
)


# Download New Orleans NSA boundaries

nsa_url <- paste0(
  "https://maps.nola.gov/server/rest/services/",
  "Reference/Reference_Layers/MapServer/0/query?",
  "where=1%3D1&outFields=*&returnGeometry=true&f=geojson"
)

nsa_boundaries <- st_read(
  nsa_url,
  quiet = TRUE
)

# Inspect columns

names(nsa_boundaries)

head(st_drop_geometry(nsa_boundaries))

# Create a consistent geographic name column

nsa_boundaries <- nsa_boundaries %>%
  mutate(
    area_name = as.character(GNOCDC_LAB)
  )

# Check

head(nsa_boundaries$area_name)

# Download Orleans Parish 2020 Census tracts

tract_boundaries <- tigris::tracts(
  state = "LA",
  county = "Orleans",
  year = 2020,
  cb = FALSE
)

# Create a consistent name for each tract

tract_boundaries <- tract_boundaries %>%
  mutate(
    area_name = GEOID
  )

# Inspect

head(tract_boundaries$area_name)

dir.create(
  "data/boundaries",
  recursive = TRUE,
  showWarnings = FALSE
)

st_write(
  nsa_boundaries,
  "data/boundaries/new_orleans_nsa.gpkg",
  delete_dsn = TRUE
)

st_write(
  tract_boundaries,
  "data/boundaries/new_orleans_tracts.gpkg",
  delete_dsn = TRUE
)

# Make sure coordinate systems match

restaurant_points <- st_transform(
  restaurant_points,
  st_crs(nsa_boundaries)
)

# Spatial join

restaurants_nsa <- st_join(
  restaurant_points,
  nsa_boundaries,
  join = st_within,
  left = TRUE
)

# Count restaurants per NSA

nsa_summary <- restaurants_nsa %>%
  st_drop_geometry() %>%
  group_by(area_name) %>%
  summarise(
    restaurant_count = n(),
    average_rating = mean(stars, na.rm = TRUE),
    total_yelp_reviews = sum(review_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(restaurant_count))

print(nsa_summary)

# Transform points to tract coordinate system

restaurant_points_tract <- st_transform(
  restaurant_points,
  st_crs(tract_boundaries)
)

# Spatial join

restaurants_tract <- st_join(
  restaurant_points_tract,
  tract_boundaries,
  join = st_within,
  left = TRUE
)

# Summarize by tract

tract_summary <- restaurants_tract %>%
  st_drop_geometry() %>%
  group_by(area_name) %>%
  summarise(
    restaurant_count = n(),
    average_rating = mean(stars, na.rm = TRUE),
    total_yelp_reviews = sum(review_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(restaurant_count))

print(tract_summary)

# Function to summarize geographic coverage

geography_stats <- function(data, area_column) {
  
  area_counts <- data %>%
    st_drop_geometry() %>%
    count(.data[[area_column]], name = "restaurant_count")
  
  tibble(
    geographic_system = area_column,
    number_of_areas = n_distinct(area_counts[[area_column]]),
    restaurants_assigned = sum(area_counts$restaurant_count),
    unmatched_restaurants = sum(is.na(data[[area_column]])),
    smallest_area_count = min(area_counts$restaurant_count),
    largest_area_count = max(area_counts$restaurant_count)
  )
}

# Compare both systems

geography_comparison <- bind_rows(
  geography_stats(restaurants_nsa, "area_name") %>%
    mutate(geographic_system = "Neighborhood Statistical Areas"),
  
  geography_stats(restaurants_tract, "area_name") %>%
    mutate(geographic_system = "2020 Census Tracts")
)

print(geography_comparison)

write_csv(
  geography_comparison,
  "output/geography_comparison.csv"
)

nsa_map <- ggplot() +
  geom_sf(
    data = nsa_boundaries,
    fill = "white",
    color = "gray50",
    linewidth = 0.3
  ) +
  geom_sf(
    data = restaurants_nsa,
    color = "darkgreen",
    size = 1.5,
    alpha = 0.7
  ) +
  labs(
    title = "Selected Restaurant Locations in New Orleans",
    subtitle = "Grouped by Neighborhood Statistical Areas",
    caption = "Source: Yelp Open Dataset and City of New Orleans"
  ) +
  theme_minimal()

print(nsa_map)

ggsave(
  "figures/nsa_restaurant_map.png",
  nsa_map,
  width = 10,
  height = 8,
  dpi = 300
)

tract_map <- ggplot() +
  geom_sf(
    data = tract_boundaries,
    fill = "white",
    color = "gray50",
    linewidth = 0.3
  ) +
  geom_sf(
    data = restaurants_tract,
    color = "darkblue",
    size = 1.5,
    alpha = 0.7
  ) +
  labs(
    title = "Selected Restaurant Locations in New Orleans",
    subtitle = "Grouped by 2020 Census Tracts",
    caption = "Source: Yelp Open Dataset and U.S. Census Bureau"
  ) +
  theme_minimal()

print(tract_map)

ggsave(
  "figures/tract_restaurant_map.png",
  tract_map,
  width = 10,
  height = 8,
  dpi = 300
)

nsa_rating_map <- nsa_boundaries %>%
  left_join(
    nsa_summary,
    by = "area_name"
  )

rating_plot <- ggplot(nsa_rating_map) +
  geom_sf(
    aes(fill = average_rating),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "gray90"
  ) +
  labs(
    title = "Average Restaurant Ratings by Neighborhood",
    fill = "Average rating",
    caption = "Source: Yelp Open Dataset"
  ) +
  theme_minimal()

print(rating_plot)

ggsave(
  "figures/nsa_average_ratings.png",
  rating_plot,
  width = 10,
  height = 8,
  dpi = 300
)

write_csv(
  nsa_summary,
  "output/nsa_restaurant_summary.csv"
)

write_csv(
  tract_summary,
  "output/tract_restaurant_summary.csv"
)

# Number of selected restaurants
nrow(selected_restaurants)

# Number of occupied NSAs
nsa_summary %>%
  filter(restaurant_count > 0) %>%
  nrow()

# Number of occupied Census tracts
tract_summary %>%
  filter(restaurant_count > 0) %>%
  nrow()

# NSA unmatched restaurants
sum(is.na(restaurants_nsa$area_name))

# Census tract unmatched restaurants
sum(is.na(restaurants_tract$area_name))

geography_stats <- function(data, area_column, boundaries) {
  
  area_counts <- data %>%
    st_drop_geometry() %>%
    filter(!is.na(.data[[area_column]])) %>%
    count(.data[[area_column]])
  
  tibble(
    number_of_areas = nrow(boundaries),
    occupied_areas = nrow(area_counts),
    restaurants_assigned = sum(area_counts$n),
    unmatched_restaurants = sum(is.na(data[[area_column]])),
    smallest_area_count = min(area_counts$n),
    largest_area_count = max(area_counts$n)
  )
}

geography_comparison <- bind_rows(
  
  geography_stats(
    restaurants_nsa,
    "area_name",
    nsa_boundaries
  ) %>%
    mutate(geographic_system = "Neighborhood Statistical Areas"),
  
  geography_stats(
    restaurants_tract,
    "area_name",
    tract_boundaries
  ) %>%
    mutate(geographic_system = "2020 Census Tracts")
) %>%
  select(geographic_system, everything())

print(geography_comparison)

write_csv(
  geography_comparison,
  "output/geography_comparison.csv"
)
