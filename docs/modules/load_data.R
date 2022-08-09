

load_airbnb_data <- function() {
  # Import data
  listings_raw <<- readr::read_csv(here::here("data", "listings.csv.gz"))
  reviews_raw <<- readr::read_csv(here::here("data", "reviews.csv.gz"))

  # Load data
  listings <- listings_raw %>%
    select(
      id,
      name,
      description,
      host_id,
      host_name,
      host_since,
      host_location,
      host_about,
      host_response_time,
      host_response_rate,
      host_acceptance_rate,
      host_is_superhost,
      host_neighbourhood,
      host_verifications,
      host_identity_verified,
      host_has_profile_pic,
      neighbourhood_cleansed,
      latitude,
      longitude,
      property_type,
      room_type,
      accommodates,
      bathrooms_text,
      bedrooms,
      beds,
      price
    ) %>%
    mutate(
      host_since = format(as.Date(host_since), "%m/%d/%Y"),
      .after = host_name
    ) %>%
    mutate(bathrooms = parse_number(str_extract(bathrooms_text, "\\d*\\.?\\d+")), .after = beds) %>%
    mutate(price = parse_number(price), ) %>%
    select(!bathrooms_text) %>%
    rename(neighbourhood = neighbourhood_cleansed)

  reviews <- reviews_raw %>%
    select(listing_id, reviewer_id, reviewer_name, comments)

  # Clean data
  # Replace empty values with NA
  listings[listings == ""] <- NA
  reviews[reviews == ""] <- NA

  # Replace N/A values with NA
  listings[listings == "N/A"] <- NA
  reviews[reviews == "N/A"] <- NA

  # Drop rows with NA in id, name
  listings <- listings %>%
    drop_na(c("id", "name", "host_id", "host_name", "host_location", "host_neighbourhood"))

  reviews <<- reviews %>%
    drop_na(c("listing_id", "reviewer_id", "reviewer_name"))

  listings <<- listings %>%
    filter(
      host_location == names(which.max(table(host_location)))
    )
}