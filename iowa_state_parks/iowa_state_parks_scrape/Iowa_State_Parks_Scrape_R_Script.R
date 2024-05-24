# Description: Code for scraping/cleaning Iowa State Parks data from the Iowa
#              DNR website
# Author: Aidan Burk
# Date Created: 12/14/2023
# Date Edited: 1/20/2024
# Data: 'https://www.iowadnr.gov/Places-to-Go/State-Parks/Iowa-State-Parks'
# Notes:
# Most parks have a iowadnr.gov page & a iowastateparks.reserveamerica.com 
# page, except for 11 which only have iowadnr.gov links.
# For the parks that don't have reservation pages, data is harder to 
# scrape from DNR pages, & geocoding from park name & city alone does not
# provide accurate data.
# This code writes the data with NA values for the 11 parks.


# Packages

library(rvest)
library(tidyverse)

# Load Data ---------------------------------------------------------------

# Read state parks page html
ia_state_parks_html <- rvest::read_html(
  'https://www.iowadnr.gov/places-to-go/state-parks/iowa-state-parks')

# Scrape table from state parks page html
ia_state_parks_df <- ia_state_parks_html |> 
  rvest::html_element('.table') |>
  rvest::html_table()

# Data Cleaning -----------------------------------------------------------

# Select all links from table
ia_state_parks_links <- ia_state_parks_html |>
  rvest::html_nodes(xpath = "//td/a") |> 
  rvest::html_attr("href")
# Checking links
ia_state_parks_links
# 71 of the links are for state parks' DNR pages, the other 60 are for
# reservation pages

# Create a list of parsed DNR pages
ia_state_parks_pages <- ia_state_parks_links |>
  subset(stringr::str_starts(ia_state_parks_links, 
                             "https://www.iowadnr.gov") == TRUE) |>
  purrr::map(read_html)
# Create a list of parsed reservation pages
ia_state_parks_reservation_pages <- ia_state_parks_links |>
  subset(stringr::str_starts(ia_state_parks_links, 
                             "https://iowastateparks.reserveamerica.com/") 
         == TRUE | 
           stringr::str_starts(ia_state_parks_links, 
                               "http://iowastateparks.reserveamerica.com/") 
         == TRUE) |>
  purrr::map(read_html)


# Create subset df with parks that have reservation pages
reservations_df <- ia_state_parks_df |>
  subset(
    ia_state_parks_df$`Reservation Information` != 'No reservable facilities')
# Create column for links to reservation pages
reservations_df$reservation_link <- 
  ia_state_parks_links |> 
  subset(stringr::str_starts(ia_state_parks_links, 
                             "https://iowastateparks.reserveamerica.com/") 
         == TRUE | 
           stringr::str_starts(ia_state_parks_links, 
                               "http://iowastateparks.reserveamerica.com/") 
         == TRUE)
# Reduce reservations_df to only Reservation Information & reservation_link
# columns
reservations_df <- reservations_df[c(4,5)]


## Select variables from reservation pages & map them to columns:
# latitude:
reservations_df$latitude <- ia_state_parks_reservation_pages |>
  purrr::map(html_element, 
             css = "#contentcol > div:nth-child(4) > span:nth-child(4)") |> 
  purrr::map_chr(html_text)
# longitude: 
reservations_df$longitude <- ia_state_parks_reservation_pages |>
  purrr::map(html_element, 
             css = "#contentcol > div:nth-child(4) > span:nth-child(5)") |> 
  purrr::map_chr(html_text)
# street:
reservations_df$street <- ia_state_parks_reservation_pages |>
  purrr::map(html_element, 
             css = "#contentcol > div:nth-child(5) > span:nth-child(5)") |> 
  purrr::map_chr(html_text)
# city:
reservations_df$city <- ia_state_parks_reservation_pages |>
  purrr::map(html_element, 
             css = "#contentcol > div:nth-child(5) > span:nth-child(7)") |> 
  purrr::map_chr(html_text)
# state:
reservations_df$state <- ia_state_parks_reservation_pages |>
  purrr::map(html_element, 
             css = "#contentcol > div:nth-child(5) > span:nth-child(8)") |> 
  purrr::map_chr(html_text)
# zip:
reservations_df$zip <- ia_state_parks_reservation_pages |>
  purrr::map(html_element, 
             css = "#contentcol > div:nth-child(5) > span:nth-child(9)") |> 
  purrr::map_chr(html_text)
# phone:
reservations_df$phone <- ia_state_parks_reservation_pages |>
  purrr::map(html_element, 
             css = "#contentcol > div:nth-child(5) > div > span") |> 
  purrr::map_chr(html_text)

# Several pages had a second extra city & zip that got mixed up in
# their column mapping
# Loop through reservations_df & extract zip & city from street if 
# street contains 'IA' 
for (row in 1:nrow(reservations_df)) {
  if(stringr::str_detect(reservations_df$street[row], 'IA') == TRUE) {
    reservations_df$zip[row] <-
      stringr::str_extract(
        reservations_df$street[row],
        '\\S*$')
    reservations_df$city[row] <- 
      stringr::str_extract(
        reservations_df$street[row],
        '(?<=, )(.*?)[^,]+')
    reservations_df$street[row] <-
      stringr::str_extract(
        reservations_df$street[row],
        '(.*?)[^,]+')
  }
}

# Concatenate street, city, state, & zip into address column
reservations_df$address <- 
  paste(
    reservations_df$street,
    reservations_df$city,
    reservations_df$state,
    reservations_df$zip
  )
# Remove address number from street column
reservations_df <- reservations_df |>
  dplyr::mutate(
    street = stringr::str_remove(street,
                                 '(?s)(.*?)[\\s]')
  )




# Trim extraneous whitespace from addresses
reservations_df$address <- stringr::str_squish(reservations_df$address)
# Format addresses
reservations_df$address <- reservations_df$address |>
  stringr::str_replace("\\sIA", ", IA")
reservations_df$address <- reservations_df$address |>
  stringr::str_remove('^\\D+')
# Format phone numbers
reservations_df$phone <- reservations_df$phone |>
  stringr::str_replace_all(" ", "")
reservations_df$phone <- reservations_df$phone |>
  stringr::str_replace("[)]", ") ")

# Join reservations_df & ia_state_parks_df
ia_state_parks_df <- full_join(
  ia_state_parks_df, reservations_df, by = 'Reservation Information')

# Create column for links to DNR pages
ia_state_parks_df$dnr_link <- 
  ia_state_parks_links |>
  subset(stringr::str_starts(ia_state_parks_links, 
                             "https://www.iowadnr.gov") == TRUE)


# Write Data --------------------------------------------------------------

write_rds(ia_state_parks_df, "iowa_state_parks_scrape.rds")


