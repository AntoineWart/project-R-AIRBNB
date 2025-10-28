## ----setup, include=FALSE,echo=FALSE------------------------------------------

library(tidyverse)   
library(lubridate)   
library(janitor)     
library(knitr)       
library(scales)  


## ----echo=FALSE---------------------------------------------------------------
# Import and clean column names

airbnb_raw = read_csv("../data/train.csv", show_col_types = FALSE) |>
clean_names()

# Dimensions of the dataset

n_rows = nrow(airbnb_raw)
n_cols = ncol(airbnb_raw)

kable(
tibble(
"Number of observations" = n_rows,
"Number of variables" = n_cols
),
caption = "Dataset overview: size and scope"
)



## ----echo=FALSE---------------------------------------------------------------
# Display column names

kable(
tibble("Column names" = colnames(airbnb_raw)),
caption = "Variables available in the dataset"
)


## ----echo=FALSE---------------------------------------------------------------
# Apply all cleaning steps (technical, hidden)

airbnb_clean = airbnb_raw |>
mutate(
last_review = suppressWarnings(ymd(last_review)),
reviews_per_month = replace_na(reviews_per_month, 0)
) |>
filter(price > 0) |>
select(-any_of(c("id", "host_id", "host_name")))

# Display final dimensions after cleaning

kable(
tibble(
"Observations after cleaning" = nrow(airbnb_clean),
"Variables after cleaning" = ncol(airbnb_clean)
),
caption = "Dataset after cleaning and preparation"
)


## ----echo=FALSE---------------------------------------------------------------
# Distribution by borough
borough_summary = airbnb_clean |>
  group_by(neighbourhood_group) |>
  summarise(
    listings = n(),
    avg_price = mean(price, na.rm = TRUE)
  ) |>
  arrange(desc(listings))

kable(borough_summary, caption = "Distribution of listings and average price by borough")

