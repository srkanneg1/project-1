# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
library(
  tidytuesdayR
  )
library(
  tidyverse
  )

tuesdata <- tidytuesdayR::tt_load(
  '2021-01-26'
  )
tuesdata <- tidytuesdayR::tt_load(
  2021, 
  week = 5
  )

plastics <- tuesdata$plastics

# Or read in the data manually

plastics <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-01-26/plastics.csv'
  )

initial <- plastics |> 
  group_by(
    country
    ) |> 
  summarise(
    Efficiency = sum(
      grand_total
      ) / sum(
        num_events
        )
    )
third <- quantile(
  initial$Efficiency, 
  probs = 0.75, 
  na.rm = TRUE
  )
first <- quantile(
  initial$Efficiency, 
  probs = 0.25,
  na.rm = TRUE
  )
mult <- 1.5 * IQR(
  initial$Efficiency, 
  na.rm = TRUE
  )

initial |>
  filter(
    (Efficiency < third + mult) & (Efficiency > first - mult)
    ) |>
  arrange(
    -Efficiency
  )