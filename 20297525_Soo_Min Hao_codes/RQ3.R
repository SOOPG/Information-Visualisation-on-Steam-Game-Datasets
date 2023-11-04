pacman::p_load(pacman,dplyr,GGally,ggmosaic,ggplot2,ggthemes,ggvis,httr,lubridate,plotly,rio,rmarkdown,shiny,stringr,tidyr)

library(readxl)
Steam_Games_Dataset <- read_excel("C:/Users/wilem/OneDrive/Desktop/Desktop Apps/UNM/Year 3/Fundamentals of InfoViz/CW1/Information-Visualisation-on-Steam-Game-Datasets/Steam Games Dataset.xlsx")

# Convert All Reviews Number column into only numerics
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(
    # Extract the numeric part after the percentage 
    UserReviewsStr = str_extract(`All Reviews Number`, "(?<=of the )\\d+[\\d,]*"),
    UserReviewsStr = str_replace_all(UserReviewsStr, ",", ""), # Remove comma if it's a value of thousand with a separator
    UserReviews = as.numeric(UserReviewsStr) # Convert the string to a numeric value
  )

# Extract the Developer column from the dataset and add it as a new column
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(GameDevelopers = Developer)

# Remove all rows containing any NAs
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  drop_na()

# Sum up all User Reviews 
# Avoid splitting on commas followed by known company suffixes eg: Ltd and LLC
Steam_Games_Dataset_Expanded <- Steam_Games_Dataset %>%
  mutate(GameDevelopers = gsub(", Inc\\.", " Inc.", gsub(", Ltd\\.", " Ltd.", Developer))) %>%
  separate_rows(GameDevelopers, sep = ",\\s*(?![^,]+\\.)") %>%
  group_by(GameDevelopers) %>%
  summarise(SumUserReviews = sum(UserReviews, na.rm = TRUE)) %>%
  ungroup()

# Select the top 10 developers by sum of user reviews
Top10_Developers <- Steam_Games_Dataset_Expanded %>%
  slice_max(order_by = SumUserReviews, n = 10)

# Check 
View(Steam_Games_Dataset[c('All Reviews Number', 'UserReviews', 'GameDevelopers')])
View(Top10_Developers)
