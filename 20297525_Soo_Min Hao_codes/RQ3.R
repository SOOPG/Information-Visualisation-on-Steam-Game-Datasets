pacman::p_load(pacman, dplyr, GGally, ggmosaic, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, scales, shiny, stringr, tidyr, readxl)

Steam_Games_Dataset <- read_excel("C:/Users/wilem/OneDrive/Desktop/Desktop Apps/UNM/Year 3/Fundamentals of InfoViz/CW1/Information-Visualisation-on-Steam-Game-Datasets/Steam Games Dataset.xlsx")

# Load dataset
Steam_Games_Dataset <- read_excel("C:/Users/wilem/OneDrive/Desktop/Desktop Apps/UNM/Year 3/Fundamentals of InfoViz/CW1/Information-Visualisation-on-Steam-Game-Datasets/Steam Games Dataset.xlsx")

# Convert All Reviews Number column into only numerics
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(
    UserReviewsStr = str_extract(`All Reviews Number`, "(?<=of the )\\d+[\\d,]*"),
    UserReviewsStr = str_replace_all(UserReviewsStr, ",", ""),
    UserReviews = as.numeric(UserReviewsStr)
  )

# Extract the Developer column from the dataset and add it as a new column
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(GameDevelopers = Developer)

# Clean up developer names
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(GameDevelopers = str_replace(GameDevelopers, ", Inc\\.|\\(Mac\\)| Co\\. Ltd\\.", ""))

# Remove all rows containing any NAs
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  drop_na()

# Sum up all User Reviews without altering the state for specific company suffixes
Steam_Games_Dataset_Expanded <- Steam_Games_Dataset %>%
  group_by(GameDevelopers) %>%
  summarise(SumUserReviews = sum(UserReviews, na.rm = TRUE)) %>%
  ungroup()

# Select the top 10 developers by sum of user reviews
Top10_Developers <- Steam_Games_Dataset_Expanded %>%
  slice_max(order_by = SumUserReviews, n = 10)

# Check 
#View(Steam_Games_Dataset[c('All Reviews Number', 'UserReviews', 'GameDevelopers')])
View(Top10_Developers)

 m       ## Find the popular tags and its tag counts based on the Top10_Developers
## Afterwards Find the most Popular tag for each of the top 10 developers 
## If there is the same tag counts, add both tags into the element
## Get the first tag with the assuming of first tag is the most popular tag from the element

# Step 1: Get games only developed by top 10 developers
games_from_top_developers <- Steam_Games_Dataset %>%
  filter(GameDevelopers %in% Top10_Developers$GameDevelopers)

# Step 2: Split 'Popular Tags' into individual rows and count them per developer
tags_count <- games_from_top_developers %>%
  separate_rows(`Popular Tags`, sep = ";") %>%
  group_by(GameDevelopers, `Popular Tags`) %>%
  summarise(TagCount = n(), .groups = 'drop') %>%
  ungroup()

# Step 3: Get the most popular tags. In case of tie, concatenate tags.
top_tags_per_developer <- tags_count %>%
  arrange(GameDevelopers, desc(TagCount)) %>%
  group_by(GameDevelopers) %>%
  filter(TagCount == max(TagCount)) %>%
  summarise(CombinedTags = toString(`Popular Tags`), .groups = 'drop')

# Step 4: Join the top tags with the top developers dataframe
Top10_Developers_With_Tags <- Top10_Developers %>%
  left_join(top_tags_per_developer, by = "GameDevelopers")

# Step 5: Extract the first tag for use in plots
Top10_Developers_With_Tags <- Top10_Developers_With_Tags %>%
  mutate(FirstTag = word(CombinedTags, 1))

# Step 6: Remove any special symbols from extracted tag
Top10_Developers_With_Tags <- Top10_Developers_With_Tags %>%
  mutate(FirstTag = str_replace_all(FirstTag, "[^\\w\\s]", ""))

# add scale_y_continuous to format the y-axis
stacked_bar_plot <- ggplot(Top10_Developers_With_Tags, aes(x = reorder(GameDevelopers, -SumUserReviews), y = SumUserReviews, fill = FirstTag)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Stacked Bar Chart of Top 10 Game Developers by User Reviews and Popular Tag",
    x = "Game Developers",
    y = "Sum of User Reviews",
    fill = "First Tag"
  ) +
  scale_y_continuous(labels = label_comma()) # Format the y-axis labels

# Display the plot
print(stacked_bar_plot)
