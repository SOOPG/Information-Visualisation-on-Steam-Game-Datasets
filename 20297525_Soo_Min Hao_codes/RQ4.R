pacman::p_load(pacman, 
               dplyr, 
               GGally, 
               ggmosaic, 
               ggplot2, 
               ggthemes, 
               ggvis, 
               httr, 
               lubridate, 
               plotly, 
               rio, 
               rmarkdown, 
               scales, 
               shiny, 
               stringr, 
               tidyr, 
               readxl)

Steam_Games_Dataset <- read_excel("C:/Users/wilem/OneDrive/Desktop/Desktop Apps/UNM/Year 3/Fundamentals of InfoViz/CW1/Information-Visualisation-on-Steam-Game-Datasets/Steam Games Dataset.xlsx")

# Create flags for the features
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(
    # Check for any online multiplayer tags
    Online_Players = str_detect(`Game Features`, "Online Co-op") | 
      str_detect(`Game Features`, "Online PvP") |
      str_detect(`Game Features`, "Online-Coop"),
    # Check for single-player tag
    Singleplayer = str_detect(`Game Features`, "Single-player"),
    # Create a Category column based on the combination of features
    Category = case_when(
      Singleplayer & Online_Players ~ "Both",
      Singleplayer ~ "Single-Player",
      Online_Players ~ "Multi-Players",
      TRUE ~ "Other"  
    )
  )

# Filter out games that do not have either Single-player or Online Players features
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  filter(Online_Players == 1 | Singleplayer == 1)

# If you want to further process the data or summarize it
# For example, count the number of games with each feature
feature_counts <- Steam_Games_Dataset %>%
  summarise(
    Count_Online_Players = sum(Online_Players),
    Count_Singleplayer = sum(Singleplayer)
  )

# Convert All Reviews Number column into only numerics
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(
    UserReviews = str_extract(`All Reviews Number`, "(?<=of the )\\d+[\\d,]*"),
    UserReviews = str_replace_all(UserReviews, ",", ""),
    UserReviews = as.numeric(UserReviews)
  )
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(
    # Extract the percentage of positive reviews
    PositiveReviewPercentage = str_extract(`All Reviews Number`, "\\d+(?=%)"),
    # Convert the extracted percentage to numeric
    PositiveReviewPercentage = as.numeric(PositiveReviewPercentage),
  ) 

# Remove all rows containing any NAs
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  drop_na()

# Use View() to see the table in RStudio
View(Steam_Games_Dataset)

# Assuming the Steam_Games_Dataset has been properly preprocessed

# Calculate the interquartile range (IQR) for UserReviews
IQR_UserReviews <- IQR(Steam_Games_Dataset$UserReviews, na.rm = TRUE)

# Define the upper whisker as Q3 + 1.5 * IQR
upper_whisker <- quantile(Steam_Games_Dataset$UserReviews, 0.75, na.rm = TRUE) + 1.5 * IQR_UserReviews

# Filter out outliers beyond the upper whisker
Steam_Games_Dataset_filtered <- Steam_Games_Dataset %>%
  filter(UserReviews <= upper_whisker)

# Create the scatterplot with more distinct colors
p <- ggplot(Steam_Games_Dataset_filtered, aes(x = UserReviews, y = PositiveReviewPercentage)) +
  geom_point(aes(color = Category), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(
    title = "Scatterplot of User Reviews vs Positive Review Percentage (Without Extreme Outliers)",
    x = "Total Number of Reviews",
    y = "Percentage of Positive Reviews",
    color = "Category"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Single-Player" = "#FFA07A",  # Light Salmon
                                "Multi-Players" = "#20B2AA",  # Light Sea Green
                                "Both" = "#8470FF"))  # Light Slate Blue

# Convert the ggplot object to an interactive plotly object
ggplotly(p) %>%
  layout(legend = list(orientation = "h", x = 0.3, y = -0.1)) %>% # Adjust legend position
  config(tooltip = list(mode = "closest")) # Configure tooltips
