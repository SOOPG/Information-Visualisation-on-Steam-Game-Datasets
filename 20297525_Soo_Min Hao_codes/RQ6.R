#Load libraries
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

# Step 1: Flag Early Access Games
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(
    Early_Access = ifelse(grepl("Early Access", `Popular Tags`), "Yes", "No")
  )

# Step 2: Get review category from column 'All Reviews Summary'
# Categorical 
allowed_reviews <- c("Overwhelmingly Positive", "Very Positive", "Mostly Positive", "Positive",
                     "Mixed", "Negative", "Mostly Negative", "Very Negative", "Overwhelmingly Negative")

Steam_Games_Dataset <- Steam_Games_Dataset %>%
  filter(`Recent Reviews Summary` %in% allowed_reviews)
# If 'All Reviews Summary' is empty (or NA), assume it's 'Recent Reviews Summary':
Steam_Games_Dataset$`All Reviews Summary`[is.na(Steam_Games_Dataset$`All Reviews Summary`)] <- Steam_Games_Dataset$`Recent Reviews Summary`[is.na(Steam_Games_Dataset$`All Reviews Summary`)]

# Step 3: Get Original Price
# Replace FREE with 0 in 'Original Price'
Steam_Games_Dataset$`Original Price`[Steam_Games_Dataset$`Original Price` == "FREE"] <- 0

# Remove the $ sign and convert prices to numeric
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(
    `Original Price` = as.numeric(gsub("\\$", "", `Original Price`)),
    `Discounted Price` = as.numeric(gsub("\\$", "", `Discounted Price`))
  )

# Remove 'Non-Available (NA)' values in original price columns
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  filter(!is.na(`Original Price`))

# Step 4: Get All Reviews Number
# Convert All Reviews Number column into only numerics
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(
    TotalUserReviews = str_extract(`All Reviews Number`, "(?<=of the )\\d+[\\d,]*"),
    TotalUserReviews = str_replace_all(TotalUserReviews, ",", ""),
    TotalUserReviews = as.numeric(TotalUserReviews)
  )

# Remove all rows containing any NAs
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  drop_na()

# Visualization Method
# Calculate the counts 
review_counts <- Steam_Games_Dataset %>%
  group_by(`All Reviews Summary`, Early_Access) %>%
  summarise(Count = n(), .groups = 'drop')

# Create a grouped bar chart
ggplot(review_counts, aes(x = `All Reviews Summary`, y = Count, fill = Early_Access)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Review Outcomes by Early Access Status",
       x = "Review Summary",
       y = "Count of Reviews",
       fill = "Early Access Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

View(Steam_Games_Dataset)

