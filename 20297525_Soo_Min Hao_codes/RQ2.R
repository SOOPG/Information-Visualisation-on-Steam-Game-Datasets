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

# Define the file path for dataset
file_path <- "C:/Users/wilem/OneDrive/Desktop/Desktop Apps/UNM/Year 3/Fundamentals of InfoViz/CW1/Information-Visualisation-on-Steam-Game-Datasets/Steam Games Dataset.xlsx"

# Read the dataset
steam_reviews <- read_excel(file_path)

# Filter rows based on categorical reviews for 'Recent Reviews Summary'
allowed_reviews <- c("Overwhelmingly Positive", 
                     "Very Positive", 
                     "Mostly Positive", 
                     "Positive",
                     "Overwhelmingly Negative", 
                     "Very Negative", 
                     "Mostly Negative", 
                     "Negative")
steam_reviews <- steam_reviews[steam_reviews$`Recent Reviews Summary` %in% allowed_reviews, ]

# If 'All Reviews Summary' is empty (or NA), use 'Recent Reviews Summary'
steam_reviews$`All Reviews Summary`[is.na(steam_reviews$`All Reviews Summary`)] <- steam_reviews$`Recent Reviews Summary`[is.na(steam_reviews$`All Reviews Summary`)]

# Replace 'FREE' with 0 in 'Original Price' and 'Discounted Price'
steam_reviews$`Original Price`[steam_reviews$`Original Price` == "FREE"] <- 0
steam_reviews$`Discounted Price`[steam_reviews$`Discounted Price` == "FREE"] <- 0

# Remove the $ sign and convert prices to numeric values
steam_reviews$`Original Price` <- as.numeric(gsub("\\$", "", steam_reviews$`Original Price`))
steam_reviews$`Discounted Price` <- as.numeric(gsub("\\$", "", steam_reviews$`Discounted Price`))

# Ensure prices are numeric
steam_reviews$`Original Price` <- as.numeric(as.character(steam_reviews$`Original Price`))
steam_reviews$`Discounted Price` <- as.numeric(as.character(steam_reviews$`Discounted Price`))

# Remove rows with NA values in both price columns
steam_reviews <- steam_reviews[!is.na(steam_reviews$`Original Price`) & !is.na(steam_reviews$`Discounted Price`), ]

# Factorize 'All Reviews Summary' based on the review levels
review_levels <- c("Overwhelmingly Negative", 
                   "Very Negative", 
                   "Mostly Negative", 
                   "Negative", 
                   "Positive", 
                   "Mostly Positive", 
                   "Very Positive", 
                   "Overwhelmingly Positive")
steam_reviews$`All Reviews Summary` <- factor(steam_reviews$`All Reviews Summary`, levels = review_levels)

# Filter out games with prices greater than $70
steam_reviews <- steam_reviews[steam_reviews$`Original Price` <= 70 & steam_reviews$`Discounted Price` <= 70, ]

# Add a 'Price Difference' column
steam_reviews$`Price Difference` <- steam_reviews$`Original Price` - steam_reviews$`Discounted Price`

# Filter out games with no discount
steam_reviews <- steam_reviews[steam_reviews$`Price Difference` > 0, ]

# Prepare the data for visualization
prices_long_format <- steam_reviews %>%
  select(`All Reviews Summary`, `Original Price`, `Discounted Price`) %>%
  gather(key = "Type", value = "Price", -`All Reviews Summary`)

# Generate and display the density plot faceted by review category
density_plot <- ggplot(prices_long_format, aes(x = Price, fill = Type)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~ `All Reviews Summary`, scales = "free") +
  scale_fill_manual(values = c("Original Price" = "blue", "Discounted Price" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Density Plot of Game Prices until 70$ by User Review Category",
       x = "Price",
       y = "Density")

print(density_plot)

