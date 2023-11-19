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

# Relative path to the dataset from the R script
file_path <- "../Steam Games Dataset.xlsx"
Steam_Games_Dataset <- readxl::read_excel(file_path)

# List of major publishers on steam
major_publishers <- c("Electronic Arts", "CI Games", "Paradox Interactive",
                      "Larian Studios", "Bethesda Softworks", "CD PROJEKT RED",
                      "Activision", "Rockstar Games", "SEGA", "Xbox Game Studios", 
                      "Valve", "Ubisoft", "Gearbox Publishing", 
                      "2k", "KRAFTON, Inc.", "CAPCOM Co., Ltd.", 
                      "Square Enix")

# Flag games as 'Indie' or 'Major' based on publisher
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  mutate(
    Publisher_Type = ifelse(Publisher %in% major_publishers, "Major", "Indie")
  )

# Categorical 
allowed_reviews <- c("Overwhelmingly Positive", "Very Positive", "Mostly Positive", "Positive",
                     "Mixed", "Negative", "Mostly Negative", "Very Negative", "Overwhelmingly Negative")

Steam_Games_Dataset <- Steam_Games_Dataset %>%
  filter(`Recent Reviews Summary` %in% allowed_reviews)

# If 'All Reviews Summary' is empty (or NA), assume it's 'Recent Reviews Summary':
Steam_Games_Dataset$`All Reviews Summary`[is.na(Steam_Games_Dataset$`All Reviews Summary`)] <- Steam_Games_Dataset$`Recent Reviews Summary`[is.na(Steam_Games_Dataset$`All Reviews Summary`)]

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

# View the modified dataset
View(Steam_Games_Dataset)

##Visualisation Techniques
# Calculate the average price for each Publisher Type within each All Reviews Summary category
average_prices <- Steam_Games_Dataset %>%
  group_by(Publisher_Type, `All Reviews Summary`) %>%
  summarise(Average_Price = mean(`Original Price`, na.rm = TRUE))

ggplot(average_prices, aes(x = `All Reviews Summary`, y = Average_Price, fill = Publisher_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(title = "Average Original Price by Review Summary and Publisher Type",
       x = "Review Summary",
       y = "Average Original Price",
       fill = "Publisher Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Indie" = "skyblue", "Major" = "orange")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis text for readability

