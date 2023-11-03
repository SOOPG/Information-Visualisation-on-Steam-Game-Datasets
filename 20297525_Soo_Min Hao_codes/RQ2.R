pacman::p_load(pacman,dplyr,GGally,ggplot2,ggthemes,ggvis,httr,lubridate,plotly,rio,rmarkdown,shiny,stringr,tidyr)

library(readxl)
Steam_Games_Dataset <- read_excel("C:/Users/wilem/OneDrive/Desktop/Desktop Apps/UNM/Year 3/Fundamentals of InfoViz/CW1/Information-Visualisation-on-Steam-Game-Datasets/Steam Games Dataset.xlsx")

# Create a working dataframe
df <- Steam_Games_Dataset

#Filter rows based on steam categorical reviews for 'Recent Reviews Summary':
allowed_reviews <- c("Overwhelmingly Positive", 
                     "Very Positive", 
                     "Mostly Positive", 
                     "Positive",
                     "Overwhelmingly Negative", 
                     "Very Negative", 
                     "Mostly Negative", 
                     "Negative")
df <- df[df$`Recent Reviews Summary` %in% allowed_reviews, ]

#If 'All Reviews Summary' is empty (or NA) assume its 'Recent Reviews Summary':
df$`All Reviews Summary`[is.na(df$`All Reviews Summary`)] <- df$`Recent Reviews Summary`[is.na(df$`All Reviews Summary`)]

#Replace FREE with 0 in 'Original Price' and 'Discounted Price'
df$`Original Price`[df$`Original Price` == "FREE"] <- 0
df$`Discounted Price`[df$`Discounted Price` == "FREE"] <- 0

#Remove the $ sign on both dataset columns
df$`Original Price` <- as.numeric(gsub("\\$", "", df$`Original Price`))
df$`Discounted Price` <- as.numeric(gsub("\\$", "", df$`Discounted Price`))

#Remove 'Non-Available (NA)' values on both column
df <- df[!is.na(df$`Original Price`) & !is.na(df$`Discounted Price`), ]

# Convert 'All Reviews Summary' into x-axis
review_levels <- c("Overwhelmingly Negative", 
                   "Very Negative", 
                   "Mostly Negative", 
                   "Negative", 
                   "Positive", 
                   "Mostly Positive", 
                   "Very Positive", 
                   "Overwhelmingly Positive")
df$`All Reviews Summary` <- factor(df$`All Reviews Summary`, levels = review_levels)

# Boxplot for 'Original Price' vs. 'All Reviews Summary' 
original_price_plot <- ggplot(df, aes(x=`All Reviews Summary`, y=`Original Price`)) + 
  geom_boxplot(outlier.shape = NA) +  # Exclude outliers from plot
  coord_cartesian(ylim = c(0, 50)) + # Adjust y-axis limit 
  scale_y_continuous(breaks = seq(0, 50, by = 5)) + # Adjust y-axis breaks
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title="Original Price vs. All Reviews Summary",
       x="All Reviews Summary",
       y="Original Price")
# Display the plot
original_price_plot
# Interactive plot
interactive_original_price_plot <- ggplotly(original_price_plot)
# Display interactive plot
interactive_original_price_plot

# Boxplot for 'Discounted Price' vs. 'All Reviews Summary'
discounted_price_plot <- ggplot(df, aes(x=`All Reviews Summary`, y=`Discounted Price`)) + 
  geom_boxplot(outlier.shape = NA) +  # Exclude outliers from plot
  coord_cartesian(ylim = c(0, 50)) + # Adjust y-axis limit 
  scale_y_continuous(breaks = seq(0, 50, by = 5)) + # Adjust y-axis breaks
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title="Discounted Price vs. All Reviews Summary",
       x="All Reviews Summary",
       y="Discounted Price")
# Display the plot
discounted_price_plot
# Interactive plot
interactive_discounted_price_plot <- ggplotly(discounted_price_plot)
# Display interactive plot
interactive_discounted_price_plot

View(df)
