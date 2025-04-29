#---
# title: "Global COVID-19 Testing Data WebScrapping Report"
# author: "Nasir Umar"
# date: "`r Sys.Date()`"
# Purpose: Scrape COVID-19 testing data from Wikipedia and clean for analysis

#--- Load Libraries ---
library(httr)
library(rvest)
library(dplyr)

# --- Step 1: Get Wikipedia Page via HTTP Request ---
wiki_base_url <- "https://en.wikipedia.org/w/index.php"
table_page <- list(title = "Template:COVID-19_testing_by_country")

response <- GET(url = wiki_base_url, query = table_page)

# Check if page loaded successfully
if (response$status_code == 200) {
  cat("Page loaded successfully!\n")
} else {
  stop("Failed to load page, status code:", response$status_code)
}

# --- Step 2: Parse and Extract Tables ---
page_html <- content(response, "text")
page <- read_html(page_html)

table_node <- html_nodes(page, "table")

# Convert to data frames
tables_data_frame <- html_table(table_node, fill = TRUE)

# Select the second table which has the COVID testing data
covid_table_raw_df <- tables_data_frame[[2]]
covid_table_raw_df


# --- Step 3: Pre-process the Extracted Data Frame ---
# View column names (optional)
names(covid_table_raw_df)
# Remove the World row
covid_table_raw_df <- covid_table_raw_df[!(covid_table_raw_df$`Country or region`=="World"),]
# Remove the last row
covid_table_raw_df <- covid_table_raw_df[1:172, ]
# Remove the Units and Ref columns
covid_table_raw_df["Ref."] <- NULL
covid_table_raw_df["Units[b]"] <- NULL
# Renaming the columns
names(covid_table_raw_df) <- c(
  "Country", "Date", "Tested", "Confirmed", 
  "Confirmed.tested.ratio", "Tested.population.ratio", 
  "Confirmed.population.ratio"
)

# Convert column data types
covid_table_raw_df$Country <- as.factor(covid_table_raw_df$Country)
covid_table_raw_df$Date <- as.factor(covid_table_raw_df$Date)
covid_table_raw_df$Tested <- as.numeric(gsub(",","",covid_table_raw_df$Tested))
covid_table_raw_df$Confirmed <- as.numeric(gsub(",","",covid_table_raw_df$Confirmed))
covid_table_raw_df$Confirmed.tested.ratio <- as.numeric(gsub(",","",covid_table_raw_df$Confirmed.tested.ratio))
covid_table_raw_df$Tested.population.ratio <- as.numeric(gsub(",","",covid_table_raw_df$Tested.population.ratio))
covid_table_raw_df$Confirmed.population.ratio <- as.numeric(gsub(",","",covid_table_raw_df$Confirmed.population.ratio))

# --- Step 4: Export the Cleaned Data Frame to CSV ---
write.csv(covid_table_raw_df, "global_covid_testing_data_clean.csv", row.names = FALSE)

cat("Data cleaned and saved successfully as 'global_covid_testing_data_clean.csv'!\n")

#---
# Get the summary of the processed data frame again
head(covid_table_raw_df)
#summary(covid_table_raw_df)


# Load cleaned data
global_covid_testdata <- read.csv(
  "global_covid_testing_data_clean.csv",
  stringsAsFactors = FALSE,
  na.strings = c("NA", "", "N/A")
)

# View first few rows
head(global_covid_testdata)
#Summary
summary(global_covid_testdata)

# Top 10 Countries by Number of Tests Conducted
top_10_tests <- global_covid_testdata %>%
  arrange(desc(Tested)) %>%
  slice_head(n = 10)

top_10_tests

# Install from CRAN 
install.packages("ggplot2")
# Load the package
library(ggplot2)

# Plot: Top 10 Countries by Number of Tests
library(scales)

ggplot(top_10_tests, aes(x = reorder(Country, -Tested), y = Tested/1e6)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  #scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Top 10 Countries by COVID-19 Tests Conducted",
       x = "Country",
       y = "Number of Tests")

# Calculate mean positive ratio 
mean_positive_ratio <- mean(global_covid_testdata$Confirmed.population.ratio, na.rm = TRUE)
mean_positive_ratio

# Calculate worldwide COVID testing positive ratio¶
# Get the total confirmed cases worldwide
total_confirmed <- sum(global_covid_testdata$Confirmed, na.rm = TRUE)
# Get the total tested cases worldwide
total_tested <- sum(global_covid_testdata$Tested, na.rm = TRUE)
# Get the positive ratio (confirmed / tested)
positive_ratio <- round(total_confirmed / total_tested, 4)

print(positive_ratio)

# Countries with confirmed to population ratio rate less than a 5% threshold
# Define threshold
threshold <- 5.0
# Subset countries below the threshold
low_ratio_countries <- global_covid_testdata[
  global_covid_testdata$Confirmed.population.ratio < threshold,
  c("Country", "Confirmed.population.ratio")  
]

# Print results
print(low_ratio_countries)


#install.packages("tinytex")
#tinytex::install_tinytex()


