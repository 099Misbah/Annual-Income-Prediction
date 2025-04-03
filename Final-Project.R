#LIBRARIES
library(dplyr)
library(tidyr)

#READ DATA
data <- read.csv("/Users/azeemshaikh/Documents/Misbah Docs/College Work_Data/Sem -02/ASDS 6303/Final_Project/income.csv")
#DATA SHAPE
# Get the shape of the dataset
data_shape <- dim(data)
# DATA STRUCTURE
print("Dataset Structure:")
str(data)
# STATISTICAL SUMMARY
print("Summary Statistics:")
summary(data)


# Replace values in the 'marital.status' column
data$marital.status <- ifelse(data$marital.status %in% c("Married-civ-spouse", "Married-AF-spouse"), "Married", "Not in relationship")

#PER-PROCESSING 

#1. NULL VALUES 
#Replace '?' with NAN
data <- read.csv("/Users/azeemshaikh/Documents/Misbah Docs/College Work_Data/Sem -02/ASDS 6303/Final_Project/income.csv",na.strings = "?")
null_values<-is.na(data)
total_missing <- sum(null_values)
#total 3300 missing values we have
#Replace Null values with Mode
# Define a function to replace NA values with mode
replace_na_with_mode <- function(x) {
  mode_value <- names(sort(table(x), decreasing = TRUE))[1]  # Get the mode value
  x[is.na(x)] <- mode_value  # Replace NA values with mode value
  return(x)
}
# Apply the function to each column of the dataset
data <- as.data.frame(lapply(data, replace_na_with_mode))


#2. DUPLICATE ROWS
duplicate_rows <- data[duplicated(data), ]
# Print duplicate rows (if any)
print(duplicate_rows)
sum(duplicated(data))
#Total 12 duplicated rows
#Drop duplicate
data <- distinct(data)
#Now cleaned data


filtered_rows <- data$capital.gain < 50000 & data$capital.loss < 3000

data$capital <- filtered_rows

data <- data[, -which(names(data) == "capital.gain")]

data <- data[, -which(names(data) == "capital.loss")]
#3. Information about the target variable and UNIQUE VALUES
target_info <- table(data$income)
print(target_info)
#UNIQUE VALUES
data_unique <- sapply(data, function(x) n_distinct(x))


#ENCODING
columns_to_encode <- c('workclass', 'marital.status', 'occupation', 'relationship', 'race', 'sex', 'native.country', 'income')

# Encode categorical variables
for (column in columns_to_encode) {
  data_unique[[column]] <- as.numeric(factor(data_unique[[column]]))
}
cols <- c('workclass', 'occupation', 'native.country')

#EDA
#Pie plot for target variable (income)
install.packages("plotrix")
library(plotrix)
custom_colors <- c( "#D2B48C","#A0522D")

#1
# Create a pie chart for the 'income' column
income_counts <- table(data$income)
pie(income_counts, main = "Income", col = custom_colors)
#The diagram above demonstrates that the data for our target is imbalanced. 
#Imbalanced data will have a detrimental impact on our results, thus we will deal with it later.

#2
#Replace categorical values with numbers 
# Columns to encode
columns_to_encode <- c('workclass', 'marital.status', 'occupation', 'relationship', 'race', 'sex', 'native.country', 'income')
# Define a function to encode categorical variables
encode_categorical <- function(column) {
  data[[column]] <- as.integer(factor(data[[column]], levels = unique(data[[column]])))
}
# Apply the encoding function to each column
lapply(columns_to_encode, encode_categorical)


#3
# Columns to exclude from the box plot
columns_to_exclude <- c('sex', 'workclass', 'education.num', 'marital.status', 'occupation', 'race', 'relationship', 'income', 'native.country')

# Get all columns except the ones to exclude
columns <- setdiff(names(data), columns_to_exclude)

# Calculate the number of rows and columns for subplots
num_columns <- length(columns)
num_rows <- ceiling(num_columns / 2)

# Create a new plot window
par(mfrow = c(num_rows, 2))

# Loop through each column to create box plots
for (i in 1:num_columns) {
  column <- columns[i]
  boxplot(data[[column]], main = paste("Box Plot of", column), col = "skyblue")
}

# Reset the plotting parameters
par(mfrow = c(1, 1))

# Convert relevant columns to numeric
numeric_columns <- c("capital.gain", "capital.loss", "hours.per.week")
data[numeric_columns] <- lapply(data[numeric_columns], as.numeric)

# Create box plots
par(mfrow=c(2,2))  # Set the layout to 2x2 grid
for (column in numeric_columns) {
  boxplot(data[[column]], main = paste("Box Plot of", column), col = "skyblue")
}

# Load the ggplot2 library
library(ggplot2)

# Create a bar plot for marital status frequency
marital_status_freq <- table(data$marital.status)
marital_status_df <- as.data.frame(marital_status_freq)
colnames(marital_status_df) <- c("Marital_Status", "Frequency")

# Plot the bar graph
ggplot(data = marital_status_df, aes(x = Marital_Status, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Marital Status", x = "Marital Status", y = "Frequency") +
  



#4
# Define the number of rows and columns for subplots
num_rows <- (num_columns + 1) %/% 2
num_cols <- 2

# Create a new plot window
par(mfrow = c(num_rows, num_cols))

# Set colors
label_color <- "black"
title_color <- "black"

# Loop through each column and plot the histogram
for (column in columns) {
  hist(data[[column]], main = paste("Histogram of", column), xlab = column, col = "skyblue")
}

# Reset the plot window configuration
par(mfrow = c(1, 1))

# Define the variables of interest
variables_of_interest <- c("capital.gain", "capital.loss", "hours.per.week")

# Create a new plot window
par(mfrow = c(1, length(variables_of_interest)))

# Set colors
label_color <- "black"
title_color <- "black"

# Loop through each variable of interest and plot the histogram
for (variable in variables_of_interest) {
  hist(data[[variable]], main = paste("Histogram of", variable), xlab = variable, col = "skyblue")
}

# Reset the plot window configuration
par(mfrow = c(1, 1))


#5
#Univariant Variable
# Load necessary library
library(e1071)

# Select numerical variables
num_vars <- c("age", "education.num", "capital.gain", "capital.loss", "hours.per.week")

# Calculate skewness for each numerical variable
skewness <- sapply(data[num_vars], skewness)

# Print skewness values
print(skewness)

# Select numerical variables for which you want to create histograms
num_vars <- c("age", "fnlwgt", "education.num", "capital.gain", "capital.loss", "hours.per.week")

# Set up the layout for the histograms
par(mfrow = c(2, 3))  # Arrange histograms in a 2x3 grid

# Create histograms for each numerical variable
for (var in num_vars) {
  hist(data[[var]], main = paste("Histogram of", var), xlab = var)
}


# Load necessary libraries
library(ggplot2)

# Create a bar plot for work class frequency
workclass_freq <- table(data$workclass)
workclass_df <- as.data.frame(workclass_freq)
colnames(workclass_df) <- c("Workclass", "Frequency")

# Specify fill colors using shades of blue and yellow
fill_colors <- c("#4682B4", "#87CEEB", "#ADD8E6","#FFA500","#FFD700","#FF6347", "#20B2AA", "#9370DB")

# Plot the bar graph with specified colors
ggplot(data = workclass_df, aes(x = Workclass, y = Frequency, fill = Workclass)) +
  geom_bar(stat = "identity") +  # Use fill aesthetic
  scale_fill_manual(values = fill_colors[1:length(unique(data$workclass))]) +  # Specify fill colors
  labs(title = "Frequency of Work Class", x = "Work Class", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed


#Frequency
#1. Count the frequency of each category in the 'workclass' column
workclass_counts <- table(data$workclass)

# Plot the bar chart
barplot(workclass_counts, main = "Frequency of Workclass", xlab = "Workclass", ylab = "Frequency", col = "skyblue")

#2. Count the frequency of each category in the 'education' column
education_counts <- table(data$education)

# Plot the bar chart
barplot(education_counts, main = "Frequency of Education", xlab = "Education", ylab = "Frequency", col = "skyblue")

#3. Count the frequency of each category in the 'marital.status' column
marital_counts <- table(data$marital.status)

# Plot the bar chart
barplot(marital_counts, main = "Frequency of marital.status", xlab = "marital.status", ylab = "Frequency", col = "skyblue")

#4. Count the frequency of each category in the 'native.country' column
nativecountry_counts <- table(data$native.country)
# Sort the frequency counts in descending order
sorted_nativecountry_counts <- sort(nativecountry_counts, decreasing = TRUE)
# Select the top 5 countries
top_5_countries <- names(sorted_nativecountry_counts)[1:5]
# Subset the table to include only the top 5 countries
top_5_counts <- sorted_nativecountry_counts[1:5]
# Plot the bar chart for the top 5 countries
barplot(top_5_counts, main = "Top 5 Countries by Frequency", xlab = "Country", ylab = "Frequency", col = "skyblue", names.arg = top_5_countries)



#5. Count the frequency of each category in the 'occupation' column
occupation_counts <- table(data$occupation)
# Sort the frequency counts in descending order
sorted_occupation_counts <- sort(occupation_counts, decreasing = TRUE)
# Select the top 5 occupation
top_5_occupation <- names(sorted_occupation_counts)[1:5]
# Subset the table to include only the top 5 occupation
top_5_counts <- sorted_occupation_counts[1:5]
# Plot the bar chart for the top 5 occupation
barplot(top_5_counts, main = "Top 5 occupation by Frequency", xlab = "occupation", ylab = "Frequency", col = "skyblue", names.arg = top_5_occupation)




#Bivariate Analysis
#1. 
# Convert selected columns to numeric
selected_data$age <- as.numeric(as.character(selected_data$age))
selected_data$education.num <- as.numeric(as.character(selected_data$education.num))
selected_data$hours.per.week <- as.numeric(as.character(selected_data$hours.per.week))
selected_data$capital.gain <- as.numeric(as.character(selected_data$capital.gain))
selected_data$capital.loss <- as.numeric(as.character(selected_data$capital.loss))

# Compute the correlation matrix
correlation_matrix <- cor(selected_data)

#2.
# Create age groups
data$age_group <- cut(data$age, breaks = seq(0, 100, by = 10))

# Create the bar plot with facets
ggplot(data = data, aes(x = age_group)) +
  geom_bar(aes(fill = workclass), position = "dodge") +
  labs(x = "Age Group", y = "Count", title = "Bar Plot by Workclass") +
  facet_wrap(~ workclass, scales = "free_y", ncol = 2)



# Load the ggplot2 library
library(ggplot2)

# Convert sex column to factor
data$sex <- factor(data$sex)

# Create the bar plot
ggplot(data = data, aes(x = sex, y = hours.per.week)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Sex", y = "Hours per Week", title = "Hours per Week by Sex")


#3.
bivariate_categorical_plot <- function(data, var_1, var_2) {
  # Create the cross-tabulation
  cross_tab <- table(data[[var_1]], data[[var_2]])
  
  # Create the bar plot with blue and gold colors
  barplot(cross_tab, beside = TRUE, col = c("skyblue", "goldenrod"), 
          main = paste(var_1, "vs", var_2), xlab = var_1, ylab = "Count", 
          legend.text = TRUE)
}




