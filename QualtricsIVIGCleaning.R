library(dplyr)

#load_qualtrics_data 
setwd("C:/Documents/BrainInflammationCollaborative/Qualtrics")
#read current data
survey_data <- read.csv("IVIGsurvey_3_6_25_values.csv")
#print(head(survey_data))  # View the first few rows
#print.simple.list(survey_data)

# Convert factors to date format for date survey signed
survey_data$Date <- as.Date(as.character(survey_data$Date), format="%m/%d/%Y") # for MM/DD/YYYY
# Remove rows where conversion failed (removing 2/29/25 which are now NA values)
survey_data <- survey_data[!is.na(survey_data$Date), ]

# Define additional dates that I would like to remove - not real data just test data!
dates_to_remove <- as.Date(c("2025-02-20", "2025-02-17", "2025-02-25"))
# Remove rows with these dates
survey_data <- survey_data[!survey_data$Date %in% dates_to_remove, ]

# Print cleaned data
print(survey_data$Date)

#categorize the responses based on the number of rows
n <- nrow(survey_data)

#probably need to add an if finished to this later (col 7)
#colnames(survey_data)  # This prints all column names

# Convert Age from factor to numeric
survey_data$Age <- as.numeric(as.character(survey_data$Age))
#survey_data$Age <- as.numeric(as.character(survey_data$Age))

# Extract only participant rows (assuming row 3 onward are participants) - not needed now bc no column titles in data
#participant_data <- survey_data[3:(n + 2), ]  # Rows 3 to (n+2) to include only n participants

  # Add AgeGroup as a new column while keeping Age as is
survey_data$AgeGroup <- ifelse(survey_data$Age < 18, "Adolescent", "Adult")

print(head(survey_data))  # View first few rows

  #CoverageCosts = survey_data$CoverageCosts  # use the question asking about % coverage

#things I want to extract
#total time to run  (use duration)
#compare QoL to price paid