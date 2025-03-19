# Load necessary libraries
library(openxlsx)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)
library(tidygeocoder)
library(stringr)

# Set working directory and load data - had to change to work with my icloud folder
setwd("/Users/kelseyaguirre/Library/Mobile Documents/com~apple~CloudDocs/Documents/BrainInflammationCollaborative/Qualtrics")
IVIG <- read_excel("IVIGsurvey_3_14_25_values.xlsx")

# Clean up the data frame
IVIG <- IVIG[-2, ]  # Remove the second row
colnames(IVIG) <- IVIG[1, ]  # previous code
IVIG <- IVIG[-1, ]  # Remove the first row (now that it's the header)

#removing the ' from enter today's date, recorded date isnt the correct column we need to remove test data
colnames(IVIG) <- gsub("'", "", colnames(IVIG))  # Remove all apostrophes

# Convert to Date format correctly since its already a character
IVIG$`Enter todays date` <- as.Date(IVIG$`Enter todays date`)  
# Remove rows where conversion failed (removing 2/29/25 which are now NA values)
IVIG <- IVIG[!is.na(IVIG$`Enter todays date`), ]

# Define additional dates that I would like to remove - not real data just test data!
dates_to_remove <- as.Date(c("2025-02-20", "2025-02-17", "2025-02-25"))
# Remove rows with these dates
IVIG <- IVIG[!IVIG$`Enter todays date` %in% dates_to_remove, ]
#currently removes 27/50 responses based on consent date
 
# Check for duplicate column names and make them unique
duplicate_names <- names(IVIG)[duplicated(names(IVIG))]
print(duplicate_names)
names(IVIG) <- make.unique(names(IVIG)) #check how this changes them

#next check for the emails!
#i saved the emails only as a new xlsx without a password 
#- was having issues with using the password protected file in R
file_path <- "previousIVIGemails.xlsx"
df <- read_excel(file_path)
# Extract the first column from row 2 to the end
column_data <- df[[1]][-1]  # Remove the first row (header)
# Convert to comma-separated string
previousemails <- paste(column_data, collapse = ", ")
previousemails <- tolower(previousemails)
# Ensure 'previousemails' is a character vector and trim whitespace
previousemails <- trimws(as.character(previousemails))

# Convert complex column name to Email
IVIG <- IVIG %>%
  rename(Email = 'Please provide your email address (this will allow the BIC team to help you with the survey if any questions arise):')
IVIG$Email <- tolower(IVIG$Email)
# Ensure Email column in IVIG is a character vector and trim whitespace
IVIG$Email <- trimws(as.character(IVIG$Email))
# Identify matches
matches <- IVIG$Email %in% previousemails
# Print matching emails
matching_emails <- IVIG$Email[matches]
cat("Matching emails:\n")
print(matching_emails)
# Remove rows with matching emails from IVIG
IVIG <- IVIG[!matches, ]
#what % progress is finished?

#categorize the responses based on the number of rows
n <- nrow(IVIG)
# Convert complex column name to Age
IVIG <- IVIG %>%
  rename(Age = "How old is the patient?\r\n\r\n(REMINDER: If you are a parent or guardian answering on behalf of or alongside your child, please remember that they are the patient)") 
# Convert Age from factor to numeric
IVIG$Age <- as.numeric(as.character(IVIG$Age))

# Add AgeGroup as a new column while keeping Age as is
IVIG$AgeGroup <- ifelse(IVIG$Age < 18, "Adolescent", "Adult")
#print(head(IVIG))

# Reverse geocode latitude and longitude
IVIG <- IVIG %>%
  reverse_geocode(
    lat = "Location Latitude",
    long = "Location Longitude",
    method = "osm",
    address = "location"
  )

# Check the location column
str(IVIG$location)
head(IVIG$location)

# Ensure the location column is a character vector
IVIG$location <- as.character(IVIG$location)

# Create a state lookup table
state_lookup <- data.frame(
  state_name = tolower(state.name),  # Full state names in lowercase
  state_abbr = state.abb             # Two-letter state abbreviations
)

#Extract state name from the location column and map to abbreviation
IVIG <- IVIG %>%
  mutate(
    state_name = str_extract(tolower(location), paste(tolower(state.name), collapse = "|"))  # Extract full state name
  ) %>%
  left_join(state_lookup, by = c("state_name" = "state_name")) %>%
  mutate(
    state = state_abbr  # Use the abbreviation
  ) %>%
  select(-location, -state_name, -state_abbr)  # Drop intermediate columns
#there are a few from out of the US, maybe we could add outisde USA to the blanks in the state column

# Replace NA with 0 in all relevant columns
IVIG <- IVIG %>%
  mutate(across(
    # Gender columns
    starts_with("What gender(s) do you most closely identify with?"),
    ~ replace(., is.na(.), 0)
  )) %>%
  mutate(across(
    # Child gender columns
    starts_with("What gender(s) does your child most closely identify with?"),
    ~ replace(., is.na(.), 0)
  )) %>%
  mutate(across(
    # Child race/ethnicity columns
    starts_with("What is your child's race and/or ethnicity?"),
    ~ replace(., is.na(.), 0)
  )) %>%
  mutate(across(
    # Adult race/ethnicity columns
    starts_with("What is your race and/or ethnicity?"),
    ~ replace(., is.na(.), 0)
  )) %>%
  mutate(across(
    # Diagnosis columns
    starts_with("What condition(s) have you been diagnosed with"),
    ~ replace(., is.na(.), 0)
  )) %>%
  mutate(across(
    # Child diagnosis columns
    starts_with("What condition(s) has your child been diagnosed with"),
    ~ replace(., is.na(.), 0)
  )) %>%
  mutate(across(
    # Work status columns
    starts_with("What is your current work status?"),
    ~ replace(., is.na(.), 0)
     )) %>%
  mutate(across(
      # If the patient had a private insurance columns
      starts_with("If the patient‚Äôs healthcare insurance coverage changed across the three intervals"),
      ~ replace(., is.na(.), 0)
  )) %>%
  mutate(across(
    # School attendance column
    "Does the patient attend school?",
    ~ replace(., is.na(.), 0)
  )) %>%
  mutate(across(
    # Financial strategies columns
    starts_with("Select all financial strategies employed to manage the costs of PANS treatment"),
    ~ replace(., is.na(.), 0)
  )) %>%
  mutate(across(
    # Health insurance columns
    starts_with("Please indicate the type(s) of health insurance or financial assistance that the patient had in each interval"),
    ~ replace(., is.na(.), 0)
  )) %>%
  mutate(across(
    # IVIG treatment reason columns
    starts_with("IVIG has the ability to treat many different ailments"),
    ~ replace(., is.na(.), 0)
  )) %>%
  mutate(`Please specify the percentage of IVIG treatment costs (including both medication and administration) covered by the patient‚Äôs insurance. Make an estimate if you are not entirely certain.` = 
           ifelse(`Please specify the percentage of IVIG treatment costs (including both medication and administration) covered by the patient‚Äôs insurance. Make an estimate if you are not entirely certain.` < 80, 0, 1))
))

# Define columns_to_remove with exact column names from your dataset
columns_to_remove <- c(
  "Start Date",
  "End Date",
  "Response Type",
  "Recipient Last Name",
  "Recipient First Name",
  "Recipient Email",
  "External Data Reference",
  "Location Latitude",
  "Location Longitude",
  "Distribution Channel",
  "User Language",
  "Q_RecaptchaScore",
  "Q_RelevantIDDuplicate",
  "Q_RelevantIDDuplicateScore",
  "Q_RelevantIDFraudScore",
  "Q_RelevantIDLastStartDate",
  "It is important that this survey be completed before the participant has their next birthday. \r\n\r\nPlease provide initials in the box below to confirm that you understand and agree. If the participant has a birthday before the survey is finished, please restart it from the beginning. - Id",
  "It is important that this survey be completed before the participant has their next birthday. \r\n\r\nPlease provide initials in the box below to confirm that you understand and agree. If the participant has a birthday before the survey is finished, please restart it from the beginning. - Name",
  "It is important that this survey be completed before the participant has their next birthday. \r\n\r\nPlease provide initials in the box below to confirm that you understand and agree. If the participant has a birthday before the survey is finished, please restart it from the beginning. - Size",
  "It is important that this survey be completed before the participant has their next birthday. \r\n\r\nPlease provide initials in the box below to confirm that you understand and agree. If the participant has a birthday before the survey is finished, please restart it from the beginning. - Type",
  "It‚Äôs up to me if I want to be in this study or not.",
  "I have to complete all of the questions of the study survey.",
  "Taking part in this study may not directly benefit (help) my health.",
  "By signing this form, I am confirming the following:\r\n\r\n\tI have read fully, and understand the Informed Consent Form.\r\n\tAll of my questions about the study have been answered to my satisfaction.\r\n\tI understand that this study is only observational, and provides no clinical care.\r\n\tI understand that this study platform is not to serve as a means of communication with my doctor for the purpose of clinical care.\r\n\tI am aged 18 ‚Äì 89, inclusive and a resident of the United States.\r\n\tI am fluent in English.\r\n\tI have access to a computer and/or smartphone and the internet.\r\n\tNeither I, nor the child I represent (if applicable) is a ward of the state.\r\n\tI agree to the collection, use, sharing and analysis of my  (or my child‚Äôs) anonymized personal health information and study information collected as part of this study by BIC and other authorized persons as described in this form.\r\n\tAt the completion of the survey, I can download an electronic copy of this signed and dated consent form as well as my survey responses if I so choose.\r\n\tI do not give up any legal rights that I would otherwise have if I were not in this study.\r\n\tIf I am participating as a parent or legal guardian, I understand the importance of not pressuring or coercing my child into assenting for the study or into completing any surveys against his or her will.\r\n\tI understand that I may ask questions at any time and can withdraw my participation without prejudice regarding any future treatment or other opportunities.",
  "Please sign below to indicate that you understand the terms of the consent form. - Id",
  "Please sign below to indicate that you understand the terms of the consent form. - Name",
  "Please sign below to indicate that you understand the terms of the consent form. - Size",
  "Please sign below to indicate that you understand the terms of the consent form. - Type",
  "Would you be interested in hearing about the results of this study, and about other research that BIC is conducting?",
  "It‚Äôs up to me if I want to be in this study or not..1",
  "I have to complete all of the questions of the study survey..1",
  "Taking part in this study may not directly benefit (help) my health..1",
  "Statement of Assent:\r\nI confirm that I am at least 7 and not yet 12 years old, that I have read this information, and that I understand the purpose of this study and its procedures.  I understand that I may ask questions at any time and can withdraw my participation without any consequences to my future treatment or other opportunities.",
  "Please allow your child to sign below, certifying that they understand they are assenting for their information to be used in this survey. - Id",
  "Please allow your child to sign below, certifying that they understand they are assenting for their information to be used in this survey. - Name",
  "Please allow your child to sign below, certifying that they understand they are assenting for their information to be used in this survey. - Size",
  "Please allow your child to sign below, certifying that they understand they are assenting for their information to be used in this survey. - Type",
  "It‚Äôs up to me if I want to be in this study or not..2",
  "I have to complete all of the questions of the study survey..2",
  "Taking part in this study may not directly benefit (help) my health..2",
  "Statement of Assent\r\n\r\nI confirm that I am at least 12 and not yet 18 years old, that I have read this information, and that I understand the purpose of this study and its procedures.  I understand that I may ask questions at any time and can withdraw my participation without any consequences to my future treatment or other opportunities.",
  "Please allow your child to sign below, certifying that they understand they are assenting for their information to be used in this survey. - Id.1",
  "Please allow your child to sign below, certifying that they understand they are assenting for their information to be used in this survey. - Name.1",
  "Please allow your child to sign below, certifying that they understand they are assenting for their information to be used in this survey. - Size.1",
  "Please allow your child to sign below, certifying that they understand they are assenting for their information to be used in this survey. - Type.1"
)


IVIG <- IVIG %>%
  select(-all_of(columns_to_remove))

# Export the cleaned data
library(openxlsx)
write.xlsx(IVIG, file = "IVIG_cleaned.xlsx")
