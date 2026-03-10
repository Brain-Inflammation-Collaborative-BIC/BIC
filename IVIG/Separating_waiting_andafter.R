library(readxl)
library(dplyr)

# Set working directory and load cleaned data
setwd("/Users/kelseyaguirre/Library/Mobile Documents/com~apple~CloudDocs/Documents/BrainInflammationCollaborative/IVIG/Merged")

# read 3rd tab of workbook
IVIG <- read_excel("IVIG DataBook_2_08_26.xlsx", sheet = 3)

#-------------------------------
# Financial strategy columns
#-------------------------------
financial_before_cols <- grep("^financial_strategy.*_before$",
                              names(IVIG), value = TRUE)

financial_after_cols  <- grep("^financial_strategy.*_after$",
                              names(IVIG), value = TRUE)

IVIG <- IVIG %>%
  mutate(
    any_financial_strategy_before = as.integer(
      rowSums(across(all_of(financial_before_cols), ~ .x == 1), na.rm = TRUE) > 0
    ),
    any_financial_strategy_after = as.integer(
      rowSums(across(all_of(financial_after_cols), ~ .x == 1), na.rm = TRUE) > 0
    )
  ) %>%
  relocate(any_financial_strategy_before, .after = all_of(financial_before_cols)) %>%
  relocate(any_financial_strategy_after,  .after = all_of(financial_after_cols))

#-------------------------------
# Health insurance columns
#-------------------------------
health_before_cols <- grep("^healthinsurance.*_before$",
                           names(IVIG), value = TRUE)

health_after_cols  <- grep("^healthinsurance.*_after$",
                           names(IVIG), value = TRUE)

IVIG <- IVIG %>%
  mutate(
    any_healthinsurance_before = as.integer(
      rowSums(across(all_of(health_before_cols), ~ .x == 1), na.rm = TRUE) > 0
    ),
    any_healthinsurance_after = as.integer(
      rowSums(across(all_of(health_after_cols), ~ .x == 1), na.rm = TRUE) > 0
    )
  ) %>%
  relocate(any_healthinsurance_before, .after = all_of(health_before_cols)) %>%
  relocate(any_healthinsurance_after,  .after = all_of(health_after_cols))

#to get the accurate Ns- regardless of coverage category
sum(IVIG$any_financial_strategy_after, na.rm = TRUE)
sum(IVIG$any_healthinsurance_after, na.rm = TRUE)

#to get the accurate Ns- defined by coverage category for the recent manuscript edits
sum(IVIG$any_financial_strategy_after == 1 & IVIG$Coverage_category == "None to Minimal", na.rm = TRUE)
sum(IVIG$any_financial_strategy_after == 1 & IVIG$Coverage_category == "Substantial", na.rm = TRUE)

sum(IVIG$any_healthinsurance_after == 1 & IVIG$Coverage_category == "None to Minimal", na.rm = TRUE)
sum(IVIG$any_healthinsurance_after == 1 & IVIG$Coverage_category == "Substantial", na.rm = TRUE)


#just for additional work N - qualtrics question only
#create subset
IVIG_subset <- IVIG[1:30, ]
#cross-ref additional work responses and cov category
IVIG_subset %>%
  count(financial_strategy_additional_work_including_working_additional_hours_at_the_same_job_or_working_an_additional_job_after, Coverage_category)
IVIG_subset %>%
  filter(financial_strategy_additional_work_including_working_additional_hours_at_the_same_job_or_working_an_additional_job_after == 1) %>%
  count(Coverage_category)

#####################################################################################
#now having R re-do these calculations to confirm
#create 4 analysis groups
IVIG <- IVIG %>%
  mutate(
    coverage_group = case_when(
      Coverage_category == "Substantial" ~ "Substantial Insurance Coverage",
      Coverage_category == "None to Minimal" ~ "Minimal/No Insurance Coverage"
    )
  )

#identify financial strategies columns
financial_before <- grep("^financial_strategy.*_before$", names(IVIG), value = TRUE)
financial_after  <- grep("^financial_strategy.*_after$", names(IVIG), value = TRUE)

#verify the denominators
IVIG %>%
  filter(coverage_group == "Substantial Insurance Coverage") %>%
  summarise(
    before_n = sum(any_financial_strategy_before == 1, na.rm = TRUE),
    after_n  = sum(any_financial_strategy_after == 1, na.rm = TRUE)
  )

IVIG %>%
  filter(coverage_group == "Minimal/No Insurance Coverage") %>%
  summarise(
    before_n = sum(any_financial_strategy_before == 1, na.rm = TRUE),
    after_n  = sum(any_financial_strategy_after == 1, na.rm = TRUE)
  )

#calculate %s for each
library(tidyr)

financial_summary_before <- IVIG %>%
  filter(any_financial_strategy_before == 1) %>%
  group_by(coverage_group) %>%
  summarise(across(all_of(financial_before), ~sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(-coverage_group,
               names_to = "strategy",
               values_to = "count") %>%
  mutate(
    denom = ifelse(coverage_group == "Substantial Insurance Coverage", 21, 12),
    percent = round(100 * count / denom, 1)
  )

#same for after period
financial_summary_after <- IVIG %>%
  filter(any_financial_strategy_after == 1) %>%
  group_by(coverage_group) %>%
  summarise(across(all_of(financial_after), ~sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(-coverage_group,
               names_to = "strategy",
               values_to = "count") %>%
  mutate(
    denom = ifelse(coverage_group == "Substantial Insurance Coverage", 21, 16),
    percent = round(100 * count / denom, 1)
  )

#format to match table in the manuscript
financial_summary_before %>%
  mutate(result = paste0(percent, "% (", count, ")"))

#triple checking the COUNTS for the additional work question
library(dplyr)

before_col <- "financial_strategy_additional_work_including_working_additional_hours_at_the_same_job_or_working_an_additional_job_before"
after_col  <- "financial_strategy_additional_work_including_working_additional_hours_at_the_same_job_or_working_an_additional_job_after"

IVIG_additional <- IVIG %>%
  slice(1:30)

# denominators + numerators together
additional_work_summary <- bind_rows(
  IVIG_additional %>%
    filter(any_financial_strategy_before == 1) %>%
    group_by(Coverage_category) %>%
    summarise(
      timepoint = "Before",
      denom = n(),
      count = sum(.data[[before_col]] == 1, na.rm = TRUE),
      .groups = "drop"
    ),
  IVIG_additional %>%
    filter(any_financial_strategy_after == 1) %>%
    group_by(Coverage_category) %>%
    summarise(
      timepoint = "After",
      denom = n(),
      count = sum(.data[[after_col]] == 1, na.rm = TRUE),
      .groups = "drop"
    )
) %>%
  mutate(
    percent = round(100 * count / denom, 1),
    result = paste0(percent, "% (", count, ")"),
    denom_label = paste0("N = ", denom)
  )

additional_work_summary