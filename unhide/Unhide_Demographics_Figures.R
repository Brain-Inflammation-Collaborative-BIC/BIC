library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(wordcloud)

# Set working directory and load merged data
setwd("/Users/kelseyaguirre/Library/Mobile Documents/com~apple~CloudDocs/Documents/BrainInflammationCollaborative/Unhide/Data")
demographics <- read_excel("10_07_25_Demographics_2.xlsx")

#############################################################################

#calculate age from dob
today <- Sys.Date()
#new age column as numeric
demographics$age <- as.numeric(difftime(today, demographics$participant_date_of_birth, units = "weeks")) / 52.25
demographics$age <- floor(demographics$age)

#############################################################################
#plot age as pie graph  
#use the following bins 18-25, 25-35, 35-45, 45-55, 55-65, 65+

# --- Total participants with valid age ---
n_participants_age <- demographics %>%
  summarize(n = sum(!is.na(age))) %>%
  pull(n)

# --- Age summary (optional, for legend text) ---
age_summary <- demographics %>%
  summarize(
    min_age    = min(age, na.rm = TRUE),
    max_age    = max(age, na.rm = TRUE),
    mean_age   = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE)
  )

# --- Bin ages ---
demographics <- demographics %>%
  mutate(
    age_bin = cut(
      age,
      breaks = c(18, 25, 35, 45, 55, 65, Inf),
      labels = c("18–25", "25–35", "35–45", "45–55", "55–65", "65+"),
      right = FALSE
    )
  )

# --- Count + percentage per bin ---
age_pie_data <- demographics %>%
  filter(!is.na(age_bin)) %>%
  count(age_bin, name = "n") %>%
  mutate(percentage = 100 * n / sum(n))

# --- Define 6-color readable gradient ---
custom_colors <- c(
  "#FFF8B0",  # soft yellow
  "#D8E8A0",  # pale olive
  "#A7D3A6",  # light forest green
  "#7FBFA4",  # green-teal
  "#5E97B0",  # muted sky blue
  "#3E6D99"   # soft navy
)

# --- Create pie chart ---
ggplot(age_pie_data, aes(x = "", y = percentage, fill = age_bin)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = paste0("Age Distribution (N = ", n_participants_age, ")"),
    subtitle = paste0("Median = ", age_summary$median_age,
                      " | Mean = ", round(age_summary$mean_age, 1),
                      " | Range = ", age_summary$min_age, "–", age_summary$max_age),
    fill = "Age Range"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
#############################################################################
#plot sex
# Build data set
sex_data <- demographics %>%
  filter(!is.na(demog_sex)) %>%
  count(demog_sex, name = "n") %>%
  mutate(
    percentage = n / sum(n) * 100,
    demog_sex = tools::toTitleCase(demog_sex)
  )

#  Get total N
n_participants_sex <- sum(sex_data$n)

# Plot
ggplot(sex_data, aes(x = reorder(demog_sex, -n), y = n, fill = percentage)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    vjust = -0.5, size = 4.2
  ) +
  scale_fill_gradientn(
    colours = c("yellow", "forestgreen", "navy")
  ) +
  labs(
    title = ("Participant Sex Distribution"),
    x = NULL,
    y = paste0("Number of Participants (N = ", n_participants_sex, ")"),
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#############################################################################
#gender

# --- Respondent N (any non-missing/non-empty gender response) ---
n_respondents_gender <- demographics %>%
  filter(!is.na(demog_gender), str_squish(demog_gender) != "") %>%
  nrow()

# --- Long (one row per selected gender) ---
gender_long <- demographics %>%
  filter(!is.na(demog_gender), str_squish(demog_gender) != "") %>%
  mutate(demog_gender = str_replace_all(demog_gender, ";", ",")) %>%        # normalize separators if any ';'
  separate_rows(demog_gender, sep = "\\s*,\\s*") %>%                        # split on commas
  mutate(
    demog_gender = str_squish(demog_gender),
    demog_gender = demog_gender[demog_gender != ""],
    # Light normalization (optional – customize as needed)
    demog_gender = str_to_title(demog_gender),
    demog_gender = case_when(
      demog_gender == "Prefer To Self-Describe" ~ "Other",
      TRUE ~ demog_gender
    )  )

# --- Counts + percentages (percent of respondents; can sum >100% in multi-select) ---
gender_data <- gender_long %>%
  count(demog_gender, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(percentage = 100 * n / n_respondents_gender)

# --- Plot: vertical bars, counts on y, % above bars, descending left->right ---
ggplot(gender_data, aes(x = reorder(demog_gender, -n), y = n, fill = percentage)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 4.2) +
  scale_fill_gradientn(colours = c("yellow", "forestgreen", "navy")) +
  labs(
    title = ("Participant Gender (Multi-Select)"),
    x = NULL,
    y = paste0("Number of Participants (N = ", n_respondents_gender, ")")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


#############################################################################
#race
# Canonical labels (display order)
race_levels <- c(
  "Asian or Pacific Islander",
  "Black, African American, or African",
  "White",
  "American Indian or Alaskan Native",
  "Prefer not to say",
  "Prefer to self-describe"
)

# Map full labels -> comma-free tokens (for safe splitting)
#previous split was having trouble with the , in Black option
to_token <- c(
  "Asian or Pacific Islander"           = "API",
  "Black, African American, or African" = "BAAA",
  "White"                               = "WHITE",
  "American Indian or Alaskan Native"   = "AIAN",
  "Prefer not to say"                   = "PNS",
  "Prefer to self-describe"             = "SELF"
)

# And tokens -> display labels (optionally rename SELF -> Other for viewing)
from_token <- c(
  API   = "Asian or Pacific Islander",
  BAAA  = "Black, African American, or African",
  WHITE = "White",
  AIAN  = "American Indian or Alaskan Native",
  PNS   = "Prefer not to say",
  SELF  = "Other"   # <- rename here if you want "Other" in the plot
)

# N respondents with any race response
n_respondents_race <- demographics %>%
  filter(!is.na(demog_race), str_squish(demog_race) != "") %>%
  nrow()

# Replace full labels with tokens, then split on commas safely, then map back
race_long <- demographics %>%
  filter(!is.na(demog_race), str_squish(demog_race) != "") %>%
  mutate(
    txt = demog_race |> 
      str_replace_all(fixed(";"), ",") |>      # normalize separators
      str_replace_all(to_token)                # replace full labels with tokens
  ) %>%
  separate_rows(txt, sep = "\\s*,\\s*") %>%     # split on commas
  mutate(
    token = str_squish(txt),
    race  = recode(token, !!!from_token)        # map tokens -> display labels
  ) %>%
  select(race)

# Counts + percentages (percent of respondents)
race_data <- race_long %>%
  count(race, name = "n") %>%
  mutate(
    percentage = 100 * n / n_respondents_race,
    race = factor(race, levels = c(
      "Asian or Pacific Islander",
      "Black, African American, or African",
      "White",
      "American Indian or Alaskan Native",
      "Prefer not to say",
      "Other"  # include this if you renamed SELF -> Other above
    ))
  ) %>%
  arrange(desc(n))

# Plot
ggplot(race_data, aes(x = reorder(race, -n), y = n, fill = percentage)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            vjust = -0.5, size = 4.2) +
  scale_fill_gradientn(colours = c("yellow", "forestgreen", "navy")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = ("Participant Race (Multi-Select)"),
    x = NULL,
    y = paste0("Number of Participants (N = ", n_respondents_race, ")")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title  = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

#############################################################################
#previous age bar plot like IVIG - not good for this data
# Total participants with a valid age (for legend)
n_participants_age <- demographics %>% summarize(n = sum(!is.na(age))) %>% pull(n)

age_summary <- demographics %>%
  summarize(
    min_age    = min(age, na.rm = TRUE),
    max_age    = max(age, na.rm = TRUE),
    mean_age   = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE)
  )

# Build the plotting data
age_data <- tibble(age = demographics$age) %>%
  filter(!is.na(age)) %>%
  count(age) %>%
  mutate(percentage = n / n_participants_age * 100)

# Legend text
legend_text <- paste0(
  "N = ",   n_participants_age,
  " | Median = ", age_summary$median_age,
  " | Mean = ",   round(age_summary$mean_age, 1),
  " | Range = ",  age_summary$min_age, "–", age_summary$max_age
)

# Plotting data (drop ages occurring <1% - but keeping them in the summary!)
age_plot_data <- demographics %>%
  filter(!is.na(age)) %>%
  count(age, name = "n") %>%
  mutate(percentage = n / n_participants_age * 100) %>%
  filter(percentage >= 1)   # <-- use 'percentage', not 'percent'

#Plot Age figure
ggplot(age_plot_data, aes(
  x = percentage,
  y = factor(age, levels = rev(sort(unique(age))))
)) +
  # shadow
  geom_col(aes(x = percentage - 0.9), fill = "gray30", width = 0.6) +
  # main bars
  geom_col(fill = "navy", color = "white", width = 0.6) +
  # labels
  geom_text(aes(label = paste0(round(percentage), "%")),
            hjust = -0.2, size = 3.5, fontface = "bold") +
  # title & caption
  labs(
    title   = "Current Ages of Participants on Platform",
    x       = NULL,
    y       = "Age of Participants",
    caption = legend_text
  ) +
  # expand x‐axis so labels fit
  xlim(0, max(age_data$percentage) + 10) +
  theme_minimal() +
  theme(
    plot.title          = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.title.position = "plot",
    panel.grid          = element_blank(),
    axis.text.x         = element_blank(),
    axis.ticks.x        = element_blank(),
    axis.ticks.y        = element_line(color = "black"),
    axis.line.y         = element_line(color = "black"),
    axis.text.y         = element_text(color = "black"),
    axis.title.y        = element_text(angle = 0, vjust = 0.5, hjust = 1),
    plot.caption        = element_text(hjust = 0, size = 9, margin = margin(t = 12))
  )
#############################################################################
