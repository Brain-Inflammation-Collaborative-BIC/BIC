library(readxl)
library(ggplot2)
library(dplyr)
library(scales)  # for percent()

# Set working directory and load merged data
setwd("/Users/kelseyaguirre/Library/Mobile Documents/com~apple~CloudDocs/Documents/BrainInflammationCollaborative/IVIG/Merged")
allIVIG <- read_excel("IVIG DataBook_8_26_25_final.xlsx", sheet = 2)

#categorize the responses based on the number of rows
n_participants <- nrow(allIVIG)

###############################################################################################
#recode coverage category for donut plot
allIVIG <- allIVIG %>%
  mutate(
    Coverage_cat3 = case_when(
      Coverage_category == "Substantial"    ~ "Substantial",
      Coverage_category == "None to Minimal" ~ "None to Minimal",
      is.na(Coverage_category) | Coverage_category == "" ~ "Unknown",
      TRUE ~ "Unknown"
    ),
    Coverage_cat3 = factor(
      Coverage_cat3,
      levels = c("Substantial", "None to Minimal", "Unknown")  # <-- no extra space
    )
  )

#double check
#table(allIVIG$Coverage_cat3)

#sums for the plot below
cov_summary <- allIVIG %>%
  count(Coverage_cat3) %>%
  mutate(prop = n / sum(n)) 

total_n <- sum(cov_summary$n)

#cov_summary
#labels for the legend to have N
legend_labels <- setNames(
  paste0(cov_summary$Coverage_cat3, " (N = ", cov_summary$n, ")"),
  cov_summary$Coverage_cat3
)

#legend_labels
###############################################################################################
#build the coverage cat donut plot
ggplot(cov_summary, aes(x = 2, y = prop, fill = Coverage_cat3)) +
  # donut ring
  geom_col(width = 1, color = "white") +
  
  # % labels inside each slice (auto-centered in each wedge)
  geom_text(
    aes(label = percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 6
  ) +
  
  #donut structure
  coord_polar(theta = "y") +
  xlim(0, 2.5) +   # <-- extend to 0 so center exists
  
  # center label for total N
  annotate(
    "text",
    x = 0, y = 0,
    label = paste0("Total\nN = ", total_n),
    fontface = "bold",
    size = 8
  ) +
  
  # your hard-coded colors
  scale_fill_manual(
    values = c(
      "Substantial"     = "#FFE76A",
      "None to Minimal" = "#E13BB9",
      "Unknown"         = "#3BAAFF"
    ),
    labels = legend_labels
  ) +
  
  theme_void() +
  theme(
    legend.title  = element_text(size = 14),
    legend.text   = element_text(size = 12),
    plot.title    = element_text(hjust = 0.5, size = 18, face = "bold")
  ) +
  labs(
    title = "IVIG Coverage by Category",
    fill  = "Coverage category"
  )
###############################################################################################
#recode coverage category for donut plot - removed unknown
allIVIG <- allIVIG %>%
  mutate(
    Coverage_cat3 = case_when(
      Coverage_category == "Substantial"    ~ "Substantial",
      Coverage_category == "None to Minimal" ~ "None to Minimal",
      is.na(Coverage_category) | Coverage_category == "" ~ "Unknown",
      TRUE ~ "Unknown"
    ),
    Coverage_cat3 = factor(
      Coverage_cat3,
      levels = c("Substantial", "None to Minimal", "Unknown")  # <-- no extra space
    )
  )

#double check
#table(allIVIG$Coverage_cat3)

# sums for the plot below - EXCLUDING "Unknown"
cov_summary <- allIVIG %>%
  filter(!is.na(Coverage_cat3),
         Coverage_cat3 != "Unknown") %>%   # drop Unknown
  count(Coverage_cat3) %>%
  mutate(prop = n / sum(n))               # % now based only on known coverage

# total N is now only known coverage
total_n <- sum(cov_summary$n)

# labels for the legend to have N (only Substantial / None to Minimal)
legend_labels <- setNames(
  paste0(cov_summary$Coverage_cat3, " (N = ", cov_summary$n, ")"),
  cov_summary$Coverage_cat3
)
###############################################################################################
#build the coverage cat donut plot - removed unknown
ggplot(cov_summary, aes(x = 2, y = prop, fill = Coverage_cat3)) +
  # donut ring
  geom_col(width = 1, color = "white") +
  
  # % labels inside each slice (auto-centered in each wedge)
  geom_text(
    aes(label = percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 6
  ) +
  
  #donut structure
  coord_polar(theta = "y") +
  xlim(0, 2.5) +   # <-- extend to 0 so center exists
  
  # center label for total N
  annotate(
    "text",
    x = 0, y = 0,
    label = paste0("Total\nN = ", total_n),
    fontface = "bold",
    size = 8
  ) +
  
  # your hard-coded colors
  scale_fill_manual(
    values = c(
      "Substantial"     = "#FFE76A",
      "None to Minimal" = "#E13BB9"
    ),
    labels = legend_labels
  )+
  
  theme_void() +
  theme(
    legend.title  = element_text(size = 14),
    legend.text   = element_text(size = 12),
    plot.title    = element_text(hjust = 0.5, size = 18, face = "bold")
  ) +
  labs(
    title = "IVIG Coverage by Category",
    fill  = "Coverage category"
  )