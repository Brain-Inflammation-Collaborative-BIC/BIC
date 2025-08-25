# Packages
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(openxlsx)

# Set working directory and load merged data
setwd("/Users/kelseyaguirre/Library/Mobile Documents/com~apple~CloudDocs/Documents/BrainInflammationCollaborative/IVIG/Merged")
allIVIG <- read_excel("IVIGDataBook_8_21_25.xlsx", sheet = 2)

#categorize the responses based on the number of rows
n_participants <- nrow(allIVIG)

###############################################################################################
#summarizing insurance coverage and appeals for qualtrics only for the manuscript
colnames(allIVIG)[colnames(allIVIG) == "did_the_patient_aos_health_insurance_plan_cover_the_cost_of_ivig_treatment_at_least_in_part_please_select_the_response_that_best_applies_decoded"] <- "Insurance_IVIGCoverage"

coverage_df <- allIVIG %>%
  filter(!is.na(Insurance_IVIGCoverage)) %>%  # drop blanks
  count(Insurance_IVIGCoverage, name = "N") %>%
  arrange(desc(N)) %>%
  mutate(
    pct         = 100 * N / sum(N),
    pct_label   = paste0(round(pct, 1), "%"),
    # reorder factor so legend & slices match descending order
    coverage    = fct_inorder(Insurance_IVIGCoverage),
    # legend text includes N
    legend_label = paste0(Insurance_IVIGCoverage, " (N = ", N, ")")
  )

# Rainbow palette (same length as number of categories)
pie_cols <- grDevices::rainbow(nrow(coverage_df))

total_n <- sum(coverage_df$N)

# Build a named vector so we can keep order AND wrap long legend lines
legend_map <- setNames(
  str_wrap(coverage_df$legend_label, width = 45),
  levels(coverage_df$coverage)
)

ggplot(coverage_df, aes(x = "", y = N, fill = coverage)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  # % only on slices, in white
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    color = "white", fontface = "bold", size = 3
  ) +
  scale_fill_manual(
    values = pie_cols,
    breaks = levels(coverage_df$coverage),  # preserve descending order
    labels = legend_map[levels(coverage_df$coverage)]
  ) +
  labs(
    title = paste0("IVIG Insurance Coverage (N = ", total_n, ")"),
    fill = NULL
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.text = element_text(size = 9)
  )
###############################################################################################
#save as a xlsx for the databook
# Write to Excel
write.xlsx(
  coverage_df,
  file = "Insurance IVIG Coverage Summary.xlsx",
  sheetName = "CoverageSummary",
  overwrite = TRUE
)

###############################################################################################
