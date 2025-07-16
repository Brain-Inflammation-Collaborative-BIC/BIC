# Load ggplot2
library(ggplot2)
library(ggrepel)
library(readxl)
library(dplyr)
library(scales)
library(tidyverse)
library(purrr)
library(RColorBrewer)

#Merged data processing
#bring in the data
# Set working directory and load merged data
setwd("/Users/kelseyaguirre/Library/Mobile Documents/com~apple~CloudDocs/Documents/BrainInflammationCollaborative/IVIG/Merged")
allIVIG <- read_excel("final_merged_data_6_19_2025_2.xlsx", sheet = 1)

#categorize the responses based on the number of rows
n_participants <- nrow(allIVIG)
#############################################################################################
# ───────────────────────────────────────
# Define 5 questions and the regex to pick them out
question_patterns <- list(
  `Overall QoL`          = "overall_quality_of_life_consider_daily_functioning",
  `Physical & Emotional` = "physical_and_emotional_well_being",
  `Social Interactions`  = "social_interactions_consider_the_ease_of_engaging_with_others_and_participation_in_social_activities",
  `Educational/Work`     = "work_performance_consider_the_ease_of_.*workload",
  `Daily Activities`     = "aos_engagement_in_daily_life_activities"
)

# Category patterns
category_patterns <- list(
  Patient   = "the_patients?_",                      
  Caretaker = "the_family_caretaker_cohabitant"     
)

# Time-period patterns
time_patterns <- list(
  `3-Month Before IVIG` = "3_month_period_before_ivig_prescription",
  `Waiting Period`      = "waiting_period_between_prescription_and_first_ivig_treatment",
  `6-Month After IVIG`  = "(6_month_period_after_ivig_prescription|6_month_period_after_first_ivig_treatment)"
)

# ───────────────────────────────────────
# grab your QoL column names
qol_cols <- grep(
  "^please_rate_each_aspect_of_quality_of_life",
  names(allIVIG),
  value = TRUE,
  ignore.case = TRUE
)

# 2. Inspect matches for each
for (q in names(question_patterns)) {
  pat  <- question_patterns[[q]]
  cols <- grep(pat, qol_cols, ignore.case = TRUE, value = TRUE)
  cat("\n--", q, "--\n")
  cat("Matches:", length(cols), "\n")
  if (length(cols)) print(cols)
}

##########################################################################################################
# question = Overall QoL
# 1. Set your Overall QoL pattern
qpat <- question_patterns[["Overall QoL"]]

# 2. Sample your column names
head(qol_cols, 5)

# 3. Debug loop with correct order: time → category → question
for(cat in names(category_patterns)) {
  for(tp_name in names(time_patterns)) {
    tpat <- time_patterns[[tp_name]]
    cpat <- category_patterns[[cat]]
    pat  <- paste0(tpat, ".*", cpat, ".*", qpat)
    
    cols <- grep(pat, qol_cols, ignore.case = TRUE, value = TRUE)
    
    message(sprintf("[%s | %s] → %d cols", cat, tp_name, length(cols)))
    if (length(cols)) print(cols)
  }
}

##################################################################################################

# Rebuild summary for “Overall QoL”
summary_list <- list()
for(cat in names(category_patterns)) {
  cpat <- category_patterns[[cat]]
  for(tp_name in names(time_patterns)) {
    tpat <- time_patterns[[tp_name]]
    pat  <- paste0(tpat, ".*", cpat, ".*", qpat)
    
    cols <- grep(pat, qol_cols, ignore.case = TRUE, value = TRUE)
    if (!length(cols)) next
    
    vals <- unlist(allIVIG[cols], use.names = FALSE)
    vals <- as.numeric(vals)
    vals <- vals[!is.na(vals)]
    
    tab <- table(factor(vals, levels = 1:10))
    pct <- as.numeric(tab) / sum(tab) * 100
    mn  <- mean(vals)
    
    summary_list[[paste(cat, tp_name)]] <- 
      data.frame(
        Category   = cat,
        TimePeriod = tp_name,
        Rating     = 1:10,
        Percent    = pct,
        Mean       = mn
      )
  }
}
df_overall <- bind_rows(summary_list)

#    Category, TimePeriod, Rating, Percent, Mean
#    and that TimePeriod is a factor in the order you want:
time_lvls <- c(
  "3-Month Before IVIG",
  "Waiting Period",
  "6-Month After IVIG"
)

# Create ordered factor for plotting
time_lvls  <- names(time_patterns)
combo_lvls <- c(
  paste("Patient",   time_lvls, sep = " • "),
  paste("Caretaker", time_lvls, sep = " • ")
)
df_overall$TimePeriod <- factor(df_overall$TimePeriod, levels = time_lvls)

df_overall <- df_overall %>%
  mutate(
    Combo = factor(
      paste(Category, TimePeriod, sep = " • "),
      levels = combo_lvls
    )
  )

#Ensure TimePeriod is a factor in the correct top-to-bottom order
df_overall$TimePeriod <- factor(
  df_overall$TimePeriod,
  levels = c("3-Month Before IVIG", "Waiting Period", "6-Month After IVIG")
)

mean_df <- df_overall %>%
  distinct(Category, TimePeriod, Mean) %>%
  mutate(
    Combo = factor(paste(Category, TimePeriod, sep = " • "), levels = combo_lvls)
  )

#Prepare the mean‐label data.frame
mean_df <- df_overall %>%
  distinct(Category, TimePeriod, Mean)

##################################################################################################

# Filter out any buckets with no data:
plot_df <- df_overall %>%
  filter(!is.na(Percent) & Percent > 0)

# (Optional) Double‐check you’ve removed them:
table(is.na(plot_df$Percent))
# should show: FALSE → all rows

# Make sure TimePeriod is a factor in the order you want
plot_df$TimePeriod <- factor(
  plot_df$TimePeriod,
  levels = rev(time_lvls)
)

##################################################################################################
#to add the N to the plot - checked with the data and it's correct
overall_patient_cols   <- grep("patient", qol_cols, value = TRUE, ignore.case = TRUE)
overall_caretaker_cols <- grep("family|caretaker|cohabitant", qol_cols, value = TRUE, ignore.case = TRUE)
  
n_patient   <- sum(rowSums(!is.na(allIVIG[ , overall_patient_cols])) > 0)
n_caretaker <- sum(rowSums(!is.na(allIVIG[ , overall_caretaker_cols])) > 0)

# cat("N (Patient):   ", n_patient,   "\n")
# cat("N (Caretaker): ", n_caretaker, "\n")

cat_labs <- c(
  Patient   = sprintf("Patient (N=%d)",   n_patient),
  Caretaker = sprintf("Caretaker (N=%d)", n_caretaker)
)

##################################################################################################
# Define your 10-step red→violet palette
my_colors <- c(
  "#7f0000",  # 1
  "#b30000",  # 2
  "#d7301f",  # 3
  "#fc8d59",  # 4
  "#fee08b",  # 5
  "#d9ef8b",  # 6
  "#91cf60",  # 7
  "#1a9850",  # 8
  "#2c7bb6",  # 9
  "#7b3294"   # 10
)
##################################################################################################

#different approach 
#Swapped the mapping so Percent is on the x and TimePeriod on the y, then keep scale_x_continuous()
ggplot(plot_df, aes(
  x    = Percent,      # continuous for scale_x_continuous()
  y    = TimePeriod,   # discrete categories
  fill = factor(Rating)
)) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width    = 0.6,
    na.rm    = TRUE
  ) +
  
  # percent labels inside each segment
  geom_text(
    aes(label = paste0(round(Percent), "%")),
    position = position_stack(reverse = TRUE, vjust = 0.5),
    size     = 2.5,
    color    = "white"
  ) +
  
  # mean labels just past the 100% line, outside the bar
  geom_text(
    data        = mean_df,
    aes(y = TimePeriod, label = sprintf("%.1f", Mean)),
    x           = 100,
    hjust       = -0.1,
    inherit.aes = FALSE,
    size        = 3
  ) +
  
  facet_wrap(
    ~ Category, 
    ncol    = 1,
    labeller = as_labeller(cat_labs)
  ) +

  # remove all x‐axis ticks & labels
  scale_x_continuous(
    breaks = NULL,
    labels = NULL,
    expand = c(0, 0)
  ) +
  
  coord_cartesian(
    xlim = c(0, 120),   # extend drawing area
    clip = "off"        # allow text outside panel
  ) +
  
  scale_fill_manual(
    values = my_colors,
    name   = "QoL Rating\n Where 1 indicates 'Exceedingly Poor' and 10 indicates 'Exceedingly Good'",
    guide  = guide_legend(
      title.position = "top",
      title.hjust    = 0.5,
      nrow           = 1
    )
  ) +
  
  labs(
    title = "Overall Quality of Life Ratings",
    x     = NULL,
    y     = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "top",
    legend.title       = element_text(size = 8),
    legend.text        = element_text(size = 6),
    strip.text         = element_text(face = "bold"),
    panel.grid         = element_blank(),
    panel.background   = element_blank()
  )


###############################################################################################
#Verify each bar really sums to 100%
# plot_df %>% 
#   group_by(Category, TimePeriod) %>% 
#   summarise(sum_percent = sum(Percent), .groups="drop") %>% 
#   print(n = Inf)








###############################################################################################
#previous approach

# # Plot
# ggplot(plot_df, aes(
#   x    = TimePeriod,          # ← discrete categories on x
#   y    = Percent,             # ← numeric on y
#   fill = factor(Rating)       # ← color by rating
# )) +
#   # full-length stacked bars, reversed so 1 sits on the left
#   geom_col(
#     width    = 0.6,
#     position = position_stack(reverse = TRUE),  # ← reverse here
#     na.rm    = TRUE
#   ) +
#   # mean labels pinned just beyond the 100% line
#   geom_text(
#     data        = mean_df,
#     aes(label = sprintf("%.1f", Mean), y = TimePeriod),
#     x           = 100,           # fixed at 100%
#     hjust       = -0.1,          # nudge outside
#     inherit.aes = FALSE,
#     size        = 3
#   ) +
#   
#   # one panel for Patient on top, Caretaker below
#   facet_wrap(~ Category, ncol = 1) +
#   
#   scale_y_continuous(
#     limits = c(0, 100),
#     expand = c(0, 0),
#     breaks = seq(0, 100, 20),
#     labels = function(x) paste0(x, "%")
#   ) +
#   
#   # swap axes so bars run left→right
#   coord_flip() +                #after setting the geoms
#   
#   scale_fill_manual(
#     values = my_colors,
#     name   = "QoL Rating\n(1 = Poor … 10 = Good)",
#     guide  = guide_legend(
#       title.position = "top",
#       title.hjust    = 0.5,
#       nrow           = 1
#     )
#   ) +
#   
#   labs(
#     title = "Overall Quality of Life Ratings",
#     x     = NULL,
#     y     = "Percent of All Ratings"
#   ) +
#   
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position    = "top",
#     legend.title       = element_text(size = 8),
#     legend.text        = element_text(size = 6),
#     strip.text         = element_text(face = "bold"),
#     panel.grid.major.y = element_blank()
#   )
