# Load ggplot2
library(ggplot2)
library(ggrepel)
library(readxl)
library(dplyr)
library(scales)
library(tidyverse)
library(purrr)
library(RColorBrewer)
library(writexl)

#Merged data processing
#bring in the data
# Set working directory and load merged data
setwd("/Users/kelseyaguirre/Library/Mobile Documents/com~apple~CloudDocs/Documents/BrainInflammationCollaborative/IVIG/Merged")
allIVIG <- read_excel("IVIG DataBook_8_26_25_final.xlsx", sheet = 2)

#categorize the responses based on the number of rows
n_participants <- nrow(allIVIG)
# #############################################################################################
#adding in the none to minimal vs substantial categories to re-plot with those
# Categorize based on output text, treating both "" and NA as "Blank"
allIVIG$Coverage_cat_grouped <- dplyr::case_when(
  is.na(allIVIG$Coverage_category) | trimws(allIVIG$Coverage_category) == "" ~ "Blank",
  allIVIG$Coverage_category == "None to Minimal" ~ "None to Minimal",
  allIVIG$Coverage_category == "Substantial" ~ "Substantial",
  TRUE ~ "Other"  # Safety net
)

# defining these to correct N on plot, this needs to be outside the loop:
overall_patient_cols   <- grep("patient", qol_cols, value=TRUE, ignore.case=TRUE)
overall_caretaker_cols <- grep("family|caretaker|cohabitant", qol_cols,
                               value=TRUE, ignore.case=TRUE)

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
#grab your QoL column names
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
##########################################################################################################
#compute per-time-period N (because they don't all match)

n_by_cat_time <- map_dfr(names(category_patterns), function(cat){
  cpat <- category_patterns[[cat]]
  map_dfr(names(time_patterns), function(tp_name){
    tpat <- time_patterns[[tp_name]]
    cols <- grep(paste0(tpat, ".*", cpat, ".*", qpat),
                 qol_cols, ignore.case = TRUE, value = TRUE)
    
    N_any <- if (length(cols)) {
      subdf <- allIVIG[, cols, drop = FALSE]
      sum(rowSums(!is.na(subdf)) > 0)
    } else 0L
    
    tibble(Category = cat, TimePeriod = tp_name, N_any = N_any)
  })
})

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

#cat facet order to make sure patient is displayed first
df_overall$Category <- factor(df_overall$Category, levels = c("Patient", "Caretaker"))

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

#cat facet order to make sure patient is displayed first
mean_df$Category    <- factor(mean_df$Category,    levels = c("Patient", "Caretaker"))

#attach new time level Ns to data frame
df_overall <- df_overall %>%
  left_join(n_by_cat_time, by = c("Category", "TimePeriod"))

mean_df <- mean_df %>%
  left_join(n_by_cat_time, by = c("Category", "TimePeriod"))

# match factor levels you use in plot_df
n_by_cat_time <- n_by_cat_time %>%
  mutate(TimePeriod = factor(TimePeriod, levels = levels(df_overall$TimePeriod)))

##################################################################################################

# Filter out any buckets with no data:
plot_df <- df_overall %>%
  filter(!is.na(Percent) & Percent > 0)

#cat facet order to make sure patient is displayed first
plot_df$Category    <- factor(plot_df$Category,    levels = c("Patient", "Caretaker"))

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

#old
# cat_labs <- c(
#   Patient   = sprintf("Patient (N=%d)",   n_patient),
#   Caretaker = sprintf("Caretaker (N=%d)", n_caretaker)
# )

cat_labs <- n_by_cat_time %>%
  mutate(TimePeriod = factor(TimePeriod, levels = time_lvls)) %>%
  arrange(Category, TimePeriod) %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(
    same = dplyr::n_distinct(N_any) == 1,
    label = if (same) {
      sprintf("%s (N=%d)", dplyr::first(Category), dplyr::first(N_any))
    } else {
      # If Ns differ by time, show them in order (e.g., "N varies: 42 / 37 / 40")
      sprintf("%s (N varies: %s)",
              dplyr::first(Category),
              paste(N_any, collapse = " / "))
    },
    .groups = "drop"
  ) %>%
  { setNames(.$label, .$Category) }
##################################################################################################
#trying to correct Ns for caretaker since some qs were skipped
# ---- helpers ----
get_cols <- function(qpat, tpat, cpat, pool) {
  pat <- paste0(tpat, ".*", cpat, ".*", qpat)
  grep(pat, pool, ignore.case = TRUE, value = TRUE)
}

# Focus only on Caretaker
cpat <- category_patterns[["Caretaker"]]

# Build a tidy table of Caretaker columns with per-column Ns
caretaker_colNs <- map_dfr(names(question_patterns), function(qname) {
  qpat <- question_patterns[[qname]]
  map_dfr(names(time_patterns), function(tp_name) {
    tpat <- time_patterns[[tp_name]]
    cols <- get_cols(qpat, tpat, cpat, qol_cols)

    if (length(cols) == 0) {
      # keep an explicit row so you see what’s missing
      tibble(
        Category   = "Caretaker",
        Question   = qname,
        TimePeriod = tp_name,
        Column     = NA_character_,
        N_col      = 0L
      )
    } else {
      tibble(
        Category   = "Caretaker",
        Question   = qname,
        TimePeriod = tp_name,
        Column     = cols,
        N_col      = vapply(cols, function(cl) sum(!is.na(allIVIG[[cl]])), 0L)
      )
    }
  })
}) %>%
  arrange(Question, TimePeriod, Column)

# Peek
print(caretaker_colNs, n = 100)

##################################################################################################
# prepare a tiny data.frame for the "Mean" header in each panel
mean_header_df <- data.frame(
  Category   = unique(plot_df$Category),
  TimePeriod = factor("3-Month Before IVIG", levels = levels(plot_df$TimePeriod)),
  label      = "Mean"
)
#new colors
my_colors <- c(
  "#a1fdff",  # 1
  "#3baaff",  # 2
  "#1a52c8",  # 3
  "#7459fa",  # 4
  "#bd59fa",  # 5
  "#e13bb9",  # 6
  "#940023",  # 7
  "#ff7734",  # 8
  "#ffe76a",  # 9
  "#cdff3f"   # 10
)
##################################################################################################
# enforce facet order everywhere used in the plot
lvl <- c("Patient", "Caretaker")

df_overall$Category    <- factor(df_overall$Category,    levels = lvl)
plot_df$Category       <- factor(plot_df$Category,       levels = lvl)
mean_df$Category       <- factor(mean_df$Category,       levels = lvl)
mean_header_df$Category<- factor(mean_header_df$Category,levels = lvl)

# (if you also use n_by_cat_time in any plotted layer, lock it too)
n_by_cat_time$Category <- factor(n_by_cat_time$Category, levels = lvl)

# quick diagnostics (optional): confirm the levels in each df
#cat("levels(df_overall$Category): ", paste(levels(df_overall$Category), collapse=", "), "\n")
#cat("levels(plot_df$Category):    ", paste(levels(plot_df$Category), collapse=", "), "\n")
#cat("levels(mean_df$Category):    ", paste(levels(mean_df$Category), collapse=", "), "\n")
#cat("levels(mean_header_df$Category): ", paste(levels(mean_header_df$Category), collapse=", "), "\n")

##################################################################################################
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
    #color    = "white"
    color    = "black"
  ) +
  
  # mean labels just past the 100% line, outside the bar
  geom_text(
    data        = mean_df,
    aes(y = TimePeriod, label = sprintf("%.1f", Mean)),
    x           = 105,
    hjust       = 0,
    inherit.aes = FALSE,
    size        = 3
  ) +
  
  # one "Mean" header above the first bar in each facet
  geom_text(
    data        = mean_header_df,
    aes(x = 106, y = TimePeriod, label = label),
    inherit.aes = FALSE,
    fontface    = "bold",
    size        = 3,    
    nudge_y     =  0.5   # <-- moves it up by 0.5 units
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
    plot.title         = element_text(hjust = 0.5, face = "bold"),
    strip.text         = element_text(face = "bold"),
    panel.grid         = element_blank(),
    panel.background   = element_blank()
  )
###############################################################################################
#removed other question pattern plots - not needed for this manuscript
#############################################################################################
#Create a named list of subsets - plots for subcategories none to minimal and substantial 
coverage_list <- list(
  "None to Minimal" = allIVIG %>% filter(Coverage_cat_grouped == "None to Minimal"),
  "Substantial"     = allIVIG %>% filter(Coverage_cat_grouped == "Substantial")
)

# Pick your question key and pattern
for (qkey in names(question_patterns)) {
  qpat <- question_patterns[[qkey]]
  
  # Loop over each coverage subset for the none to minimal and the substantial
  for (cov_name in names(coverage_list)) {
    data_cov <- coverage_list[[cov_name]]
    
    # recalc Ns: removing any NAs
    n_patient_cov   <- sum(rowSums(!is.na(data_cov[, overall_patient_cols ])) > 0)
    n_caretaker_cov <- sum(rowSums(!is.na(data_cov[, overall_caretaker_cols])) > 0)
    cat_labs_cov <- c(
      Patient   = sprintf("Patient (N=%d)",   n_patient_cov),
      Caretaker = sprintf("Caretaker (N=%d)", n_caretaker_cov)
    )
    
    summary_list_cov <- list()
    for(cat in names(category_patterns)) {
      for(tp in names(time_patterns)) {
        pat  <- paste0(time_patterns[[tp]], ".*",
                       category_patterns[[cat]], ".*", qpat)
        cols <- grep(pat, qol_cols, ignore.case=TRUE, value=TRUE)
        if (!length(cols)) next
        
        vals <- unlist(data_cov[cols], use.names=FALSE) %>% as.numeric()
        vals <- vals[!is.na(vals)]
        tab  <- table(factor(vals, levels=1:10))
        pct  <- as.numeric(tab)/sum(tab)*100
        mn   <- mean(vals)
        
        summary_list_cov[[ paste(cat, tp) ]] <-
          data.frame(Category=cat, TimePeriod=tp,
                     Rating=1:10, Percent=pct, Mean=mn)
      }
    }
    df_cov      <- bind_rows(summary_list_cov)
    plot_df_cov <- df_cov %>%
      filter(Percent>0) %>%
      mutate(TimePeriod=factor(TimePeriod, levels=rev(names(time_patterns))))
    mean_df_cov <- df_cov %>% distinct(Category, TimePeriod, Mean)
    
    # Rebuild the “Mean” header row so it appears above the bottom bar
    mean_header_df_cov <- data.frame(
      Category   = unique(plot_df_cov$Category),
      TimePeriod = factor("3-Month Before IVIG",
                          levels=levels(plot_df_cov$TimePeriod)),
      label      = "Mean"
    )
    
    # #QC verifying each bar really sums to 100 for the new none to minimal and substantial plots
    # # 1) print a header so you know which combo you’re looking at
    # cat("\nVerifying:", qkey, "|", cov_name, "\n")
    # 
    # # 2) group & sum on the exact data you’re plotting
    # plot_df_cov %>%
    #   # optionally tag it so the output shows Question & Coverage too
    #   mutate(
    #     Question = qkey,
    #     Coverage = cov_name
    #   ) %>%
    #   group_by(Question, Coverage, Category, TimePeriod) %>%
    #   summarise(
    #     sum_percent = sum(Percent),
    #     .groups = "drop"
    #   ) %>%
    #   print(n = Inf)
    
    # plot, but change the title line to:
    p <- ggplot(plot_df_cov, aes(Percent, TimePeriod, fill=factor(Rating))) +
      geom_col(position=position_stack(reverse=TRUE), width=0.6, na.rm=TRUE) +
      geom_text(aes(label=paste0(round(Percent), "%")),
                position=position_stack(reverse=TRUE, vjust=0.5),
                size=2.5, color="black") +
      geom_text(data=mean_df_cov,
                aes(y=TimePeriod, label=sprintf("%.1f", Mean)),
                x=105, hjust=0, inherit.aes=FALSE, size=3) +
      geom_text(data=mean_header_df_cov,
                aes(x=106, y=TimePeriod, label=label),
                inherit.aes=FALSE, fontface="bold", size=3, nudge_y=0.5) +
      facet_wrap(~ Category, ncol=1,
                 labeller= labeller(Category = cat_labs_cov)) +
      scale_x_continuous(breaks=NULL, labels=NULL, expand=c(0,0)) +
      coord_cartesian(xlim=c(0,120), clip="off") +
      scale_fill_manual(values=my_colors,
                        name="QoL Rating\nWhere 1 indicates 'Exceedingly Poor' and 10 indicates 'Exceedingly Good'",
                        guide=guide_legend(title.position="top", title.hjust=0.5, nrow=1)) +
      labs(
        title = paste0(qkey, " Ratings for '", cov_name, "'"),
        x     = NULL,
        y     = NULL
      ) +
      theme_minimal(base_size=14) +
      theme(
        plot.title       = element_text(hjust=0.5, face="bold"),
        legend.position  = "top",
        legend.title     = element_text(size=8),
        legend.text      = element_text(size=6),
        strip.text       = element_text(face="bold"),
        panel.grid       = element_blank(),
        panel.background = element_blank(),
        plot.margin      = margin(5.5,40,5.5,5.5)
      )
    
    print(p)
  }
}

###############################################################################################
#summany calculations for easier comparisons 
# ── 1) Overall table summary (first 5 “total” plots) ────────────────────────────────────────────
overall_summary_list <- list()

for (qkey in names(question_patterns)) {
  qpat <- question_patterns[[qkey]]
  for (cat in names(category_patterns)) {
    cpat <- category_patterns[[cat]]
    for (tp in names(time_patterns)) {
      tpat <- time_patterns[[tp]]
      pat  <- paste0(tpat, ".*", cpat, ".*", qpat)
      cols <- grep(pat, qol_cols, ignore.case = TRUE, value = TRUE)
      if (!length(cols)) next
      
      vals <- unlist(allIVIG[cols], use.names = FALSE) %>% 
        as.numeric() %>% 
        na.omit()
      
      overall_summary_list[[paste(qkey, cat, tp)]] <- 
        data.frame(
          Question   = qkey,
          TimePeriod = tp,
          Category   = cat,
          N          = length(vals),
          Mean       = mean(vals)
        )
    }
  }
}

overall_summary_df <- bind_rows(overall_summary_list)

# collapse N+Mean into one column
overall_table <- overall_summary_df %>%
  mutate(Total = paste0(N, " (", round(Mean, 1), ")")) %>%
  select(Question, TimePeriod, Category, Total)

# view it
print(overall_table)
###############################################################################################
# ── 2) Subset summaries (“None to Minimal” vs “Substantial”) ────────────────────

make_sub_summary <- function(group_name) {
  df_list <- list()
  dat <- allIVIG %>% filter(Coverage_cat_grouped == group_name)
  
  for (qkey in names(question_patterns)) {
    qpat <- question_patterns[[qkey]]
    for (cat in names(category_patterns)) {
      cpat <- category_patterns[[cat]]
      for (tp in names(time_patterns)) {
        tpat <- time_patterns[[tp]]
        pat  <- paste0(tpat, ".*", cpat, ".*", qpat)
        cols <- grep(pat, qol_cols, ignore.case = TRUE, value = TRUE)
        if (!length(cols)) next
        
        vals <- unlist(dat[cols], use.names = FALSE) %>% 
          as.numeric() %>% 
          na.omit()
        
        df_list[[paste(qkey, cat, tp)]] <-
          data.frame(
            Question   = qkey,
            TimePeriod = tp,
            Category   = cat,
            N          = length(vals),
            Mean       = mean(vals)
          )
      }
    }
  }
  
  bind_rows(df_list)
}

none_df <- make_sub_summary("None to Minimal") %>%
  mutate(None_to_Minimal = paste0(N, " (", round(Mean, 1), ")")) %>%
  select(Question, TimePeriod, Category, None_to_Minimal)

sub_df  <- make_sub_summary("Substantial") %>%
  mutate(Substantial = paste0(N, " (", round(Mean, 1), ")")) %>%
  select(Question, TimePeriod, Category, Substantial)

subset_table <- left_join(none_df, sub_df,
                          by = c("Question", "TimePeriod", "Category"))

# view it
print(subset_table)

###############################################################################################
# save tabular data in an xlsx
sheets <- list(
  "Overall Summary"   = overall_table,
  "Coverage Subsets"  = subset_table
)

# write out an .xlsx with two tabs
write_xlsx(sheets, path = "QoL_summary_tables_2.xlsx")
###############################################################################################
#QC
#Verify each bar really sums to 100%
# plot_df %>% 
#   group_by(Category, TimePeriod) %>% 
#   summarise(sum_percent = sum(Percent), .groups="drop") %>% 
#   print(n = Inf)
###############################################################################################