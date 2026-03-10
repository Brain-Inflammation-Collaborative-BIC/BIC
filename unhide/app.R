#app.R

# LOAD + PREP YOUR DATA - update as needed
library(readxl)

# Set working directory and load merged data
setwd("/Users/kelseyaguirre/Library/Mobile Documents/com~apple~CloudDocs/Documents/BrainInflammationCollaborative/Unhide/Data/DemographicsDashboard")
demographics <- read_excel("3_03_26_Demographics.xlsx")

################################################################################################################################
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)
library(tidyr)
library(tools)
library(ggrepel)

# ---- Calculate age from DOB ----
today <- Sys.Date()
demographics <- demographics |>
  mutate(
    age = as.numeric(difftime(today, participant_date_of_birth, units = "weeks")) / 52.25,
    age = floor(age)
  )

demo <- demographics

# ---- Race label helpers updated ----
race_levels <- c(
  "Asian or Pacific Islander",
  "Black, African American, or African",
  "White",
  "American Indian or Alaskan Native",
  "Other/Undisclosed"
)

to_token <- c(
  "Asian or Pacific Islander"            = "API",
  "Black, African American, or African"  = "BAAA",
  "White"                                = "WHITE",
  "American Indian or Alaskan Native"    = "AIAN",
  "Prefer not to say"                    = "PNS",
  "Prefer to self-describe"              = "SELF"
)

from_token <- c(
  API   = "Asian or Pacific Islander",
  BAAA  = "Black, African American, or African",
  WHITE = "White",
  AIAN  = "American Indian or Alaskan Native",
  PNS   = "Other/Undisclosed",
  SELF  = "Other/Undisclosed"
)

# 7 colors for plotting
custom_colors <- c(
  "#FFE5B4",  # <18 - peach
  "#FFF8B0",  # 18–25 - soft yellow
  "#D8E8A0",  # 25–35 - pale olive
  "#A7D3A6",  # 35–45 - light forest green
  "#7FBFA4",  # 45–55 - green-teal
  "#5E97B0",  # 55–65 - muted sky blue
  "#3E6D99"   # 65+ - soft navy
)

################################################################################################################################
#### 2. UI ####
ui <- fluidPage(
  
  # Dynamic dashboard title with N = count
  fluidRow(
    column(
      width = 12,   # must be between 1 and 12
      h2(textOutput("dashboard_title"),
         style = "text-align:center; font-weight:bold; margin-top:20px;")
    )
  ),
  fluidRow(
    
    # LEFT COLUMN: Age (top) + Sex (bottom)
    column(
      width = 6,
      
      h3("Age Distribution"),
      plotOutput("age_pie", height = "380px"),   # taller
      br(),
      
      h3("Sex (Binary View)"),
      uiOutput("sex_icons"),
      br()
    ),
    
    # RIGHT COLUMN: Race (top) + Gender (bottom)
    column(
      width = 6,
      
      h3("Race (Multi-select)"),
      plotOutput("race_bar", height = "380px"),   # taller
      br(),
      
      h3(HTML("Gender Identity<br/>(Multi-select)")),
      plotOutput("gender_bar", height = "300px")  # can adjust if needed
    )
  )
)

################################################################################################################################
#### 3. SERVER ####

server <- function(input, output, session) {
  
  #dynamic title that only has N listed once at the top
  output$dashboard_title <- renderText({
    total_n <- sum(!is.na(demo$age))
    paste0("Demographics Dashboard (N = ", total_n, ")")
  })

  # ---- 1) AGE PIE ----
  output$age_pie <- renderPlot({
    df <- demo
    df <- df |> dplyr::filter(!is.na(age))
    if (nrow(df) == 0) return(NULL)
    
    # N with valid age
    n_participants_age <- df %>%
      summarize(n = sum(!is.na(age))) %>%
      pull(n)
    if (n_participants_age == 0) return(NULL)
    
    # Age summary
    age_summary <- df %>%
      summarize(
        min_age    = min(age, na.rm = TRUE),
        max_age    = max(age, na.rm = TRUE),
        mean_age   = mean(age, na.rm = TRUE),
        median_age = median(age, na.rm = TRUE)
      )
    
    # Bin ages + compute slice centers
    age_pie_data <- df %>%
      mutate(
        age_bin = cut(
          age,
          breaks = c(-Inf, 18, 25, 35, 45, 55, 65, Inf),
          labels = c("<18", "18–25", "25–35", "35–45", "45–55", "55–65", "65+"),
          right  = FALSE
        )
      ) %>%
      filter(!is.na(age_bin)) %>%
      count(age_bin, name = "n") %>%
      mutate(
        percentage = 100 * n / sum(n)
      ) %>%
      arrange(age_bin) %>%
      mutate(
        cum_pct = cumsum(percentage),
        label_y = cum_pct - percentage / 2      # center of each slice
      )
    
      ggplot(age_pie_data, aes(x = 1, y = percentage, fill = age_bin)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      
      # 1) INSIDE labels for all bins EXCEPT "<18"
      geom_text(
        data = subset(age_pie_data, age_bin != "<18"),
        aes(label = paste0(round(percentage, 1), "%")),
        position = position_stack(vjust = 0.5),
        size = 6
      ) +
      
      # 2) OUTSIDE label for "<18" only, with pointer and vertical tweak
      ggrepel::geom_label_repel(
        data = subset(age_pie_data, age_bin == "<18") |> 
          dplyr::mutate(label_y = label_y + 5),   # move label "up" a bit
        aes(
          y     = label_y,
          mutate(label_y = label_y + 10
                 ),
          label = paste0(round(percentage, 1), "%")
        ),
        x            = 1,
        nudge_x      = 0.35,
        show.legend  = FALSE,
        size         = 5.5,
        label.size   = 0.25,
        color        = "black",
        segment.color = "grey40",
        segment.size  = 0.4
      ) +
      
      scale_fill_manual(values = custom_colors) +
      labs(
        subtitle = paste0(
          "Median = ", age_summary$median_age,
          " | Mean = ", round(age_summary$mean_age, 1),
          " | Range = ", age_summary$min_age, "–", age_summary$max_age
        ),
        fill = "Age Range"
      ) +
      theme_minimal(base_size = 18) +
      theme(
        plot.title    = element_text(size = 22, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title    = element_blank(),
        axis.text     = element_blank(),
        axis.ticks    = element_blank(),
        panel.grid    = element_blank()
      )
  })
  
  
  # ---- 2) SEX ICONS ----
  output$sex_icons <- renderUI({
    df <- demo
    if (nrow(df) == 0) {
      return(tags$p("No data."))
    }
    
    sex_clean <- tolower(df$demog_sex)
    total <- sum(!is.na(sex_clean))
    if (total == 0) {
      return(tags$p("No sex data."))
    }
    
    n_female <- sum(sex_clean == "female", na.rm = TRUE)
    n_male   <- sum(sex_clean == "male",   na.rm = TRUE)
    
    pct_female <- round(100 * n_female / total, 1)
    pct_male   <- round(100 * n_male   / total, 1)
    
    tags$div(
      style = "display: flex; justify-content: space-around; 
             align-items: center; margin-top: 20px; margin-bottom: 10px;",
      
      # Male
      tags$div(
        style = "text-align: center;",
        tags$div(style = "font-size: 110px; line-height: 1;", HTML("&#9794;")),   # icon
        tags$div(style = "font-size: 32px; font-weight: bold; margin-top: 12px;",
                 paste0(pct_male, "%")),                                          # percentage
        tags$div(style = "font-size: 26px; margin-top: 4px;", "Male")             # label
      ),
      
      # Female
      tags$div(
        style = "text-align: center;",
        tags$div(style = "font-size: 110px; line-height: 1;", HTML("&#9792;")),   # icon
        tags$div(style = "font-size: 32px; font-weight: bold; margin-top: 12px;",
                 paste0(pct_female, "%")),                                        # percentage
        tags$div(style = "font-size: 26px; margin-top: 4px;", "Female")           # label
      )
    )
  })
  
  
  # ---- 3) GENDER BAR (multi-select) ----
  output$gender_bar <- renderPlot({
    df <- demo
    if (nrow(df) == 0) return(NULL)
    
    df_gender <- df %>%
      filter(!is.na(demog_gender), str_squish(demog_gender) != "")
    
    n_respondents_gender <- nrow(df_gender)
    if (n_respondents_gender == 0) return(NULL)
    
    gender_long <- df_gender %>%
      mutate(demog_gender = str_replace_all(demog_gender, ";", ",")) %>%
      separate_rows(demog_gender, sep = "\\s*,\\s*") %>%
      mutate(
        demog_gender = str_squish(demog_gender),
        demog_gender = demog_gender[demog_gender != ""],
        demog_gender = str_to_title(demog_gender),
        demog_gender = case_when(
          demog_gender == "Prefer To Self-Describe" ~ "Other",
          TRUE ~ demog_gender
        )
      )
    
    gender_data <- gender_long %>%
      count(demog_gender, name = "n") %>%
      arrange(desc(n)) %>%
      mutate(percentage = 100 * n / n_respondents_gender)
    
    ggplot(gender_data, aes(x = reorder(demog_gender, -n), y = n, fill = percentage)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = paste0(round(percentage, 1), "%")),
        vjust = -0.4, size = 6.3 
      ) +
      scale_fill_gradientn(colours = c("yellow", "forestgreen", "navy")) +
      labs(
        x = NULL,
        y = NULL,           # no y-axis label
        title = NULL        # no internal title
      ) +
      theme_minimal(base_size = 20) +
      theme(
        axis.text.x = element_text(size = 16, angle = 20),
        axis.text.y = element_text(size = 16),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08)))
  })
  # ---- 4) RACE BAR (multi-select) ----
  output$race_bar <- renderPlot({
    df <- demo
    if (nrow(df) == 0) return(NULL)
    
    df_race <- df %>%
      filter(!is.na(demog_race), str_squish(demog_race) != "")
    
    n_respondents_race <- nrow(df_race)
    if (n_respondents_race == 0) return(NULL)
    
    race_long <- df_race %>%
      mutate(
        txt = demog_race |>
          str_replace_all(fixed(";"), ",") |>
          str_replace_all(to_token)
      ) %>%
      separate_rows(txt, sep = "\\s*,\\s*") %>%
      mutate(
        token = str_squish(txt),
        race  = recode(token, !!!from_token)
      ) %>%
      select(race)
    
    race_data <- race_long %>%
      count(race, name = "n") %>%
      mutate(
        percentage = 100 * n / n_respondents_race,
        race = factor(
          race,
          levels = c(
            "Asian or Pacific Islander",
            "Black, African American, or African",
            "White",
            "American Indian or Alaskan Native",
            "Other/Undisclosed"
          )
        )
      ) %>%
      arrange(desc(n))
    
    ggplot(race_data, aes(x = reorder(race, -n), y = n, fill = percentage)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = paste0(round(percentage, 1), "%")),
        vjust = -0.4, size = 6.2
      ) +
      scale_fill_gradientn(colours = c("yellow", "forestgreen", "navy")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
      labs(
        x = NULL,
        y = NULL,      # no y-axis label
        title = NULL   # no internal title
      ) +
      theme_minimal(base_size = 20) +
      theme(
        axis.text.x = element_text(size = 16, angle = 20, hjust = 1),
        axis.text.y = element_text(size = 16),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
      )
  })
}

shinyApp(ui = ui, server = server)


