# read packages and data --------------------------------------------------
library(shiny)
library(tidyverse)
library(ggthemes)

cps <- read_rds("data/cps_data.rds")

# data manipulation -------------------------------------------------------
# by race
racial_groups <- c("student_count_black", "student_count_hispanic", "student_count_white", "student_count_asian", "student_count_native_american", "student_count_other_ethnicity","student_count_asian_pacific_islander", "student_count_multi", "student_count_hawaiian_pacific_islander", "student_count_ethnicity_not_available")

# by ela score
ela_score <- c("percent_did_not_meet_ela", "percent_partially_met_ela", "percent_approached_ela", "percent_met_ela", "percent_exceeded_ela", "percent_met_or_exceeded_ela")

# by math score
math_score <- c("percent_did_not_meet_math", "percent_partially_met_math", "percent_approached_math", "percent_met_math", "percent_exceeded_math", "percent_met_or_exceeded_math")

cps_sorted <- cps |> 
  mutate(
    primary_race = apply(cps[, racial_groups], 1, function(row) {
      names(row)[which.max(row)]}),
    primary_ela = names(cps[, ela_score])[max.col(cps[, ela_score], "last")],
    primary_math = names(cps[, math_score])[max.col(cps[, math_score], "last")]
  ) |> 
  mutate(primary_race = sub("student_count_", "", primary_race),
         percent_low_income = round((student_count_low_income / student_count_total) * 100, 1),
         title_one = percent_low_income >= 40) |>
  filter(school_name != "U OF C - WOODLAWN HS") |> 
  mutate(
    primary_race = factor(primary_race),
    primary_ela = factor(primary_ela),
    primary_math = factor(primary_math)
  )

pandemic_scores_race <- cps_sorted |> 
  group_by(year, primary_race) |> 
  summarise(
    avg_met_ela = mean(percent_met_ela, na.rm = TRUE),
    avg_met_math = mean(percent_met_math, na.rm = TRUE)
  )

pandemic_scores_race_lowincome <- cps_sorted |>
  filter(title_one == TRUE) |>
  group_by(year, primary_race) |>
  summarise(
    avg_met_ela = mean(percent_met_ela, na.rm = TRUE),
    avg_met_math = mean(percent_met_math, na.rm = TRUE)
  )

# shiny app ---------------------------------------------------------------

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Chicago Public Schools: Exploring ELA and Math Scores by Majority Racial Makeup of Schools across Time"),
  
  sidebarLayout(
    mainPanel(
      plotOutput("score_plot"),
      plotOutput("low_income_plot")
    ),
    
    sidebarPanel(
      helpText("This app takes the English Language Arts (ELA) and Math scores from 3rd - 8th graders in Chicago. Beyond test subject, the data is also examined through the majority racial group of each school. The visualization also separates pre and post-pandemic years."),
      
      selectInput("race_selector", "Select Majority Racial Makeup", choices = c("Asian", "Black", "Hispanic", "White"), selected = "Asian"),
      radioButtons("subject_selector", "Select Subject", choices = c("ELA", "Math"), selected = "ELA"),
      
      checkboxInput("low_income_selector", "Include Low Income Data", value = FALSE),
      checkboxInput("all_data_selector", "Include All Data", value = FALSE),
      
      tags$p("Data Source: City of Chicago")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # function to generate the line plot based on the selected race and subject
  generate_plot <- reactive({
    if (input$all_data_selector) {
      # Use all data
      data <- pandemic_scores_race
      low_income_data <- pandemic_scores_race_lowincome
    } else {
      # Use selected race data
      data <- subset(pandemic_scores_race, primary_race == tolower(input$race_selector))
      low_income_data <- subset(pandemic_scores_race_lowincome, primary_race == tolower(input$race_selector))
    }
    
    y_variable <- ifelse(input$subject_selector == "ELA", "avg_met_ela", "avg_met_math")
    y_label <- ifelse(input$subject_selector == "ELA", "Average % Met ELA Standards", "Average % Met Math Standards")
    title <- paste("Percentage of Students In Majority", input$race_selector, "Schools Meeting", input$subject_selector, "Standards Over Time")
    
    plot <- ggplot(data, aes(x = as.factor(year), y = !!sym(y_variable), color = primary_race, group = primary_race)) +
      geom_point() +
      geom_line() +
      labs(title = title,
           x = "Year",
           y = y_label,
           color = "Primary Race") +
      theme_minimal() +
      ylim(0, 50) +
      theme(legend.position = "bottom")
    
    if (input$low_income_selector) {
      plot <- plot +
        geom_point(data = low_income_data, aes(x = as.factor(year), y = !!sym(y_variable), color = primary_race), shape = 1) +
        geom_line(data = low_income_data, aes(x = as.factor(year), y = !!sym(y_variable), group = primary_race), linetype = "dashed")
    }
    
    return(plot)
  })
  
  # line plot for average of all scores
  output$score_plot <- renderPlot({
    generate_plot()
  })
  
  # ...
  
}

# Run the Shiny app
shinyApp(ui, server)