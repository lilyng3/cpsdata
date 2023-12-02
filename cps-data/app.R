
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
         percent_low_income = round((student_count_low_income / student_count_total) * 100, 1)) |>
  # omitted due to NA values for ELA in one year, chose to remove whole school for continuity
  filter(school_name != "U OF C - WOODLAWN HS")

cps_sorted <- cps_sorted |> 
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

cps_asian <- cps_sorted |> 
  filter(primary_race == "asian")

cps_black <- cps_sorted |> 
  filter(primary_race == "black")

cps_hispanic <- cps_sorted |> 
  filter(primary_race == "hispanic")

cps_white <- cps_sorted |> 
  filter(primary_race == "white")


# shiny app ---------------------------------------------------------------

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Chicago Public Schools: Exploring ELA and Math Scores by Majority Racial Makeup of Schools across Time"),
  
  sidebarLayout(
    mainPanel(
      plotOutput("score_plot")
    ),
    
    sidebarPanel(
      helpText("This app takes the English Language Arts (ELA) and Math scores from 3rd - 8th graders in Chicago. Beyond test subject, the data is also examined through majority racial group of each school. The visualization also separates pre and post pandemic years."),

      selectInput("race_selector", "Select Majority Racial Makeup", choices = c("Asian", "Black", "Hispanic", "White"), selected = "Asian"),
      radioButtons("subject_selector", "Select Subject", choices = c("ELA", "Math"), selected = "ELA"),
      
      tags$p("Data Source: City of Chicago")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Function to generate the bar plot based on the selected race and subject
  generate_plot <- reactive({
    race_data <- subset(pandemic_scores_race, primary_race == tolower(input$race_selector))
    
    y_variable <- ifelse(input$subject_selector == "ELA", "avg_met_ela", "avg_met_math")
    y_label <- ifelse(input$subject_selector == "ELA", "Average % Met ELA Standards", "Average % Met Math Standards")
    title <- paste("Percentage of Students In Majority", input$race_selector, "Schools Meeting", input$subject_selector, "Standards Over Time")
    
    custom_colors <- c("#556EE6", "#778BEB", "#F78FB3", "#F8A5C2")  # Specify your custom colors here
    
    ggplot(race_data, aes(x = as.factor(year), y = !!sym(y_variable), fill = as.factor(year))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = title,
           x = "Year",
           y = y_label,
           fill = "Year") +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Output the bar plot
  output$score_plot <- renderPlot({
    generate_plot()
  })
}

# Run the Shiny app
shinyApp(ui, server)