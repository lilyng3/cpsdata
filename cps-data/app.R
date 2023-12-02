
library(shiny)
library(tidyverse)
library(ggthemes)

# read data
cps <- read_rds("data/cps_data.rds")

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

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Percentage of Students Meeting ELA Levels By Race Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("race_selector", "Select Race", choices = unique(pandemic_scores_race$primary_race))
    ),
    
    mainPanel(
      plotOutput("ela_plot")
    )
  )
)

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Percentage of Students Meeting ELA Levels By Race Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("race_selector", "Select Race", choices = unique(pandemic_scores_race$primary_race))
    ),
    
    mainPanel(
      plotOutput("ela_plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Function to generate the bar plot based on the selected race
  generate_plot <- reactive({
    race_data <- subset(pandemic_scores_race, primary_race == input$race_selector)
    
    ggplot(race_data, aes(x = as.factor(year), y = avg_met_ela, fill = as.factor(year))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Percentage of Students Meeting ELA Levels Over Time",
           x = "Year",
           y = "Average % Met ELA Levels",
           fill = "Year") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Output the bar plot
  output$ela_plot <- renderPlot({
    generate_plot()
  })
}

# Run the Shiny app
shinyApp(ui, server)