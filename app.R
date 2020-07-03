library(shiny)
library(tidyverse)
library(RColorBrewer)
library(shinythemes)

# Pull in function to run the attacks
source(file = "attack_function.R", local = TRUE)

monsters <- readRDS(file = "data/open5e_monster_data.rds")[, c("name", "armor_class")]


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  # Theme
  theme = shinytheme("slate"),
  
  # Title
  titlePanel("DnD 5e Barbarian Attack Simulator"),
  
  # Small header
  h5("For simulating damage against a hypothetical opponent"),
  
  # Create a sidebar
  sidebarLayout(
    sidebarPanel(
      
      # Strength
      numericInput(inputId = "STR",
                   label = "Strength modifier",
                   min = -20,
                   max = 100,
                   value = c(3)),
      # Proficiency
      numericInput(inputId = "PROF",
                   label = "Proficiency modifier",
                   min = 1,
                   max = 100,
                   value = c(2)),
      # Number of dice on damage roll
      numericInput(inputId = "numdice",
                   label = "Number of dice per damage roll",
                   min = 1,
                   max = 100,
                   value = c(1)),
      # Threshold to get a critical hit
      numericInput(inputId = "crit_thresh",
                   label = "Minimum attack roll on which you get a critical hit",
                   min = 1,
                   max = 20,
                   value = c(20)),
      # Number of dice on crit roll
      numericInput(inputId = "crit_dice",
                   label = "Total number of dice rolled on a critical hit damage roll",
                   min = 1,
                   max = 100,
                   value = c(2)),
      # Type of dice to roll for damage
      selectInput(inputId = "typedice",
                  label = "Damage roll die",
                  choices = list(
                    "d4" = 4,
                    "d6" = 6,
                    "d8" = 8,
                    "d10" = 10,
                    "d12" = 12),
                  selected = 12,
                  multiple = FALSE),
      # Armor class
      numericInput(inputId = "AC",
                   label = "AC of monster",
                   min = 1,
                   max = 100,
                   value = c(12)),
      # Allow user to pick a monster to generate AC automatically
      selectInput(inputId = "mon_name",
                  label = "Optional: Select a monster to autofill AC",
                  choices = unique(monsters$name),
                  selected = "Kobold",
                  multiple = FALSE),
      # Any attack/damage bonus added by a weapon
      numericInput(inputId = "WEAP",
                   label = "Weapon attack/damage bonus, if any",
                   min = 0,
                   max = 3,
                   value = c(0)),
      # How many attacks the user can take on a turn
      numericInput(inputId = "num_attacks",
                   label = "Number of attacks (1 or 2)",
                   min = 1,
                   max = 2,
                   value = c(1)),
      # How many simulations to run
      numericInput(inputId = "runs",
                   label = "Number of times to simulate",
                   min = 1,
                   max = 1000000,
                   value = c(1000)),
      # Define an action button
      actionButton("go",
                   "Simulate")
      
    ),
    
    mainPanel(
      tabsetPanel(
        
        # Create a tab with a plot as output from this process, called "results"
        tabPanel("Plot", plotOutput("results")),
        # Create a tab with a table as output from this process, called "table"
        tabPanel("Hit stats", tableOutput("table"))
        
      ),
      
      # Statistics about the simulation output
      textOutput("info"),
      br(),
      # Include median damage statement
      textOutput("median_text"),
      # Include mean damage statement
      textOutput("mean_text")
      
    )
    
  ),
  
  hr(),
  # Include Open5e/OGL cite
  print(includeHTML("data/cite.html"))
  
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # React to the 'go' button being pushed by running the simulation
  simulated_vals <- eventReactive(input$go, {
    
    # Run simulation including a progress bar
    withProgress(message = "Running simulation:", value = 0, {
      the_sim <- sim_damage_roll(STR = input$STR,
                                 PROF = input$PROF,
                                 sides = input$typedice,
                                 num_dice = input$numdice,
                                 AC = input$AC,
                                 crit = input$crit_dice,
                                 runs = input$runs,
                                 WEAP = input$WEAP,
                                 num_attacks = input$num_attacks,
                                 crit_thresh = input$crit_thresh)
      
    })
    
  })
  
  # React to 'go' button being pushed by creating text
  info <- eventReactive(input$go, {
    
    print(paste0("The results of simulating an attack aganst a monster of ",
                 input$AC, " AC ", run_num(), " times are below:"))
    
  })
  
  median_text <- eventReactive(input$go, {
    
    print(paste("The median damage is", median(simulated_vals())))
    
  })
  
  mean_text <- eventReactive(input$go, {
    
    print(paste("The mean damage is", round(mean(simulated_vals()), digits = 1)))
    
  })
  
  # React to 'go' button being pushed by updating number of runs for text in app
  run_num <- eventReactive(input$go, {
    
    input$runs
    
  })
  
  # Update the AC when the user selects a monster
  observe({
    
    new_ac <- monsters %>%
      filter(name == input$mon_name) %>%
      pull(armor_class)
    
    updateNumericInput(session = session, inputId = "AC", value = new_ac)
    
  })
  
  # Plot the simulation output
  output$results <- renderPlot({
    
    # Set up data to plot lines for median & mean attack
    plotlines <- data.frame(
      # Lower X & Y bounds
      X = c(median(simulated_vals()), mean(simulated_vals())),
      Y = c(0, 0),
      # Upper X & Y bounds
      Xend = c(median(simulated_vals()), mean(simulated_vals())),
      Yend = c(Inf, Inf),
      # Differentiate lines
      Group = c("Median attack", "Mean attack"))
    
    # Compose plot
    ggplot(data = data.frame(simulated_vals()), aes(simulated_vals())) +
      geom_histogram(bins = max(simulated_vals()), color = "black") +
      geom_segment(data = plotlines,
                   aes(x = X, xend = Xend, y = Y, yend = Yend, color = Group)) +
      scale_color_manual(name = "", values = c("deepskyblue3", "green4"),
                         labels = c("Median attack", "Mean attack")) +
      theme_bw() +
      xlab("Damage amount") +
      ylab("Number of occurrences")
    
  })
  
  # Render the summary table of the simulation results
  output$table <- renderTable({
    
    tbl <- table(simulated_vals())
    tbl <- data.frame(tbl)
    tbl <- mutate(tbl, percentage = 100 * (tbl$Freq / sum(tbl$Freq)))
    names(tbl) <- c("Damage",
                    paste("Freq per", paste(run_num()), sep = " "),
                    "% of attacks")
    
    tbl
    
  })
  
  # Render the informational text
  output$info <- renderText({
    
    info()
    
  })
  
  # Render the median attack text
  output$median_text <- renderText({
    
    median_text()
    
  })
  
  # Render the mean attack text
  output$mean_text <- renderText({
    
    mean_text()
    
  })
  
}


shinyApp(ui = ui, server = server)