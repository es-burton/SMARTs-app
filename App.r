# Packages
library(shiny)
library(shinyjs)
library(bslib)
library(sortable)
library(tidyverse)

# Functions

## For Rank List
Objectives <- list(
  "Minimize tick populations at home",
  "Minimize tick bites acquired at home",
  "Minimize harm to the environment",
  "Minimize cost (USD)",
  "Minimize time spent"
)

rank_list_basic <- rank_list(
  text = HTML("To rank the objectives, envision all objectives performing at their worst and then decide for which objective a swing from worst to best (with the other objectives remaining worst) would be most important, second most important, and least important."),
  labels = Objectives,
  input_id = "rank_list_basic"
)

## Build DF for Objective Weights

build_weight_df <- function(Ranks, Weights) {
  weight_df <- data.frame(
    Rank = c(1:5),
    Objective = c(Ranks),
    Weight = c(Weights)
  )
}

## GGplot of Objective Weights
visualize <- function(Ranks, Weights) {
  df <- data.frame(
    Rank = c(1:5),
    Objective = c(Ranks),
    Weight = c(Weights)
  )
  
  df %>%
    mutate(Objective = fct_reorder(as.factor(Objective), -Rank)) %>%
    ggplot(aes(x = Weight, y=Objective)) +
    geom_col(fill = "mediumturquoise") +
    xlab("Weight") +
    ylab("") +
    theme_bw(base_size = 18) +
    theme()
}

## GGplot of Results
### Import Data
ee_norm <- read_csv("ExpertMeansNorm.csv")

ee_norm$Objective <- as.factor(ee_norm$Objective)

### Divide into consequence tables for mean, min, and max estimates
con_mean <- ee_norm %>%
  select(Objective, Alternative, Mean_n) %>%
  pivot_wider(names_from = "Alternative", values_from = "Mean_n")

con_min <- ee_norm %>%
  select(Objective, Alternative, Min_n) %>%
  pivot_wider(names_from = "Alternative", values_from = "Min_n")

con_max <- ee_norm %>%
  select(Objective, Alternative, Max_n) %>%
  pivot_wider(names_from = "Alternative", values_from = "Max_n")

### Function to multiply by weights and build graph
results <- function(Ranks, Weights) {
  weights <- data.frame(
    Rank = c(1:5),
    Objective = as.factor(c(Ranks)),
    Weight = c(Weights)
  ) 
  
  weights <- weights %>%
    mutate(Objective = recode(
      as.factor(Objective),
      "Minimize tick populations at my home" = "1",
      "Minimize my risk of tick bites at my home" = "2",
      "Minimize harm to the environment" = "3",
      "Minimize cost (USD)" = "4",
      "Minimize time spent" = "5"))
    
  #scale weights to add to 1
  weights$Weight_s <- weights$Weight/sum(weights$Weight)
  
  #Multiply consequence tables by weights
  #Mean
  con_mean_w <- con_mean %>%
    #join dfs
    left_join(select(weights, Objective, Weight_s), by = "Objective") %>%
    #multiple alternative values by weights
    mutate(across(c(2:21), ~ .x*weights$Weight_s)) %>%
    #pivot longer (each row is an alternative, columns are objectives) and create new column summing weighted values for each alternative
    select(!Weight_s) %>%
    pivot_longer(cols = c(2:21),
                 names_to = "Alternative", values_to = "value") %>%
    group_by(Alternative) %>%
    summarize(Mean = sum(value))
  
  
  #Min
  con_min_w <- con_min %>%
    #join dfs
    left_join(select(weights, Objective, Weight_s), by = "Objective") %>%
    #multiple alternative values by weights
    mutate(across(c(2:21), ~ .x*weights$Weight_s)) %>%
    #pivot longer (each row is an alternative, columns are objectives) and create new column summing weighted values for each alternative
    select(!Weight_s) %>%
    pivot_longer(cols = c(2:21),
                 names_to = "Alternative", values_to = "value") %>%
    group_by(Alternative) %>%
    summarize(Min = sum(value))
  
  #Max
  con_max_w <- con_max %>%
    #join dfs
    left_join(select(weights, Objective, Weight_s), by = "Objective") %>%
    #multiple alternative values by weights
    mutate(across(c(2:21), ~ .x*weights$Weight_s)) %>%
    #pivot longer (each row is an alternative, columns are objectives) and create new column summing weighted values for each alternative
    select(!Weight_s) %>%
    pivot_longer(cols = c(2:21),
                 names_to = "Alternative", values_to = "value") %>%
    group_by(Alternative) %>%
    summarize(Max = sum(value))
  #Combine DFs
  con_w <- list(con_mean_w, con_min_w, con_max_w) %>% 
    reduce(left_join, by = "Alternative") %>%
    arrange(Mean) %>%    
    mutate(Alternative=factor(Alternative, levels=Alternative)) %>%
    pivot_longer(cols=c(2:4), names_to = "Estimate", values_to = "Value")
  
  con_w$Estimate <- factor(con_w$Estimate, levels = c("Mean", "Min", "Max"))
  
  #Build Graph
  est.labs <- c("Experts Estimate of Mean", "Experts Estimate of Minimum", "Experts Estimate of Maximum")
  names(est.labs) <- c("Mean", "Min", "Max")
  
  max_val <- max(con_w$Value)
  
  con_w %>%
    filter(Estimate == "Mean") %>%
    mutate(
      Alternative = recode(
        as.factor(Alternative),
        "1" = "Do nothing",
        "2" = "Mow often",
        "3" = "Maintain edge understory",
        "4" = "Mulch / gravel barrier",
        "5" = "Remove leaf litter",
        "6" = "Move high use areas \n from lawn edge",
        "7" = "Synthetic sprays",
        "8" = "Natural sprays",
        "9" = "Granular synthetic acaricide",
        "10" = "Bait boxes",
        "11" = "Tick tubes",
        "12" = "Remove wildlife feeders",
        "13" = "Deer fencing",
        "14" = "Synthetic personal protective spray/lotion",
        "15" = "Natural personal protective spray/lotion",
        "16" = "Treated clothing",
        "17" = "Tick checks",
        "18" = "Shower",
        "19" = "Laundry",
        "20" = "Treat pets"
      )
    ) %>%
    ggplot() +
    geom_point(aes(x = Value, y = Alternative)) +
    geom_segment(aes(x = 0, xend = Value, y = Alternative)) +
    # facet_grid(~Estimate, labeller=labeller(Estimate = est.labs)) +
    theme_bw(base_size = 18) +
    scale_x_continuous(limits = c(0, max_val),
                       breaks = c(0, max_val), 
                       labels = c("Worst\nPerforming", "Best\nPerforming")) +
    xlab("Performance of Method") +
    theme(legend.position="none",
          axis.title.y = element_blank(),
          panel.spacing.x = unit(0.04, "npc", data = NULL),
          plot.margin = margin(0, 0.02, 0, 0.02, "npc"))
}
  
  
# User Interface
ui <- page_fixed(
  shinyjs::useShinyjs(),
  theme = bslib::bs_theme(preset = "zephyr"),
  headerPanel("Tick Control Decision Support"),
  #description of app
  p(HTML("")),
  tabsetPanel(
    id = "wizard",
    type = "hidden",
    tabPanel("page_1",
             card(
               align = "center",
               p(HTML("<b>Drag the objectives from most to least important to you.</b>")),
               rank_list_basic),
             layout_columns(
               actionButton("page_12", "Next")
             )),
    tabPanel("page_2",
             card(
               height = "650px",
               p(HTML("<b>Add weights to each objective, reflecting their importance to you.</b>")),
               p(HTML("Use the sliders on the left to rank how important each objective is, relative to the others. The objective you ranked first in the previous page is automatically set to 100 (highest value possible). The weight of an objective cannot exceed the weight of a higher ranked objective. For best results, use the full range of values (0-100) to better differentiate between objectives.")),
               layout_columns(
                 col_widths = c(9, 3),
                 div(plotOutput("viz_weight")
                 ),
                 div(sliderInput("R1", label = "", min = 0, max = 100, value = 100, step = 100, ticks = FALSE),
                     sliderInput("R2", label = "", min = 0, max = 100, value = 0, step = 1, ticks = FALSE),
                     sliderInput("R3", label = "", min = 0, max = 0, value = 0, step = 1, ticks = FALSE),
                     sliderInput("R4", label = "", min = 0, max = 0, value = 0, step = 1, ticks = FALSE),
                     sliderInput("R5", label = "", min = 0, max = 0, value = 0, step = 1, ticks = FALSE)
                 )
               )
             ),
             layout_columns(actionButton("page_21", "Previous"),
                            actionButton("page_23", "See Results"))
    ),
    tabPanel("page_3",
             card(
               height = "650px",
               card_header("Results"),
               layout_columns(
                 div(plotOutput("viz_results")
                 )
               )
             ),
             layout_columns(actionButton("page_32", "Previous"))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  ## Switch Pages
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }
  
  observeEvent(input$page_12, switch_page(2))
  observeEvent(input$page_21, switch_page(1))
  observeEvent(input$page_23, switch_page(3))
  observeEvent(input$page_32, switch_page(2))
  
  # Disable slider for R1
  observe({shinyjs::disable("R1")})
  
  # Create Rank List
  Ranks <- reactive(c(input$rank_list_basic))
  
  # Create Weights List
  Weights <- reactive(c(input$R1, input$R2, input$R3, input$R4, input$R5))
  
  # Generate graph of weights
  output$viz_weight <- renderPlot({
    visualize(Ranks(), Weights())
  }, height = 475)
  
  # Build Weight DF
  weight_df <- reactive({
    build_weight_df(Ranks(), Weights())
  })
  
  # Generate table
  output$table <- renderTable(weight_df())
  
  # Update Slider Maximums
  observeEvent(input$R2, {
    updateSliderInput(inputId = "R3", max = input$R2)
  })
  
  observeEvent(input$R3, {
    updateSliderInput(inputId = "R4", max = input$R3)
  })
  
  observeEvent(input$R4, {
    updateSliderInput(inputId = "R5", max = input$R4)
  })
  
  # Visualize Results
  output$viz_results <- renderPlot({
    results(Ranks(), Weights())
  }, height = 575)
}

# Run App
shinyApp(ui = ui, server = server)
