library(shiny)
library(tidyverse)

bad_drivers = read_csv(file = "data/bd.csv")

ui = navbarPage(

  title = "U.S. Fatal Car Crashes",
  tabPanel(
    title = "Visualization",
    titlePanel(title = "U.S. Car Crash Data by State"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "metric",
                    label = "Metric:",
                    choices = names(bad_drivers)[2:6],
                    selected = names(bad_drivers)[2]
                    ),
        selectInput(inputId = "comp",
                    label = "Filter:",
                    choices = c("Metric Only" = FALSE, "Metric vs. Total" = TRUE),
                    selected = "Metric Only"),
        checkboxInput(inputId = "sort", 
                      label = "Sort Decreasing by # of Fatalities (Graphic and Data Table)",
                      value = FALSE)
        ),
      mainPanel(
        plotOutput("plot")
      )
    )
    
  ),
  tabPanel(
    title = "Data Table",
    dataTableOutput("table")
  ),
  
  tabPanel(title = "About", includeMarkdown("about.Rmd"))
)


server = function(input, output) {
  
  crash_metric = reactive({
    bad_drivers %>%
      select(State, input$metric) %>%
      arrange(State)
  })
  
  observeEvent(
    eventExpr = input$metric,
    handlerExpr = {
      if (input$metric == "Fatalities") {
        updateSelectInput(inputId = "comp", 
                          choices = c("Metric Only" = FALSE))
      } else {
        updateSelectInput(inputId = "comp",
                          choices = c("Metric Only" = FALSE, "Metric vs. Total" = TRUE),
                          selected = input$comp)
      }
    }
  )
  
  output$plot = renderPlot({
    
    # State-to-State AND self-comparison
    if (input$comp) {
      if (input$sort) {
        c2 = col2(bad_drivers, input$metric)
        bad_drivers %>%
          mutate(sub = (c2 / 100.0) * bad_drivers$Fatalities) %>%
          arrange(sub) %>%
          ggplot() +
          geom_col(aes(x = fct_inorder(State), y = Fatalities, fill = "Total"), alpha = 0.3) +
          geom_col(aes(x = fct_inorder(State), y = sub, fill = input$metric), alpha = 0.6) +
          labs(x = "State", 
               y = comp_label(input$metric),
               fill = "Fatalities") +
          coord_flip()
      } else {
        bad_drivers %>%
          mutate(sub = (col2(bad_drivers, input$metric) / 100.0) * bad_drivers$Fatalities) %>%
          ggplot() +
          geom_col(aes(x = fct_inorder(State), y = Fatalities, fill = "Total"), alpha = 0.3) +
          geom_col(aes(x = fct_inorder(State), y = sub, fill = input$metric), alpha = 0.6) +
          labs(x = "State", 
               y = comp_label(input$metric),
               fill = "Fatalities") +
          coord_flip()
      }
    } 
    
    # State-to-State Comparison Only
    else {
      c2 = col2(bad_drivers, input$metric)
      if (input$sort) {
        bad_drivers %>%
          mutate(c2 = col2(bad_drivers, input$metric)) %>%
          arrange(c2) %>%
          ggplot() +
          aes(x = fct_inorder(State), 
              y = c2, 
              fill = c2) %>%
          geom_col() +
          labs(x = "State", 
               y = ylabel(input$metric),
               fill = input$metric) +
          coord_flip()
      } else {
        bad_drivers %>%
          ggplot() +
          aes(x = fct_inorder(State), 
              y = c2, 
              fill = c2) %>%
          geom_col() +
          labs(x = "State", 
               y = ylabel(input$metric),
               fill = input$metric) +
          coord_flip()
      }
    }
  })
  
  output$table = renderDataTable({
    tab = crash_metric()
    
    if (input$comp) {
      tf = bad_drivers %>%
        arrange(State) %>%
        select(Fatalities)
      
      tab = tab %>%
        mutate(Total.Fatalities = tf) %>%
        mutate(Metric.Fatalities = round((tab[,2] / 100.0) * tf, digits = 1)) 
      
      if (input$sort) {
        tab = tab %>%
          arrange(desc(Metric.Fatalities))
      }
      
    } else if (input$sort) {
      tab = tab %>%
        arrange(desc(tab[,2]))
    }
    
    tab
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
