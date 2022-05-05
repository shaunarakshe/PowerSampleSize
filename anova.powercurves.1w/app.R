#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(broom)
library(ggplot2)

cohensfminmax <- function(Delta, sd, G){
  #calculate min cohen's f
  sdm.min <- Delta/sqrt(2*G)
  fmin <- sdm.min/sd
  
  #calculate max cohen's f
  #formula is different for odd vs even numbers of G
  sdm.max <- ifelse(
    #check if even
    (G%%2)==0,
    #formula for even G
    Delta/2,
    #formula for odd G
    (Delta*sqrt(G^2-1))/(2*G)
  )
  fmax <- sdm.max/sd
  
  #Make power curves
  cohensf <- seq(fmin, fmax, by = (fmax-fmin)/5)
  n <- c(seq(2, 10, by = 1), seq(12, 20, by = 2))
  
  powerdata <- crossing(cohensf, n) %>%
    rowwise() %>%
    mutate(power = pwr::pwr.anova.test(
      f=cohensf,
      k=G,
      n = n,
      sig.level = 0.05
    ) %>% tidy() %>% pull(power) )
  
  ggplot(powerdata, aes(x = n, y = power, color = factor(cohensf))) + 
    geom_line() + 
    theme_minimal() + 
    labs(
      x = "Number in each group (n)",
      y = "Power"
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Power curves for 1-way ANOVA based on minimum difference between 2 means"),

    # Sidebar with numeric inputs for delta, sd, number of groups
    sidebarLayout(
        sidebarPanel(
            numericInput("Delta",
                        "Minimum difference in means (delta):",
                        min = 0.01,
                        max = 10,
                        value = 1),
            numericInput("sd",
                         "Standard deviation",
                         min = 0.01,
                         max = 10,
                         value = 1
                          ),
            numericInput("G",
                         "Number of groups",
                         min=2,
                         max=10,
                         value=3)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("powerplot")
        )
    )
)

# Define server logic
server <- function(input, output, session) {

    x1 <- reactive(input$Delta)
    x2 <- reactive(input$sd)
    x3 <- reactive(input$G)
  
    output$powerplot <- renderPlot({
          cohensfminmax(x1(), x2(), x3())
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
