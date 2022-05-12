#One way ANOVA power calculations

library(shiny)
library(tidyverse)
library(broom)
library(ggplot2)

cohensfminmax <- function(Delta=1, sd=1, G=3){
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

nfrompower <- function(powermin=0.7, powermax=0.9, 
                       Delta=1, sd=1, G=3){
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
  cohensf <- c(fmin, fmax, 0.1, 0.25, 0.4)
  #n <- c(seq(2, 10, by = 1), seq(12, 20, by = 2))
  power <- c(powermin, powermax, 0.5*(powermin+powermax))
  
  powerdata <- crossing(cohensf, power) %>%
    rowwise() %>%
    mutate(n= pwr::pwr.anova.test(
      f=cohensf,
      k=G,
      power = power,
      sig.level = 0.05)
      %>% 
        tidy() %>% 
        pull(n) )
  
  ggplot(powerdata, aes(x = power, y = n, color = factor(cohensf))) +
  geom_line() +
   theme_minimal() +
   labs(
    x = "Power",
     y = "Number in each group (n)"
   )
} 

#set up tabs
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel(1
  ),
  tabPanel(2,
           numericInput("powermin",
                        "Minimum power",
                        min = 0.1,
                        max = 1,
                        value = 0.75),
           numericInput("powermax",
                        "Maximum power",
                        min=0.1,
                        max=1,
                        value=0.9),

  )
)



# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Power curves for 1-way ANOVA based on minimum difference between 2 means"),

    # Sidebar with numeric inputs for delta, sd, number of groups
    sidebarLayout(
        sidebarPanel(#fluid = T,
                     #width = 3,
            selectInput("select",
                        h3("Select one to calculate:"),
                       choices = list("Power" = 1, 
                                      "Sample size" = 2),
                       ),
            numericInput("Delta",
                         "Minimum difference in means (delta):",
                         min = 0.01,
                         max = 10,
                         value = 1),
            numericInput("sd",
                         "Standard deviation",
                         min = 0.01,
                         max = 10,
                         value = 1),
            numericInput("G",
                         "Number of groups",
                         min=2,
                         max=10,
                         value=3),
            parameter_tabs,
        ),
            

        # Show a plot 
        mainPanel(
          #tabsetPanel(id = "main_tab",
           #           type = "tabs",
           #           tabPanel("Power visualization"), plotOutput("powerplot"),
           #           tabPanel("Sample size vs power", plotOutput("ncurve")),
          #selected = "Power visualization"
          plotOutput("powerplot")
        )
    )
)


# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$select, {
    updateTabsetPanel(inputId = "params", selected = input$select)
  }) 
  
 # x1 <- reactive(input$Delta)
 # x2 <- reactive(input$sd)
 # x3 <- reactive(input$G)

  
 output$powerplot <- renderPlot({
   print(input$select)
   if(input$select==1) {cohensfminmax(input$Delta, input$sd, input$G)
   }else{nfrompower(input$powermin, input$powermax,
                                input$Delta, input$sd, input$G)}
 })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
