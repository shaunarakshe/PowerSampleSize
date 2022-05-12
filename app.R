
#One way ANOVA power calculations

library(shiny)
library(tidyverse)
library(broom)
library(ggplot2)
library(pwr)

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
} 

effectsize <- function(nmin, nmax, G = G){
  n <- c(nmin, round(0.5*(nmin + nmax)), nmax)
  power <- c(0.6, 0.7, 0.8, 0.9)
  
  powerdata <- crossing(n, power) %>%
    rowwise() %>%
    mutate(f= pwr::pwr.anova.test(
      n = n,
      k=G,
      power = power,
      sig.level = 0.05)[["f"]] )
  
  
}

#set up tabs
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel(1,
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
           
  ),
  tabPanel(3,
           numericInput("nmin",
                        "Minimum n per group",
                        min = 2,
                        max = 200,
                        value = 3),
           numericInput("nmax",
                        "Maximum n per group",
                        min=3,
                        max=300,
                        value=20),
           
  )
)




# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Power calcs for 1-way ANOVA"),
  
  # Sidebar with numeric inputs for delta, sd, number of groups
  sidebarLayout(
    sidebarPanel(#fluid = T,
      #width = 3,
      selectInput("select",
                  h3("Select one to calculate:"),
                  choices = list("Power" = 1, 
                                 "Sample size" = 2,
                                 "Effect size" = 3),
      ),
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
      plotOutput("powerplot"),
      dataTableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$select, {
    updateTabsetPanel(inputId = "params", selected = input$select)
  }) 
  
  
  output$powerplot <- renderPlot({
    #   print(input$select)
    if(input$select==1) {
      powerdata <- cohensfminmax(input$Delta, input$sd, input$G)
      ggplot(powerdata, aes(x = n, y = power, color = factor(cohensf))) + 
        geom_line() + 
        theme_minimal() + 
        labs(
          x = "Number in each group (n)",
          y = "Power"
        )
    }else if(input$select==2){
      powerdata <- nfrompower(input$powermin, input$powermax,
                              input$Delta, input$sd, input$G)
      ggplot(powerdata, aes(x = power, y = n, color = factor(cohensf))) +
        geom_line() +
        theme_minimal() +
        labs(
          x = "Power",
          y = "Number in each group (n)"
        )
    }else{
      powerdata <- effectsize(input$nmin, input$nmax,
                              input$G)
      ggplot(powerdata, aes(x = n, y = f, color = factor(power))) +
        geom_line() +
        theme_minimal() +
        labs(
          x = "Number in each group (n)",
          y = "Detectable Cohen's f"
        )
    }
  })
  
  output$table <- renderDataTable({
    if(input$select==1) {
      powerdata <- cohensfminmax(input$Delta, input$sd, input$G)
    }else if(input$select==2){
      powerdata <- nfrompower(input$powermin, input$powermax,
                              input$Delta, input$sd, input$G)
    }else{
      powerdata <-effectsize(input$nmin, input$nmax, input$G)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


