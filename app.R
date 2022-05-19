
#One way ANOVA power calculations

library(shiny)
library(tidyverse)
library(broom)
library(ggplot2)
library(pwr)
library(glue)

cohensfminmax <- function(Delta=1, sd=1, G=3, nmin = 2, nmax = 20){
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
  cohensf <- c(fmin, fmax, 0.1, 0.25, 0.4)
  n <- seq(nmin, nmax, by = 1)
  
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
  power <- seq(powermin, powermax, by = 0.01)
  
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


#changing from function(nmin, nmax, G)
effectsize <- function(nmin, nmax, powermin, powermax, G){
  n <- seq(nmin, nmax, by = 1)
  #changing from c(0.6, 0.7, 0.8, 0.9) 
  power <- c(seq(powermin, powermax, by = 0.05))
  
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
           numericInput("nmin",
                        "Minimum n per group",
                        min = 2,
                        max = 200,
                        value = 3),
           numericInput("nmax",
                        "Maximum n per group",
                        min=3,
                        max=300,
                        value=20)
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
      tabPanel(4
               )
           
           
  )
)



# Define UI for application 
ui <- fluidPage(
  
  # Application title
 
  titlePanel("Power calcs for 1-way ANOVA"),
  withMathJax(),
  
  
  
  # Sidebar with numeric inputs for delta, sd, number of groups
  sidebarLayout(
    sidebarPanel(#fluid = T,
      #width = 3,
      selectInput("select",
                  h3("Select one to calculate:"),
                  choices = list("Power" = 1, 
                                 "Sample size" = 2,
                                 "Effect size" = 3,
                                 "Documentation" = 4),
      ),
      numericInput("G",
                   "Number of groups",
                   min=2,
                   max=15,
                   value=3),
      parameter_tabs,
    ),
    
    
    # Show a plot 
    mainPanel(
     # tabsetPanel(
           #       type = "tabs",
             #     tabPanel("Calculations", plotOutput("powerplot", width = "100%"),
               #            textOutput("text", width = "100%"), dataTableOutput("table", width = "100%")),
               #   tabPanel("About",
                #           width = "100%",
                #           tags$br(),
                #           div(id = "about_p", p("This app calculates power and sample size for a one-sample Z and T-test. 
       # It was designed to give learners an intuitive understanding of these calculations by 
       # visualizing the way changes in parameters yield different power curves and distributions."))),
     # selected = "Calculations")
     # )
    htmlOutput("docs"),
     plotOutput("powerplot"),
      #h5("Table columns are values of Cohen's f. Table cells are values of power."),
     textOutput("text"),
     dataTableOutput("table")
   )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$select, {
    updateTabsetPanel(inputId = "params", selected = input$select)
  }) 
  
  output$docs <- renderUI({
    if(input$select ==1){
      HTML("<b>Power as a function of effect size and sample size.</b>")
    }else if(input$select ==2){
      HTML("<b>Sample size as a function of effect size and power.</b>")
    }else if(input$select ==3){
      HTML("<b>Detectable effect size as a function of power and sample size.</b>")
    }else{
      HTML("<b>This is a test.</b>")
    }
  })
  
  output$powerplot <- renderPlot({
    #   print(input$select)
    if(input$select==1) {
      powerdata <- cohensfminmax(input$Delta, input$sd, input$G,
                                 input$nmin, input$nmax)
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
    }else if(input$select==3){
      powerdata <- effectsize(input$nmin, input$nmax,
                              input$powermin, input$powermax,
                              input$G)
      ggplot(powerdata, aes(x = n, y = f, color = factor(power))) +
        geom_line() +
        theme_minimal() +
        labs(
          x = "Number in each group (n)",
          y = "Detectable Cohen's f"
        )
    }else{
      powerdata <- NULL
    }
  })

  output$text <- renderText({
    if(input$select ==1){
      "Table columns are values of Cohen's f. Table cells are values of power."
    }else if(input$select ==2){
      "Table columns are values of power. Table cells are values of n per group."
    }else if(input$select == 3){
      "Table columns are values of power. Table cells are values of Cohen's f."
    }else{
      ""
    }
  })
  
  output$table <- renderDataTable({
    if(input$select==1) {
      powerdata <- cohensfminmax(input$Delta, input$sd, input$G,
                                 input$nmin, input$nmax) %>% 
        mutate(cohensf = round(cohensf, digits = 2),
               power = round(power, digits = 4)) %>%
        pivot_wider(names_from = "cohensf",
                    values_from = "power") 
    }else if(input$select==2){
      powerdata <- nfrompower(input$powermin, input$powermax,
                              input$Delta, input$sd, input$G) %>%
        mutate(cohensf = round(cohensf, digits = 2),
              n = round(n)) %>%
        pivot_wider(names_from = "power",
                    values_from = "n") 
    }else if(input$select==3){
      powerdata <-effectsize(input$nmin, input$nmax, 
                             input$powermin, input$powermax,
                             input$G) %>%
        mutate(f = round(f, digits = 2)) %>%
        pivot_wider(names_from = "power",
                    values_from = "f") 
    }else{
      powerdata <- NULL
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


