
# One way ANOVA power calculations
# Shauna Rakshe
# BSTA 500: Power Sample Size


library(shiny)
library(tidyverse)
library(broom)
library(ggplot2)
library(pwr)


cohensfminmax <- function(Delta=1, sd=1, G=3, nmin = 2, nmax = 20, ninc = 1){
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
  
  #Make data
  cohensf <- c(fmin, fmax, 0.1, 0.25, 0.4)
  n <- seq(nmin, nmax, by = ninc)
  
  powerdata <- crossing(cohensf, n) %>%
    rowwise() %>%
    mutate(power = pwr::pwr.anova.test(
      f=cohensf,
      k=G,
      n = n,
      sig.level = 0.05
    ) %>% tidy() %>% pull(power) )  
  
}

#to generate table for power calc from n and effect size
powerfromfn <- function(fmin, fmax, G=3, nmin = 2, nmax = 20, ninc = 1){
  
  cohensf <- c(fmin, fmax, 0.1, 0.25, 0.4)
  n <- seq(nmin, nmax, by = ninc)
  
  powerdata <- crossing(cohensf, n) %>%
    rowwise() %>%
    mutate(power = pwr::pwr.anova.test(
      f=cohensf,
      k=G,
      n = n,
      sig.level = 0.05
    ) %>% tidy() %>% pull(power) )  
  
}

#for sample size calculation from power and effect size--cohen's f
nfrompowerf <- function(powermin=0.7, powermax=0.9, powerinc = 0.01, 
                       fmin=0.1, fmax=1.5, G=3){
  
  cohensf <- c(fmin, fmax, 0.1, 0.25, 0.4)
  power <- seq(powermin, powermax, by = powerinc)
  
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

#for sample size calculation from power and effect size
nfrompower <- function(powermin=0.7, powermax=0.9, powerinc = 0.01, 
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
  power <- seq(powermin, powermax, by = powerinc)
  
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


#calculate minimum detectable effect size
effectsize <- function(nmin=3, nmax=20, ninc=1, powermin=0.75, powermax=0.90, powerinc = 0.05, G=3){
  n <- seq(nmin, nmax, by = ninc)
  power <- seq(powermin, powermax, by = powerinc)
  
  powerdata <- crossing(n, power) %>%
    rowwise() %>%
    mutate(f= pwr::pwr.anova.test(
      n = n,
      k=G,
      power = power,
      sig.level = 0.05)[["f"]] )
  
  
}

ui <- fluidPage(
  
  #Title
  titlePanel("Power & Sample Size for 1-way ANOVA"),
  hr(),
  actionButton("calculate", "Calculate", 
               class = "btn-warning"),
  hr(),
  
  #Sidebar panel 
  sidebarPanel(fluid = T,
               selectInput("select",
                           h3("Select option:"),
                           choices = list("Power" = 1, 
                                          "Sample size" = 2,
                                          "Effect size" = 3)),
               numericInput("G",
                            "Number of groups",
                            min=2,
                            max=15,
                            value=3),
               #conditional panel for power calculation
               conditionalPanel(condition="input.select == 1",
                                numericInput("nmin1",
                                             "Minimum n per group (integer)",
                                             min = 2,
                                             max = 200,
                                             value = 3),
                                numericInput("nmax1",
                                             "Maximum n per group (integer)",
                                             min=3,
                                             max=300,
                                             value=20),
                                numericInput("ninc1",
                                             "Increment for n (integer)",
                                             min=1,
                                             max=100,
                                             value=1),
                                
               #select whether to enter delta and sd, or cohen's f
               selectInput("effect_calc",
                           h3("Select input type:"),
                           choices = list("Cohen's f" = 1, 
                                          "Difference in means" = 2)),
                   #conditional panel for cohen's f
                 conditionalPanel(condition="input.effect_calc == 1",
                                  p("Rule of thumb: 0.01 = small effect size,
                                  0.25 = medium effect size, 
                                  0.4 = large effect size."),
                                  numericInput("fmin1",
                                               "Minimum Cohen's f",
                                               min = 0.01,
                                               max = 5,
                                               value = 0.1),
                                  numericInput("fmax1",
                                               "Maximum Cohen's f",
                                               min=0.1,
                                               max=10,
                                               value=0.4)),
               #conditional panel for mean/sd
               conditionalPanel(condition="input.effect_calc == 2",
                                numericInput("Delta1",
                                             "Minimum difference in means (delta):",
                                             min = 0.01,
                                             max = 10,
                                             value = 1),
                                numericInput("sd1",
                                             "Standard deviation",
                                             min = 0.01,
                                             max = 10,
                                             value = 1))
               ),
                                
               #conditional panel for sample size calculation
               conditionalPanel(condition="input.select == 2",
                                numericInput("powermin2",
                                             "Minimum power",
                                             min = 0.1,
                                             max = 1,
                                             value = 0.75),
                                numericInput("powermax2",
                                             "Maximum power",
                                             min=0.1,
                                             max=1,
                                             value=0.9),
                                numericInput("powerinc2",
                                             "Increment for power",
                                             min=0.001,
                                             max=0.1,
                                             value=0.01),
                                #select whether to enter delta and sd, or cohen's f
                                selectInput("effect_calc2",
                                            h3("Select input type:"),
                                            choices = list("Cohen's f" = 1, 
                                                           "Difference in means" = 2)),
                        
                                #conditional panel for cohen's f
                                conditionalPanel(condition="input.effect_calc2 == 1",
                                                 p("Rule of thumb: 0.01 = small effect size,
                                  0.25 = medium effect size, 
                                  0.4 = large effect size."),
                                                 numericInput("fmin2",
                                                              "Minimum Cohen's f",
                                                              min = 0.01,
                                                              max = 5,
                                                              value = 0.1),
                                                 numericInput("fmax2",
                                                              "Maximum Cohen's f",
                                                              min=0.1,
                                                              max=10,
                                                              value=0.4)),
                                #conditional panel for mean/sd
                                conditionalPanel(condition="input.effect_calc2 == 2",
                                                 numericInput("Delta2",
                                                              "Minimum difference in means (delta):",
                                                              min = 0.01,
                                                              max = 10,
                                                              value = 1),
                                                 numericInput("sd2",
                                                              "Standard deviation",
                                                              min = 0.01,
                                                              max = 10,
                                                              value = 1))
               ),
               
               #conditional panel for effect size calculation
               conditionalPanel(condition="input.select == 3",
                                numericInput("powermin3",
                                             "Minimum power",
                                             min = 0.1,
                                             max = 1,
                                             value = 0.75),
                                numericInput("powermax3",
                                             "Maximum power",
                                             min=0.1,
                                             max=1,
                                             value=0.9),
                                numericInput("powerinc3",
                                             "Increment for power",
                                             min=0.001,
                                             max=0.1,
                                             value=0.01),
                                numericInput("nmin3",
                                             "Minimum n per group",
                                             min = 2,
                                             max = 200,
                                             value = 3),
                                numericInput("nmax3",
                                             "Maximum n per group",
                                             min=3,
                                             max=300,
                                             value=20),
               numericInput("ninc3",
                            "Increment for n",
                            min=1,
                            max=100,
                            value=1))
  ),
  mainPanel(
    tabsetPanel(id = "main_tab",
                type = "tabs",
                tabPanel("Results", 
                         p(h3("Graphical output:"),
                           "For power and sample size curves only:",
                           tags$br(),
                           "Solid lines: minimum and maximum values of Cohen's f 
                           derived from user input",
                           tags$br(),
                           "Dotted lines: rule-of-thumb values of Cohen's f:",
                           tags$br(),
                           "0.01 = small effect size,
                            0.25 = medium effect size, 
                            0.4 = large effect size."),
                         plotOutput("powerplot"),
                         p(h3("Datatable output:")),
                         h4(textOutput("text")),
                         downloadButton("downloadData", "Download"),
                         dataTableOutput("table")),
                tabPanel("Documentation",
                         tags$br(),
                         div(id = "about_p", 
                             p("This app calculates power, sample size per group,
                               or minimum detectable effect size for one-way ANOVA models.")),
                         p(h4("Calculations"),
                           "Power/sample size/effect size were calculated using 
                           pwr:: pwr.anova.test().  See the pwr package documentation for details:",
                           tags$a(href="https://cran.r-project.org/web/packages/pwr/pwr.pdf", "pwr documentation"),
                           "The source code for this app can be found here:",
                           tags$a(href="https://github.com/shaunarakshe/PowerSampleSize", "code"),
                          "Minimum and maximum Cohen's f effect sizes were calculated from the difference 
                         in means using the method described in the PASS documentation:",
                         tags$a(href="https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/One-Way_Analysis_of_Variance_Assuming_Equal_Variances-F-Tests.pdf","PASS documentation"),
                         "Note that this procedure assumes equality of variances across groups."
                         ),
                         p(h4("About ANOVA Models"),
                           "ANOVA models allow the comparison of multiple group means at the same time.
                           One-way ANOVA compares means of groups across one categorical variable.
                           There are several assumptions that must be satisfied: the response variable must be
                           continuous; all observations both within and between groups must be independent of each
                           other; the variance of groups must be roughly equal; and observations within each group 
                           must be reasonably normally distributed.  For these sample size calculations, we also
                           assume that the number of observations (n) is the same for each group."),
                         p(h5("Hypotheses:"),
                           "H0: The means of all the groups are equal.",
                           tags$br(),
                           "HA: The mean of at least one group is different from the others.",
                           tags$br(),
                           "alpha = 0.05",
                           tags$br(),
                           "Note that rejecting the null hypothesis only tells us that the mean of at least
                           one group is different.  It does not tell us which means differ.  In order to
                           determine which means differ, we must perform a 2-sample t test on each pair of means.
                           This is often done using a correction factor, such as the Bonferroni correction, in 
                           order to control the overall type I error rate.  This app does not perform
                           power calculations for multiple comparisons."),
                         p(h4("Overview of App Options"),
                           p(h5("Power"),
                             "This option calculates the power of the F test for ANOVA based on sample size,
                             effect size, and number of groups.  Effect size can be input as Cohen's f, or the user
                             can specify the minimum difference between two group means and the overall standard deviation.
                             This information is then used to calculate a minimum and a maximum possible value for 
                             Cohen's f."),
                           p(h5("Sample Size"),
                             "This option calculates the required sample size per group based on selected values of power,
                             effect size, and number of groups.  Effect size can be input as Cohen's f, or the user
                             can specify the minimum difference between two group means and the overall standard deviation.
                             This information is then used to calculate a minimum and a maximum possible value for 
                             Cohen's f."),
                           p(h5("Effect Size"),
                             "This option calculates the minimum detectable effect size based on selected values of power,
                             sample size per group, and number of groups.  Effect size is reported as a value of Cohen's f."),
                           p(h5("Note about Effect Size"), 
                             "The rule of thumb for effect size given by Cohen for ANOVA is: small effect: 0.1, medium effect: 0.25,
                             large effect: 0.4.  However, these effect sizes may not be meaningful in the context of your experiment.
                             We have included the option to calculate minimum and maximum possible values of Cohen's f given a minimum 
                             difference in means between two groups, so that researchers can see how their expected effect sizes compare.")
                           ),
                         p("Created by Shauna Rakshe with guidance from Meike Niederhausen and Jessica Minnier."),
                         p("Please contact rakshe@ohsu.edu with questions or feedback.")
                         ),
                selected = "Results"
    )
    
    
    
  )
  
  
)

server <- function(input, output){
  
  
  #calculate powerdata df based on selected inputs and calcs
  #only calculate when actionbutton gets pushed
      powerdata <- eventReactive(input$calculate,{
        if((input$select==1)&(input$effect_calc==1)){
          
        powerfromfn(input$fmin1, input$fmax1, input$G,
                                 input$nmin1, input$nmax1, input$ninc1)
    }else if((input$select==1)&(input$effect_calc==2)){
        cohensfminmax(input$Delta1, input$sd1, input$G,
             input$nmin1, input$nmax1, input$ninc1)

    }else if((input$select==2)&(input$effect_calc2==1)){
      
    nfrompowerf(input$powermin2, input$powermax2, input$powerinc3,
                               input$fmin2, input$fmax2, input$G)
  
    }else if((input$select==2)&(input$effect_calc2==2)){
      nfrompower(input$powermin2, input$powermax2, input$powerinc2,
                              input$Delta2, input$sd2, input$G)

    }else{
      effectsize(input$nmin3, input$nmax3, input$ninc3,
                              input$powermin3, input$powermax3, input$powerinc3,
                              input$G)}
  })
  
 
  
  
 output$powerplot <- renderPlot({   
   
      
      
      if((input$select==1)&(input$effect_calc==1)){ 
   powerdata() %>%
          ggplot(aes(x = n, y = power, color = factor(round(cohensf,3)),
          linetype=factor(ifelse(cohensf==0.1|cohensf==0.25|cohensf==0.4, 1, 0)))) + 
          geom_line() + 
         theme_minimal() + 
          labs(
          x = "Number in each group (n)",
          y = "Power",
          color = "Cohen's f"
          ) +
          guides(linetype = F)
 
  }else if((input$select==1)&(input$effect_calc==2)){
     ggplot(powerdata(), aes(x = n, y = power, color = factor(round(cohensf,3)),
       linetype=factor(ifelse(cohensf==0.1|cohensf==0.25|cohensf==0.4, 1, 0)))) + 
        geom_line() + 
         theme_minimal() + 
         labs(
          x = "Number in each group (n)",
          y = "Power",
          color = "Cohen's f"
          ) +
         guides(linetype = F)
    }else if((input$select==2)&(input$effect_calc2==1)){
      
    ggplot(powerdata(), aes(x = power, y = n, color = factor(round(cohensf,3)),
       linetype=factor(ifelse(cohensf==0.1|cohensf==0.25|cohensf==0.4, 1, 0)))) +
       geom_line() +
       theme_minimal() +
       labs(
       x = "Power",
       y = "Number in each group (n)",
       color = "Cohen's f"
       ) +
       guides(linetype = F)
    }else if((input$select==2)&(input$effect_calc2==2)){
  
     ggplot(powerdata(), aes(x = power, y = n, color = factor(round(cohensf,3)),
       linetype=factor(ifelse(cohensf==0.1|cohensf==0.25|cohensf==0.4, 1, 0)))) +
        geom_line() +
         theme_minimal() +
        labs(
        x = "Power",
        y = "Number in each group (n)",
         color = "Cohen's f"
         ) +
        guides(linetype = F)
    }else{
      
      ggplot(powerdata(), aes(x = n, y = f, color = factor(power))) +
       geom_line() +
       theme_minimal() +
       labs(
       x = "Number in each group (n)",
       y = "Detectable Cohen's f",
       color = "Power"
       )
    }

  })

  output$text <- renderText({
    if(input$select ==1){
      "Table columns are values of Cohen's f. Table cells are values of power."
    }else if(input$select ==2){
      "Table columns are values of power. Table cells are values of n per group.
      User-input values of effect size shown in bold, rule-of-thumb values in dotted lines."
    }else{
      "Table columns are values of power. Table cells are values of Cohen's f."
    }
  })
  
  output$table <- renderDataTable({
    if((input$select==1)&(input$effect_calc==1)){

      powerdata() %>%  
      mutate(cohensf = round(cohensf, digits = 2),
               power = round(power, digits = 4)) %>%
        pivot_wider(names_from = "cohensf",
                    values_from = "power") 
    }else if((input$select==1)&(input$effect_calc==2)){

      powerdata() %>%  
      mutate(cohensf = round(cohensf, digits = 2),
               power = round(power, digits = 4)) %>%
        pivot_wider(names_from = "cohensf",
                    values_from = "power") 
    }else if((input$select==2)&(input$effect_calc==1)){
 
      powerdata() %>%  
      mutate(cohensf = round(cohensf, digits = 2),
               n = round(n)) %>%
        pivot_wider(names_from = "power",
                    values_from = "n") 
    }else if((input$select==2)&(input$effect_calc==2)){
    
      powerdata() %>%  
      mutate(cohensf = round(cohensf, digits = 2),
               n = round(n)) %>%
        pivot_wider(names_from = "power",
                    values_from = "n") 
    }else{
      
      powerdata() %>%
        mutate(f = round(f, digits = 2)) %>%
        pivot_wider(names_from = "power",
                    values_from = "f") %>%
        rename("n per group" = n)
    }
    
  })
  
  #downloadable csv of table
  output$downloadData <- downloadHandler(
    filename = "powerdata.csv",
    content = function(file){
      write.csv(powerdata(), file, row.names = F)
    }
  )
  
  
}

shinyApp(ui = ui, server = server)