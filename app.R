#### Australian 2021 - 2022 personal income tax and HECs/HELP debt calculator 
## https://www.ato.gov.au/rates/individual-income-tax-rates/
## https://www.studyassist.gov.au/paying-back-your-loan/loan-repayment


library(shiny)
library(shinythemes)
library(plotly)

## Dependent functions and calcs here

## Define the tax payable calculation

tax_payable <- function(pi) {
  tax <- (if (pi >= 0 & pi <= 18200) {
    0
  } else if (pi>= 18201 & pi <=45000){
    pi*0.19 
  } else if (pi >= 45001 & pi <= 120000){
    ((pi - 45000)*.325) + 5092 
  } else if (pi >= 120001 & pi <= 180000){
    ((pi - 120000)*.37) + 29467 
  } else if (pi >= 180001){
    ((pi - 180000)*.45) + 51667 
  })
  return(round(tax,0))
}

### Calculate proportion of personal income taxable

tax_payable_proportion <- function(pi) {
  tax <- (if (pi >= 0 & pi <= 18200) {
    0
  } else if (pi>= 18201 & pi <=45000){
    pi*0.19 
  } else if (pi >= 45001 & pi <= 120000){
    ((pi - 45000)*.325) + 5092 
  } else if (pi >= 120001 & pi <= 180000){
    ((pi - 120000)*.37) + 29467 
  } else if (pi >= 180001){
    ((pi - 180000)*.45) + 51667 
  })
  
  tax_proportion <- round ((tax/pi)*100,2)
  return(tax_proportion)
  
}

## Modeling tax payment amount from $45,001 through to $100,000,000 in increments of $10,000

pi <- seq(45001, 100000000, by=10000)
taxframe <- data.frame(personal_income  = pi,
                       tax = NA,
                       proportion = NA)

n.values <- length(pi)
n.values
for (i in 1:n.values){
  #get the personal income amount in row i of the "i" column
  i.current <- taxframe[i,"personal_income"]
  #apply the tax calculation
  i.taxcalculated <- tax_payable(i.current)
  # calculate how much the tax is worth out of total personal income
  i.taxproportion <- tax_payable_proportion(i.current)
  #save the two new columns to the dataf rame
  taxframe[i,"tax"] <- i.taxcalculated
  taxframe[i,"proportion"] <- i.taxproportion
}

## Line chart to model the proportion of tax payable at each $10,000 step of income
fig <- plot_ly(taxframe, 
               x = ~personal_income, 
               y = ~proportion, 
               type = 'scatter', 
               mode = 'lines',
               line = list(color = 'rgb(50, 168, 147)', width = 3))

fig <- fig %>% layout (
  title = "Taxed proportion of personal income",
  xaxis = list(
    rangeslider = list(type = "personal_income") , title = "2021-2022 Personal Income"),
  yaxis = list(title = "Tax payable %"))


### Reactive Shiny app work starts from here
# Define UI

ui <- fluidPage(              
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("

    
      body {

        background-color: white;
        color: black;
      }
      

      "))
  ),
  
  
  navbarPage("Australian Personal Income Tax Explorer 2021-2022",
             
             tabPanel("Tax payable calculator",
                      
                      
                      sidebarPanel(
                        tags$h3("Enter your 2021-2022 personal income"),
                        numericInput("pi", "", "")
                        
                        ,
                        
                        
                      ), # sidebarPanel
                      mainPanel(
                        
                        h4("Your 2021-2022 tax payable:"),
                        verbatimTextOutput("txtout_tax"),
                        h4("Got HECs/HELP debt? Your 2021-2022 tax and study loan payable combined:"),
                        verbatimTextOutput("txtout_tax_hecs"),
                        
                        tags$div(
                          
                          
                          tags$b("Disclaimer"),
                          
                          tags$br(),
                          
                          
                          "This is a simple tax calculator for Australian residents for 
                                      tax purposes that helps you understand the tax owing on your 2021-2022 personal income.",
                          
                          tags$p(),
                          
                          "It has the ", tags$b("added benefit"), "of also calculating any HECs/HELP repayments and combining that amount with
                                      the personal income tax owing to show an ",tags$b("overall liability amount for the financial year."),
                          
                          tags$p(),
                          
                          
                          "This calculator does not cover the Medicare levy, Medicare levy surcharge, working holiday makers' tax obligations
                                      nor the First Home Super Saver (FHSS) scheme.",
                          
                          tags$p(),
                          
                          
                          "Every effort has been made to ensure correct calculations based on 2021-2022 tax and HECs/HELP repayment rates published by the
                                      Australian Tax Office, however this calculator should be used for ",tags$b("estimation and guidance purposes only."),
                          
                          tags$p(),
                          
                          tags$b("The exact amount of your income tax can only be calculated upon lodgement of your income tax return."),
                          
                          tags$p(),
                          
                          "See the below Australian Taxation Office Links for the original taxation and HECs/HELP repayment thresholds and rates used by this calculator.",
                          
                          
                          tags$p(),
                          
                          tags$a(href="https://www.ato.gov.au/rates/individual-income-tax-rates/", "ATO individual income tax rates"),
                          tags$br(),
                          tags$a(href="https://www.studyassist.gov.au/paying-back-your-loan/loan-repayment", "HECs/HELP repayment rates"),
                          tags$br(),
                          tags$a(href="https://github.com/jessicacychew/tax_calc_2021_2022", "Calculator details available on GitHub")
                          
                        )
                        
                      ), # mainPanel
                      
             ), #navbar 1, tabPanel
             
             ## Exploratory line graph
             ## There are three dependencies for 'fig'
             ## 1) Execution of the tax payable function (within same above script)
             ## 2) Execution of the tax proportion function (within same above script)
             ## 3) Execution of the line graph script (line_graph script)
             
             tabPanel("Progressive tax rate modeling",
                      tags$b("Australia's personal income tax system is progressive, with individuals paying a greater proportion of their income as their earnings increase."),
                      
                      tags$br(),
                      
                      "The personal income tax rate caps at just under 45%. The model below demonstrates that from $3M through to $100M of individual earnings taxation rates flatten out at approximately ~44%.",
                      
                      tags$p(),
                      
                      
                      fig ,
                      
                      tags$br(),"Explore the interactive visualisation above by sliding the 2021-2022 personal income scales to better view the tax rate at each increment of income.",
             ))
  
  
) #navbar 2 tabPanel

tax_payable <- function(input,output) {
  #### FIRST OUTPUT WITH TAX CALC ONLY
  output$txtout_tax <- renderText({
    ### Paste statements for symbols and other concatenated messaging
    req(input$pi)
    paste('$',format(round((if (input$pi >= 0 & input$pi <= 18200) {
      0
    } else if (input$pi>= 18201 & input$pi <=45000){
      input$pi*0.19
    } else if (input$pi >= 45001 & input$pi <= 120000){
      ((input$pi - 45000)*.325) + 5092
    } else if (input$pi >= 120001 & input$pi <= 180000){
      ((input$pi - 120000)*.37) + 29467
    } else if (input$pi >= 180001){
      ((input$pi - 180000)*.45) + 51667
    }),0) , nsmall = 0, big.mark = ",")
    , ### Middle of the paste statement ### sprintf function for decimal places
    'or',
    sprintf("%.1f",((if (input$pi >= 0 & input$pi <= 18200) {
      0
    } else if (input$pi>= 18201 & input$pi <=45000){
      input$pi*0.19
    } else if (input$pi >= 45001 & input$pi <= 120000){
      ((input$pi - 45000)*.325) + 5092
    } else if (input$pi >= 120001 & input$pi <= 180000){
      ((input$pi - 120000)*.37) + 29467
    } else if (input$pi >= 180001){
      ((input$pi - 180000)*.45) + 51667
    })/input$pi)*100),'percent of annual personal income')
    
  }) #render end
  
  #### SECOND OUTPUT WITH HECS/HELP CALC BAKED IN
  output$txtout_tax_hecs <- renderText({ ##Calculate base tax rate
    req(input$pi)
    paste('$',format(round((if (input$pi >= 0 & input$pi <= 18200) {
      0
    } else if (input$pi>= 18201 & input$pi <=45000){
      input$pi*0.19
    } else if (input$pi >= 45001 & input$pi <= 120000){
      ((input$pi - 45000)*.325) + 5092
    } else if (input$pi >= 120001 & input$pi <= 180000){
      ((input$pi - 120000)*.37) + 29467
    } else if (input$pi >= 180001){
      ((input$pi - 180000)*.45) + 51667
    }) 
    + ##Calculate HECS/HELP repayment rate and add to base tax amount
      (if (input$pi >= 0 & input$pi <= 47014) {
        0
      } else if (input$pi>= 47015 & input$pi <=54282){
        input$pi*0.01 
      } else if (input$pi >= 54283 & input$pi <= 57538){
        input$pi*0.02
      } else if (input$pi >= 57539 & input$pi <= 60991){
        input$pi*0.025
      } else if (input$pi >= 60992 & input$pi <= 64651){
        input$pi*0.03
      } else if (input$pi >= 64652 & input$pi <= 68529){
        input$pi*0.035
      } else if (input$pi >= 68530 & input$pi <= 72641){
        input$pi*0.04
      } else if (input$pi >= 72642 & input$pi <= 77001){
        input$pi*0.045
      } else if (input$pi >= 77002 & input$pi <= 81620){
        input$pi*0.05  
      } else if (input$pi >= 81621 & input$pi <= 86518){
        input$pi*0.055
      } else if (input$pi >= 86519 & input$pi <= 91709){
        input$pi*0.06
      } else if (input$pi >= 91710 & input$pi <= 97212){
        input$pi*0.065  
      } else if (input$pi >= 97213 & input$pi <= 103045){
        input$pi*0.07
      } else if (input$pi >= 103046 & input$pi <= 109227){
        input$pi*0.075  
      } else if (input$pi >= 109228 & input$pi <= 115781){
        input$pi*0.08
      } else if (input$pi >= 115782 & input$pi <= 122728){
        input$pi*0.085
      } else if (input$pi >= 122729 & input$pi <= 130092){
        input$pi*0.09
      } else if (input$pi >= 130093 & input$pi <= 137897){
        input$pi*0.095
      } else if (input$pi >= 137898) {
        input$pi*0.1
      }),0), nsmall = 0, big.mark = ",")
    
    , ### Middle of the paste statement ### sprintf function for decimal places
    'or',
    #Base tax
    sprintf("%.1f",(((if (input$pi >= 0 & input$pi <= 18200) {
      0
    } else if (input$pi>= 18201 & input$pi <=45000){
      input$pi*0.19
    } else if (input$pi >= 45001 & input$pi <= 120000){
      ((input$pi - 45000)*.325) + 5092
    } else if (input$pi >= 120001 & input$pi <= 180000){
      ((input$pi - 120000)*.37) + 29467
    } else if (input$pi >= 180001){
      ((input$pi - 180000)*.45) + 51667
    })
    
    + #Separate HECS/HELP calc
      (if (input$pi >= 0 & input$pi <= 47014) {
        0
      } else if (input$pi>= 47015 & input$pi <=54282){
        input$pi*0.01 
      } else if (input$pi >= 54283 & input$pi <= 57538){
        input$pi*0.02
      } else if (input$pi >= 57539 & input$pi <= 60991){
        input$pi*0.025
      } else if (input$pi >= 60992 & input$pi <= 64651){
        input$pi*0.03
      } else if (input$pi >= 64652 & input$pi <= 68529){
        input$pi*0.035
      } else if (input$pi >= 68530 & input$pi <= 72641){
        input$pi*0.04
      } else if (input$pi >= 72642 & input$pi <= 77001){
        input$pi*0.045
      } else if (input$pi >= 77002 & input$pi <= 81620){
        input$pi*0.05  
      } else if (input$pi >= 81621 & input$pi <= 86518){
        input$pi*0.055
      } else if (input$pi >= 86519 & input$pi <= 91709){
        input$pi*0.06
      } else if (input$pi >= 91710 & input$pi <= 97212){
        input$pi*0.065  
      } else if (input$pi >= 97213 & input$pi <= 103045){
        input$pi*0.07
      } else if (input$pi >= 103046 & input$pi <= 109227){
        input$pi*0.075  
      } else if (input$pi >= 109228 & input$pi <= 115781){
        input$pi*0.08
      } else if (input$pi >= 115782 & input$pi <= 122728){
        input$pi*0.085
      } else if (input$pi >= 122729 & input$pi <= 130092){
        input$pi*0.09
      } else if (input$pi >= 130093 & input$pi <= 137897){
        input$pi*0.095
      } else if (input$pi >= 137898) {
        input$pi*0.1
      }))      
    # Tax + HECs/HELP together, then divided by personal income
    /input$pi)*100),'percent of annual personal income')    
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = tax_payable)

## Future improvements - add more colour and style to the page
## https://stackoverflow.com/questions/50862653/customise-title-bar-in-shiny & https://getbootstrap.com/docs/4.0/components/navbar/












