library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Mortgage Simulator"),
    sidebarPanel(
      numericInput(inputId = 'house_price', label = "Price of House:", 
                   value = 180000, step = 5000),
      numericInput(inputId = 'down_payment', label = "Down Payment:", 
                   value = 15000, step = 500),
      numericInput(inputId = "interest", label = "Interest Rate:", 
                   min = 2, max = 5, value = 3.875, step = .005),
      numericInput(inputId = "loan_duration", label = "Loan Duration:", 
                   value = 30),
      checkboxInput(inputId = "is_paying_more", 
                    label = "Make bigger monthly payment?", value = FALSE),
      numericInput(inputId = "monthly_pmt", label = "Size of monthly payment", 
                   value = NULL, step = 50)
    ),
    mainPanel(
      h2(textOutput('monthly_pmt')),
      plotOutput('int_vs_prin', width = 800, height = 400),
      plotOutput('cum_plot', width = 800, height = 600)      
    )
  )
)
