library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)

mortgage <- function(P = 500000, I = 6, L = 30, monthly_pmt = NA) { 
  
  # Definitions: 
  #   P = principal, the initial amount of the loan
  #   I = annual interest rate
  #   L = length of the loan in years, or at least the length over which the loan is amortized.
  #   J = monthly interest in decimal form = I / (12 x 100)
  #   M = monthly payment; formula: M = P * ( J / (1 - (1 + J) ^ -N))
  #   N = number of months over which loan is amortized = L x 12
  # see also: http://www.jeacle.ie/mortgage/instructions.html
  #
  # Added:
  #   monthly_pmt = a monthly payment you could consistently pay more quickly pay off principal
  #
  
  J <- I / (12 * 100)
  N <- 12 * L
  M <- P * J/(1-(1+J)^(-N))
  monthPay <- M
  
  # Calculate Amortization for each Month
  Pt <- P # current principal or amount of the loan
  currP <- NULL
  
  if (!is.na(monthly_pmt)) {
    M <- monthly_pmt
    monthPay <- M
  }
  while(Pt >= 0) {
    H <- Pt * J # this is the current monthly interest
    C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
    Q <- Pt - C # this is the new balance of your principal of your loan
    Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
    currP <- c(currP, Pt)
  }
  
  monthP <- c(P, currP[1:(length(currP)-1)]) - currP
  aDFmonth <- data.frame(
    Amortization=c(P, currP[1:(length(currP)-1)]), 
    Monthly_Payment=monthP+c((monthPay-monthP)[1:(length(monthP)-1)],0),
    Monthly_Principal=monthP, 
    Monthly_Interest=c((monthPay-monthP)[1:(length(monthP)-1)],0), 
    Year=sort(rep(1:ceiling(N/12), 12))[1:length(monthP)]
  )
  
  aDFyear <- data.frame(
    Amortization=tapply(aDFmonth$Amortization, aDFmonth$Year, max), 
    Annual_Payment=tapply(aDFmonth$Monthly_Payment, aDFmonth$Year, sum), 
    Annual_Principal=tapply(aDFmonth$Monthly_Principal, aDFmonth$Year, sum), 
    Annual_Interest=tapply(aDFmonth$Monthly_Interest, aDFmonth$Year, sum), 
    Year=as.vector(na.omit(unique(aDFmonth$Year)))
  )
  
  return(
    invisible(
      list(
        monthly_payment = monthPay,
        monthly_amort = aDFmonth,
        yearly_amort = aDFyear
      )
    )
  )    
}

# input = list(house_price = NA, down_payment = NA, interest = NA)

shinyServer(
  function(input, output) {
  
  #   if(plotData==T) {
  #     barplot(t(aDFyear[,c(3,4)]), 
  #             col=c("blue", "red"), 
  #             main="Annual Interest and Principal Payments", 
  #             sub="The data for this plot is stored in aDFyear.",
  #             xlab="Years", ylab="$ Amount", 
  #             legend.text=c("Principal", "Interest"), 
  #             ylim=c(0, max(aDFyear$Annual_Payment)*1.3))
  #   }
  #   
  
  # compute our payment schedule
  compute_mortgage <- reactive({
    print (input$monthly_pmt)
    
    loan_amount = input$house_price - input$down_payment
    mort_calcs = mortgage(P = loan_amount, I = input$interest, 
                          L = input$loan_duration, 
                          monthly_pmt = input$monthly_pmt)
    
  })

  # monthly payment
  output$monthly_pmt <- renderText({
    paste0('Monthly payment would be $', 
           round(compute_mortgage()$monthly_payment, digits = 2))
  })
  
  # interst and principal over time
  output$int_vs_prin <- renderPlot({
    month_melt = melt(compute_mortgage()$monthly_amort, id.vars = 'Year')
    month_melt %>%
        filter(variable %in% c("Monthly_Principal", "Monthly_Interest")) %>%
        group_by(variable) %>%
        mutate(x_axis = row_number()) %>%
        ggplot(aes(x = x_axis, y = value, color = variable)) +
          geom_line() + scale_x_continuous(breaks = 0:30 * 12, labels = 0:30) + 
          ylab('Amount ($)') + xlab('Year') + 
          scale_color_brewer(palette = 'Dark2') + ggtitle('Interest Vs Principal')
    })
  
  # cumulative values for these
  output$cum_plot <- renderPlot({
    
    year_vals = compute_mortgage()$yearly_amort
    year_melt = melt(year_vals, id.vars = 'Year')
    
    year_melt %>%
      filter(variable %in% c("Annual_Principal", "Annual_Interest", 
                             'Annual_Payment')) %>%
      group_by(variable) %>%
      mutate(cum_val = cumsum(value)) %>%
      ggplot(aes(x = Year, y = cum_val, color = variable)) +
      geom_line() + scale_x_continuous(breaks = 0:30, labels = 0:30) + 
      scale_y_continuous(breaks = seq(0, ceiling(sum(year_vals$Annual_Payment) 
                                               / 10000) * 10000, 10000), 
                         labels = seq(0, ceiling(sum(year_vals$Annual_Payment) 
                                                 / 10000) * 10000, 10000)) + 
      ylab('Amount ($)') + xlab('Year') + 
      scale_color_brewer(palette = 'Dark2') + 
      ggtitle('Cumulative Interest & Principal') + 
      geom_hline(yintercept = input$house_price * .2)
  })  
  
  }
)
