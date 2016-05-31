library(shiny)
library(ggplot2)
library(reshape2)
library(stockPortfolio)


ui <- fluidPage(
  textInput(inputId = "stock_init_price", label = "Stock Price at time t=0", value = "100", placeholder = "e.g. 100.00")
  , textInput(inputId = "stock_return", label = "Stock Annual Return", value = "0.05", placeholder = "e.g. 0.05")
  , textInput(inputId = "stock_volatility", label = "Stock Annual Volatility", value = "0.1", placeholder = "e.g. 0.10")
  , textInput(inputId = "sim_yr", label = "Number of Years to Simulate", value = "2", placeholder = "Positive integer or fraction, e.g. 2.5")
  , textInput(inputId = "sim_n_per_yr", label = "Number of Time Points per Year", value = "200", placeholder = "Integer, e.g. 200")
  , textInput(inputId = "sim_seed", label = "Random Seed", value = "1234", placeholder = "Integer, e.g. 12345")
  , textInput(inputId = "sim_num", label = "Number of Simulations", value = "1000", placeholder = "Integer, e.g. 10000")
  , textInput(inputId = "sim_n_disp", label = "Number of Simulations to display", value = "100", placeholder = "Integer, e.g. 100")
  , actionButton(inputId = "sim_run", label = "Run Simulation")
  , plotOutput(outputId = "path_plot")
)

server <- function(input, output){
  sim <- eventReactive(input$sim_run, {
    list('n_yr' = as.numeric(input$sim_yr)
         , 'delta_t' = 1 / as.numeric(input$sim_n_per_yr)
         , 'seed' = as.numeric(input$sim_seed)
         , 'nsim' = as.numeric(input$sim_num)
         , 'iters' = ceiling(as.numeric(input$sim_yr) * as.numeric(input$sim_n_per_yr))
         , 'n_display' = as.numeric(input$sim_n_disp)
         )
  })
  stock <- eventReactive(input$sim_run, {
    list('init_price' = as.numeric(input$stock_init_price)
         , 'return' = as.numeric(input$stock_return)
         , 'volatility' = as.numeric(input$stock_volatility))
  })
  
  sim_one <- function(sim_spec){
    init_price <- sim_spec$init_price
    return <- sim_spec$return
    volatility <- sim_spec$volatility
    delta_t <- sim_spec$delta_t
    iters <- sim_spec$iters
    
    prices <- c(init_price)
    for(i in 2:(iters+1)){
      prices[i] <- prices[i-1] * exp((return - (volatility)^2 / 2) * delta_t + volatility * sqrt(delta_t) * rnorm(1))
    }
    return(prices)
  }
  
  simulated_prices <- function(init_price, return, volatility, n_yr, delta_t, iters, nsim, seed){
    
    set.seed(seed)
    
    prices <- data.frame(
      sapply(lapply(
        rep(list(list("init_price"=init_price, "return"=return,
                   "volatility"=volatility, "delta_t"=delta_t,
                   "iters"=iters)), nsim), 
        sim_one), unlist), 
      row.names=delta_t * (0:iters))
    
    prices$time <- as.numeric(row.names(prices))
    return(prices)
    
  }
  
  result_prices <- reactive(
    do.call(simulated_prices, list("init_price"=stock()$init_price, "return"=stock()$return, 
                                   "volatility"=stock()$volatility, "n_yr"=sim()$n_yr, 
                                   "delta_t"=sim()$delta_t, "iters"=sim()$iters, 
                                   "nsim"=sim()$nsim, "seed"=sim()$seed))
    )
  
  plot_data <- reactive(do.call(melt, list("data"=result_prices()[,c(1:sim()$n_display, sim()$nsim+1)], "id"="time")))
  
  
  output$path_plot <- renderPlot({
    plot_object <- ggplot(data=plot_data(), aes(x=time, y=value)) +
      geom_line(aes(group=variable), color="black", size=0.1)
    print(plot_object)
  })
}

shinyApp(ui = ui, server = server)