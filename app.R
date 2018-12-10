# By Anthony Layton (Tony Layton)
#

library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(readr)


# Load data
load("data/available_drinks.Rda")
load("data/recommendations.Rda")
load("data/cust_names.Rda")


# Define UI
ui <- fluidPage(theme = shinytheme("slate"),
                titlePanel("Dram Shop - Customer Recommendation Engine"),
                h4("by: Anthony 'Tony' Layton"),
                sidebarLayout(
                    sidebarPanel(
                        
                        # Select Customer
                        selectizeInput(inputId = "Customer", label = h3("Customer Name"), 
                                       choices = cust_names, selected = "John_Chandler", 
                                       multiple = TRUE),
                        
                        # Select to display only available drinks, or all drinks.
                        radioButtons(inputId = "limit", label = h4("Show Me:"),
                                     choices = list("Limit Recommendations to Available Drinks" = "limited", "All Drink Recommendations" = "all"),
                                     selected = "limited"),
                        
                        # Select Number of Recommendations to display.
                        sliderInput(inputId = "nRecommend",
                                    label = h4("Number of Recommendations:"),
                                    min = 1,
                                    max = 10,
                                    value = 5)
                        ),
                    mainPanel(
                        h3("Recommended Drinks"),
                        tableOutput('table')
                    )
                )
)

# Define server function
server <- function(input, output) {
    
    # Subset data
    select_drink <- reactive({
        if (input$limit == "limited") {
            format(sort(recommendations[input$Customer, available_drinks],decreasing = T), digits = 2)
            #sort(select_customer()[, available_drinks], decreasing = TRUE)
        } else if (input$limit == "all") {
            format(sort(recommendations[input$Customer,], decreasing = T), digits = 2)
        } else
            print("Error")
    })
    
    # Prepare the results to go in the table (with the Drinks will now the row)
    long_select_customer <- reactive({
        select_drink() %>%
            gather("Drink", "Index")
    })
    
    # Display only the number of requested recomendations
    n_long_select_customer <- reactive({
        long_select_customer()[1:(input$nRecommend), ]
    })
    
    # Output the final table
    output$table <- renderTable({
        n_long_select_customer() 
    })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

