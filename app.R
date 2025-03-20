#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The very important Shiny app for books"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          width = 3,
          actionButton("refresh", "Refresh Data"),
          hr(),
          uiOutput("author_filter")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            
            tabPanel("Author books with GET request",
                     verbatimTextOutput("stats")
            ),
            tabPanel("Add a new book with POST request",
                     verbatimTextOutput("new_book_details"),
                     h4("Add New Book"),
                     textInput("new_title", "Title"),
                     textInput("new_author", "Author"),
                     numericInput("new_year", "Year", value = 2024, min = 1000, max = 2024),
                     actionButton("add_book", "Add Book"),
                     textOutput("add_status"))
          )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive function to fetch data with error handling
  author_data <- eventReactive(input$refresh, {
    tryCatch({
      response <- GET("http://localhost:8000/authors/stats")
      if (status_code(response) == 200) {
        output$error_message <- renderText({ "" })
        
        # Get the JSON data
        data <- fromJSON(rawToChar(response$content))
        
        # Convert to data frame directly
        df <- data.frame(
          Author = names(data),
          Total_Books = sapply(data, function(x) x$total_books),
          Years_Active = sapply(data, function(x) x$years_active),
          Books = sapply(data, function(x) paste(x$books, collapse = ", ")),
          stringsAsFactors = FALSE
        )
        
        return(df)
        
      } else {
        output$error_message <- renderText({ 
          paste("API Error:", status_code(response))
        })
        return(NULL)
      }
    }, error = function(e) {
      output$error_message <- renderText({ 
        "Error: Cannot connect to API. Make sure it's running on port 8000."
      })
      return(NULL)
    })
  }, ignoreNULL = FALSE)
  
  output$author_filter <- renderUI({
    
    selectInput("author_filter", "Choose an author",
                choices = sort(author_data()$Author))
    
  })
  
  # Display raw JSON data
  output$stats <- renderPrint({
    
    df <- author_data()
    
    filtered_df <- df %>% 
      filter(Author == input$author_filter)
    
    str(filtered_df)# Make sure it matches your selectInput id
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
