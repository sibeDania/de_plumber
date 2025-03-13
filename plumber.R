#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(tidyverse)

books <- readRDS("data/books.rds")


#* @apiTitle Data Engineering 2025 API
#* @apiDescription Plumber example description.

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a ggplot
#* @serializer png
#* @get /plot
function() {
    
  p <- ggplot2::ggplot(data = books, aes(author, year)) +
    geom_point()
  
  print(p)
  
}


#* Plot a histogram
#* @serializer csv
#* @get /csv
function() {
  
  p <- write.csv(books, "books.csv")
  
  return(p)
  
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
