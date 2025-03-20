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

#* @get /books
#* @param author
#* @param year
#* @param from_year
#* @param to_year
#* 
function(author = NULL, year = NULL, from_year = NULL, to_year = NULL) {
  result <- books
  
  if (!is.null(author)) {
    result <- result[grep(author, result$author, ignore.case = TRUE), ]
  }
  if (!is.null(year)) {
    result <- result[result$year == as.numeric(year), ]
  }
  if (!is.null(from_year)) {
    result <- result[result$year >= as.numeric(from_year), ]
  }
  if (!is.null(to_year)) {
    result <- result[result$year <= as.numeric(to_year), ]
  }
  return(result)
  
}

#* @post /books
#* @param title:string Book title
#* @param author:string Author name
#* @param year:integer Publication year

function(title, author, year) {
  
  new_book <- data.frame(
    title = title,
    author = author,
    year = as.integer(year),
    stringsAsFactors = FALSE
  )
  
  books <- rbind(books, new_book)
  
  return(list(
    message("Book added succesfully"),
    book = new_book
  ))
  
}

#####






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
