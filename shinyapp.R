library(shiny)
library(rvest)
library(dplyr)
library(DT)

# Define the scraper function
vectorize_item <- function(my_char_vec) {
  a <- unlist(strsplit(my_char_vec, "\nDozent/-in:\n"))
  a <- unlist(strsplit(a, "\nZeit und Ort:\n"))
  a <- unlist(strsplit(a, "\nwöchentlich "))
  a <- unlist(strsplit(a, "\t\t\n"))
  a <- unlist(strsplit(a, ", Raum:"))
  a <- unlist(strsplit(a, "\nZielgruppe:\n "))
  a <- unlist(strsplit(a, "\nZielgruppen:\n" ))
  a <- unlist(strsplit(a, "\n\n\nKommentar:\n\n"))
  return(a)
}

# Define the UI
ui <- fluidPage(
  titlePanel("jlu evv webscraper"),
  tags$div(HTML("<h4>by Thomas Haase")),
  sidebarLayout(
    sidebarPanel(
      HTML("<p>Choose your course of study at the evv until you only see a page of courses.</p>"),
      HTML("<p>For example: Here is the <a href='https://studip.uni-giessen.de/evv/extern.php?parent_id=d7f5ba03c2cc44c075b7a773ca11b44a' target='_blank'>Link</a> to the BA Social Sciences.</p>"),
      HTML("<p>Get the URL of the page and paste it below to get all courses with informations as a table</p>"),
      textInput("url", "Enter a URL:", value = ""),
      actionButton("scrapeButton", "Scrape Data"),
      downloadButton("downloadCSV", "Download CSV")
    ),
    mainPanel(
      DTOutput("scrapedData")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  scrapedData <- eventReactive(input$scrapeButton, {
    url <- input$url
    
    if (url == "") {
      return(NULL)
    }
    
    # Web scraping
    page <- read_html(url)
    lectureitems0 <- page %>% html_elements(".lectureItem0") %>% html_text2()
    lectureitems1 <- page %>% html_elements(".lectureItem1") %>% html_text2()
    items <- c(rbind(lectureitems1, lectureitems0))
    items_list <- sapply(items, vectorize_item)
    df <- do.call(rbind.data.frame, items_list)
    
    # Dynamically assign column names based on the number of columns in df
    colnames(df) <- paste("Column", 1:ncol(df))
    
    if (length(colnames(df)) >= 1) {
      names(df)[1] <- "title"
    }
    if (length(colnames(df)) >= 2) {
      names(df)[2] <- "lecturer"
    }
    if (length(colnames(df)) >= 3) {
      names(df)[3] <- "startdate"
    }
    if (length(colnames(df)) >= 4) {
      names(df)[4] <- "day_time"
    }
    if (length(colnames(df)) >= 5) {
      names(df)[5] <- "nedftdate"
    }
    if (length(colnames(df)) >= 6) {
      names(df)[6] <- "place"
    }
    if (length(colnames(df)) >= 7) {
      names(df)[7] <- "audience"
    }
    if (length(colnames(df)) >= 8) {
      names(df)[8] <- "comment"
    }
    
    
    return(df)
  })
  
  
  
  
  output$scrapedData <- renderDT({
    if (!is.null(scrapedData())) {
      datatable(scrapedData(), options = list(
        pageLength = 10, # Adjust the number of rows per page as needed
        autoWidth = TRUE  # Set autoWidth to TRUE to make the table width responsive
      ))
    }
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      "scraped_data.csv"
    },
    content = function(file) {
      if (!is.null(scrapedData())) {
        write.csv(scrapedData(), file)
      }
    }
  )
}

shinyApp(ui, server)
