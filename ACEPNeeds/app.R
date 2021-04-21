#
# This app will assess an EM physician's feelings and needs.
#

library(shiny)
library(shinyjs)
library(dplyr)
library(googlesheets4)
library(shinythemes)
library(wordcloud)
library(RColorBrewer)
library(tm)

# sheets reauth with specified token and email address
sheets_auth(
    cache = ".secrets",
    email = "spltt.tlb@gmail.com"
)

outputDir <- "Responses"

saveData <- function(data) {
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.csv(
        x = data,
        file = file.path(outputDir, fileName), 
        row.names = FALSE, quote = TRUE
    )
}

loadData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = TRUE) 
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
}

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Define UI for application
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                shinyjs::useShinyjs(),
                
                # Application title
                titlePanel("Emergency Medicine - Say What You're Thinking"),
                
                # Well panel for feelings
                splitLayout(cellArgs = list(style='white-space: normal'),
                        wellPanel(
                            div(id = "input_form",
                        titlePanel("I am an emergency physician and I feel _____________ ."),
                        helpText("Use the box below to enter a single word to describe how you feel about emergency medicine right now. (All lowercase, please.)"),
                        textInput("feels", "How I feel:"),
                        br(),
                        hr(),
                        titlePanel("I am an emergency physician and I need _____________ ."),
                        helpText("Use the box below to enter a single word to describe your most pressing need in emergency medicine right now. (All lowercase, please.)"),
                        textInput("needs", "What I need:"),
                        br(),
                        actionButton("submit", "Submit Your Thoughts"),
                        br()
                            ),
                        shinyjs::hidden(
                            div(id = "thanks",
                                helpText(tags$b("Thanks for your response! Turn up the volume by sharing!")))),
                        hr(),
                        helpText("Number of responses:"),
                        textOutput("num_respondents"),
                        br(),
                        tags$a(href = "https://docs.google.com/spreadsheets/d/1N6WbPZTJ5RXgmDV2ppueDFI0m9jRHhgln8ugOFtZ1fk/edit?usp=sharing", "SEE THE DATA")
                    ),
                    
                    # Main body of quiz
                    shinyjs::hidden(
                        div(id = "cloud_panel",
                    wellPanel(
                        titlePanel("Feelings"),
                        plotOutput("feels_cloud",
                                   #width = "500px", height = "500px"
                                   ),
                        titlePanel("Needs"),
                        plotOutput("needs_cloud",
                                   #width = "500px", height = "500px"
                                   )
                )
                        )
                )
                )
)

# Define server logic
server <- function(input, output) {
    
    v <- reactiveValues()
    v$initial <- data.frame()
    v$old <- as.data.frame(unclass(read_sheet("https://docs.google.com/spreadsheets/d/1N6WbPZTJ5RXgmDV2ppueDFI0m9jRHhgln8ugOFtZ1fk/edit?usp=sharing")), stringsAsFactors = TRUE)
    v$full <- as.data.frame(unclass(read_sheet("https://docs.google.com/spreadsheets/d/1N6WbPZTJ5RXgmDV2ppueDFI0m9jRHhgln8ugOFtZ1fk/edit?usp=sharing")), stringsAsFactors = TRUE)
    
    observeEvent(input$submit, {
        v$initial <- data.frame(Feels = input$feels,
                                Needs = input$needs)
        shinyjs::toggle(id = "input_form", anim = TRUE)
        shinyjs::toggle(id = "thanks", anim = TRUE)
        shinyjs::toggle(id = "cloud_panel", anim = TRUE)
        v$full <- rbind(v$initial, v$old)
        sheet_append("https://docs.google.com/spreadsheets/d/1N6WbPZTJ5RXgmDV2ppueDFI0m9jRHhgln8ugOFtZ1fk/edit?usp=sharing", v$initial[1,])
    })
    
    output$num_respondents <- reactive({
        nrow(v$full)
    })
    
    #This part of the program will generate the word clouds.
    
    output$feels_cloud <- renderPlot({
        docs <- Corpus(VectorSource(v$full$Feels))
        docs <- tm_map(docs, toSpace, "/")
        docs <- tm_map(docs, toSpace, "@")
        docs <- tm_map(docs, toSpace, "\\|")
        # Convert the text to lower case
        docs <- tm_map(docs, content_transformer(tolower))
        # Remove numbers
        docs <- tm_map(docs, removeNumbers)
        # Remove english common stopwords
        docs <- tm_map(docs, removeWords, stopwords("english"))
        # specify your stopwords as a character vector
        docs <- tm_map(docs, removeWords, c("be")) 
        # Remove punctuations
        docs <- tm_map(docs, removePunctuation)
        # Eliminate extra white spaces
        docs <- tm_map(docs, stripWhitespace)
        dtm <- TermDocumentMatrix(docs)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m),decreasing=TRUE)
        d <- data.frame(word = names(v),freq=v)
        wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                  max.words=30, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"), scale = c(4,0.2))
    })
    
    output$needs_cloud <- renderPlot({
        docs <- Corpus(VectorSource(v$full$Needs))
        docs <- tm_map(docs, toSpace, "/")
        docs <- tm_map(docs, toSpace, "@")
        docs <- tm_map(docs, toSpace, "\\|")
        # Convert the text to lower case
        docs <- tm_map(docs, content_transformer(tolower))
        # Remove numbers
        docs <- tm_map(docs, removeNumbers)
        # Remove english common stopwords
        docs <- tm_map(docs, removeWords, stopwords("english"))
        # specify your stopwords as a character vector
        docs <- tm_map(docs, removeWords, c("be")) 
        # Remove punctuations
        docs <- tm_map(docs, removePunctuation)
        # Eliminate extra white spaces
        docs <- tm_map(docs, stripWhitespace)
        dtm <- TermDocumentMatrix(docs)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m),decreasing=TRUE)
        d <- data.frame(word = names(v),freq=v)
        wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                  max.words=30, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"), scale = c(4,0.2))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)