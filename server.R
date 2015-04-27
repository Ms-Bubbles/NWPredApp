library(shiny)
library(tm)
library(SnowballC)
library(RWeka)
library(stringr)


#Read in various types of text files
ennews <- readLines("ennews.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
enblog <- readLines("enblog.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
entwitter <- readLines("entwitter.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
enaca1 <- readLines("enaca1.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
enaca2 <- readLines("enaca2.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
enfic1 <- readLines("enfic1.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
enfic2 <- readLines("enfic2.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
enmag1 <- readLines("enmag1.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
enmag2 <- readLines("enmag2.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
enbook <- readLines("enbook.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
profanity <- readLines("profanity.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
stopwords.EN <- stopwords(kind = "en")
stopwords.EN.v <- unlist(strsplit(stopwords.EN, " ", fixed = TRUE))
profanity.EN.v <- unlist(strsplit(profanity, "\n"))
TDM.EN.df.4 <- readRDS("TDMENdf4.rds")
TDM.EN.df.3 <- readRDS("TDMENdf3.rds")
TDM.EN.df.2 <- readRDS("TDMENdf2.rds")

#Create an empty list to collect results
resList <- c()

#Function for cleaning the user input string (ustring)

clean <- function(x) {
        string <- tolower(x)
        string <- gsub("[[:punct:]]+", "", string)
        string <- gsub("[[:digit:]]+", "", string)
        string <- gsub(paste(profanity.EN.v, collapse="\\b|\\b"),"", string)
        string <- gsub("^\\s+|\\s+$", "", string)
        string<- gsub(paste0(stopwords.EN.v, collapse="\\b|\\b"), "", string)
        string <- as.character(string)
        return(string)
        
}

# Next word generation function

nextWord <- function(string) {
        
        search.res <- TDM.EN.df.4[grep(string, TDM.EN.df.4$Tokens),]
        search.res <- search.res[order(search.res$TDM.EN.matrix.4, decreasing=TRUE),]
        res <- head(search.res$LW)
        resList <- c(resList, res)
        print(resList)
        #                         }
        
        #If no terms were found, shortening the search to 2 terms, and then 1
        
        if (nrow(search.res) <= 1) {
                print("No terms were found. Widening the search...")
                check <- word(string, -2:-1)
                check <- paste0(check, collapse = " ")
                search.res <- TDM.EN.df.3[grep(check, TDM.EN.df.3$Tokens),]
                search.res <- search.res[order(search.res$TDM.EN.matrix.3, decreasing=TRUE),]
        }
        
        if (nrow(search.res) != 0){
                res <- head(search.res$LW)
                resList <- c(resList, res)
                print(resList)
                
                
        }
        
        if (nrow(search.res) <= 1){
                print("No terms were found. Widening the search...")
                check <- word(string, -1)
                check <- paste0(check, collapse = " ")
                pattern <- paste("\\s",check, "\\s", sep="")
                search.res <- TDM.EN.df.2[grep(pattern, TDM.EN.df.2$Tokens),]
                search.res <- search.res[order(search.res$TDM.EN.matrix.2, decreasing=TRUE),]
        }
        
        if (nrow(search.res) != 0) {
                res <- head(search.res$LW)
                resList <- c(resList, res)
                print(resList)
                
        } else  {
                print("No matching terms were found")}
        
        
}


shinyServer(function(input, output) {
        start <- reactive({ Sys.time() })
        withProgress(message = "Cleaning user string", value = 0.1, {
                
                str <- reactive({  clean(input$ustring) })
                incProgress(0.1)
                setProgress(1, message = "String has been cleaned")
        })
        
        withProgress(message = "Predicting next word...", value = 0.1, {
                recNW <- reactive({ nextWord(str()) })
                incProgress(0.1)
                setProgress(1, message = "Prediction completed")
        })
        
        output$results <- renderText({ unique(recNW()) })
        end <- reactive({ Sys.time() })
        output$time <- renderText({ end() - start() })
})
