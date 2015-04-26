library(shiny)
library(tm)
library(SnowballC)
library(RWeka)
library(stringr)

# setwd("D:/RWorking Directory/CapstoneProject/Dataset/EN")
# 
# #Read in various types of text files
# en.news <- readLines("en_US.news.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# en.blog <- readLines("en_US.blogs.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# en.twitter <- readLines("en_US.twitter.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# en.aca1 <- readLines("w_acad_2006.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# en.aca2 <- readLines("w_acad_1996.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# en.fic1 <- readLines("w_fic_2002.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# en.fic2 <- readLines("w_fic_2012.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# en.mag1 <- readLines("w_mag_1990.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# en.mag2 <- readLines("w_mag_2000.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# en.book <- readLines("pg17594.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# profanity.EN <- readLines("profanity.txt", skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
# # 
# #Sample the data to smaller portions for better performance
# set.seed(1210)
# en.news <- en.news[sample(1:length(en.news), 25000, replace = FALSE)]
# en.blog <- en.blog[sample(1:length(en.blog), 25000, replace = FALSE)]
# en.twitter <- en.twitter[sample(1:length(en.twitter), 25000, replace = FALSE)]
# 
# en.news <- as.String(en.news)
# en.blog <- as.String(en.blog)
# en.twitter <- as.String(en.twitter)
# 
# #Create a new directory for the data, which will be used for the corpus creation
# 
# mainDir <- getwd()
# subDir <- "TrainSet"
# 
# dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
# new.dir <- file.path(mainDir, subDir)
# 
# setwd(new.dir)
# 
# write(en.news, file = "en.news.txt")
# write(en.blog, file = "en.blog.txt")
# write(en.twitter, file = "en.twitter.txt")
# write(en.aca1, file = "en.aca1.txt")
# write(en.aca2, file = "en.aca2.txt")
# write(en.fic1, file = "en.fic1.txt")
# write(en.fic2, file = "en.fic2.txt")
# write(en.mag1, file = "en.mag1.txt")
# write(en.mag2, file = "en.mag2.txt")
# write(en.book, file = "en.book.txt")
# write(profanity.EN, file = "profanity.EN.txt")

#Create corpus and clean the data
stopwords.EN <- stopwords(kind = "en")
stopwords.EN.v <- unlist(strsplit(stopwords.EN, " ", fixed = TRUE))
profanity.EN.v <- unlist(strsplit(profanity.EN, "\n"))
en.corpus <- VCorpus(DirSource(mainDir),readerControl = list(language = "en"))
en.corpus <- tm_map(en.corpus, removeWords, stopwords.EN)
en.corpus <- tm_map(en.corpus, removeWords, profanity.EN.v)
en.corpus <- tm_map(en.corpus, content_transformer(tolower))
en.corpus <- tm_map(en.corpus, content_transformer(removePunctuation))
en.corpus <- tm_map(en.corpus, content_transformer(removeNumbers))

#en.corpus <- tm_map(en.corpus, stemDocument)
en.corpus <- tm_map(en.corpus, stripWhitespace)

#Tokenize, create TDMs and lastly df's for final word search
# setwd(mainDir)

#N4s
Tokenizer.4 <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
TDM.EN.4 <- TermDocumentMatrix(en.corpus, control = list(tokenize = Tokenizer.4))
TDM.EN.matrix.4 <- as.matrix(TDM.EN.4)
TDM.EN.matrix.4 <- rowSums(TDM.EN.matrix.4) 
TDM.EN.matrix.4 <- sort(TDM.EN.matrix.4, decreasing=TRUE)
TDM.EN.df.4 <- as.data.frame(TDM.EN.matrix.4)
TDM.EN.df.4$Tokens <- row.names(TDM.EN.df.4)
row.names(TDM.EN.df.4) <- NULL

#find non-characters
remove <- c('[\\]\\[\\(\\)-/+;:#%$^\\*=^~\\{\\}/"<>«»_\\\\“\\”°•‘’–]')
TDM.EN.df.4$Tokens <- gsub(paste(remove,collapse="|"), "", TDM.EN.df.4$Tokens)

##Extract last word
library(stringr)
TDM.EN.df.4$LW <- str_extract(TDM.EN.df.4$Tokens, '\\w+$')

#N3s
Tokenizer.3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
TDM.EN.3 <- TermDocumentMatrix(en.corpus, control = list(tokenize = Tokenizer.3))
TDM.EN.matrix.3 <- as.matrix(TDM.EN.3)
TDM.EN.matrix.3 <- rowSums(TDM.EN.matrix.3) 
TDM.EN.matrix.3 <- sort(TDM.EN.matrix.3, decreasing=TRUE)
TDM.EN.df.3 <- as.data.frame(TDM.EN.matrix.3)
TDM.EN.df.3$Tokens <- row.names(TDM.EN.df.3)
row.names(TDM.EN.df.3) <- NULL
#find non-characters
TDM.EN.df.3$Tokens <- gsub(paste(remove,collapse="|"), "", TDM.EN.df.3$Tokens)

##Extract last word
TDM.EN.df.3$LW <- str_extract(TDM.EN.df.3$Tokens, '\\w+$')

#N2s
Tokenizer.2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TDM.EN.2 <- TermDocumentMatrix(en.corpus, control = list(tokenize = Tokenizer.2))
TDM.EN.matrix.2 <- as.matrix(TDM.EN.2)
TDM.EN.matrix.2 <- rowSums(TDM.EN.matrix.2) 
TDM.EN.matrix.2 <- sort(TDM.EN.matrix.2, decreasing=TRUE)
TDM.EN.df.2 <- as.data.frame(TDM.EN.matrix.2)
TDM.EN.df.2$Tokens <- row.names(TDM.EN.df.2)
row.names(TDM.EN.df.2) <- NULL
#find non-characters
TDM.EN.df.2$Tokens <- gsub(paste(remove,collapse="|"), "", TDM.EN.df.2$Tokens)

#Extract last word
TDM.EN.df.2$LW <- str_extract(TDM.EN.df.2$Tokens, '\\w+$')

#Create an empty list to collect results
resList <- c()

#Function for cleaning the user input string (ustring)

clean <- function(x) {
        string <- tolower(x)
        string <- gsub("[[:punct:]]+", "", string)
        string <- gsub("[[:digit:]]+", "", string)
        remove <- c('[\\]\\[\\(\\)-/+;:#%$^\\*=^~\\{\\}/"<>«»_\\\\“\\”°•‘’–]')
        string <- gsub(paste(remove,collapse="|"), "", string)
        string <- gsub(paste(profanity.EN.v, collapse="\\b|\\b"),"", string)
        string <- gsub("^\\s+|\\s+$", "", string)
        string<- gsub(paste0(stopwords.EN.v, collapse="\\b|\\b"), "", string)
        string <- as.character(string)
        return(string)
        
}

# Next word generation function

nextWord <- function(string) {
#         
#         if (sum((sapply(gregexpr("\\W+", string), length) + 1)) < 3) {
#                         
#                                 stop("Exiting... String is too short.")
#                         
#                         } else {
        
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
        
        
        withProgress(message = "Cleaning user string", value = 0.1, {
        
                str <- reactive({ 
                        start <- Sys.time()
                        clean(input$ustring) })
                 incProgress(0.1) 
                 setProgress(1, message = "String has been cleaned")
                })
#         output$cleanStr <- renderText({ str() })
        
        withProgress(message = "Predicting next word...", value = 0.1, {
        recNW <- reactive({ nextWord(str()) })   
        incProgress(0.1) 
        setProgress(1, message = "Prediction completed")
        })
        
        output$results <- renderText({ unique(recNW()) })
        output$time <- renderText({ Sys.time() - start()  })
})
