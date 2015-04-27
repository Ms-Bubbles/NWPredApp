shinyUI(fluidPage(
        
        titlePanel("Word Prediction Application"),
        
        navlistPanel(
                "Navigation Panel",
                tabPanel("Application",
                         h3("Enter your text in the box below and press Submit"),
                         textInput("ustring", label = h5("Prediction app will return (if available) alternative suggestions for the next word."), value = "Enter text here (min. 3 words)..."),
                         submitButton("Submit"),
                         hr(),
                         fluidRow(column(3, verbatimTextOutput("results"))),
                         hr(),
                         h5("Time it took to predict next word:"),
                         fluidRow(column(3, verbatimTextOutput("time")))
                         
                         
                ),
                "-----",
                tabPanel("About",
                         h3("A little bit about the app"),
                         hr(),
                         h4("This application is a submission for the Capstone Project of the Data Science Specialization."),
                         hr(),
                         h4("Behind the App"),
                         hr(),
                         h5("Is based on n-gram language modelling.Which in short indicates the sequences in which words occur in a sentence and the probablitiy of this occurance. In other words: it checks the probability of an upcoming word.
                            Uses Stupid Backoff concept. For next word predicition the algorithm will first take 3 last words of a cleaned string and check it against a 4-gram, if no results will be found it will shorten the string to 2 tokens and renew the search for 3-gram. This process will be repeated until unigram level.
                            Uses a variety of source texts in order to provide best predictions for different language types. 
                            App uses dictionaries based on texts from academic and fiction literature, news, magazines as well as less formal blogs and twitter messages.
                            Provides basic cleaning processes to assure good quality predictions. After having created a language corpus it lowers the font, cleans from digits and punctuation and applies a profanity filter. This process ensures better search results."),
                         hr(),
                         h4("About the App"),
                         hr(),
                         h5("It's small, simple, fast...Sleek and simple UI assures ease of use for anyone** 
                            Enter your string in the text box and press Submit to start the predcition. Progress bar at the top indicates the status of the predictor
                            Open Source approach assures that the application is a living matter and can be improved at any time by anyone. All to become more precise, more efficient and up-to-date with the ever evolving langauge processes.
                            "),
                         hr(),
                         h4('GitHub directory:'),
                         h4('https://github.com/Ms-Bubbles/NWPredApp/')
                         
                         ),
                
                tabPanel("Refrences",
                         h3("List of refrences used in this application"),
                         hr(),
                         h4("Text Corpuses"),
                         strong("HC corpora"),
                         h5("http://www.corpora.heliohost.org/"),
                         hr(),
                         strong("Corpus.byu.edu"),
                         h5("http://corpus.byu.edu/"),
                         hr(),
                         strong("William S. Balch, Lectures on Language
                         As Particularly Connected with English Grammar."),
                         h5("https://www.gutenberg.org/ebooks/17594"),
                         hr(),
                         h4("Material Reference on NLP"),
                         strong("Natural Language Processing course by Dan Jurafsky, Christopher Manning"),
                         h5("https://class.coursera.org/nlp/lecture"),
                         hr(),
                         strong("Structured Prediction for Natural Language Processing"),
                         h5("http://www.cs.cmu.edu/~nasmith/sp4nlp/"),
                         hr(),
                         strong("Text Mining the Complete Works of William Shakespeare by andrew"),
                         h5("http://www.r-bloggers.com/text-mining-the-complete-works-of-william-shakespeare/")
                         
                         
                )
                         )
                ))