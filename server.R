library(shiny)
library(tm)
library(wordcloud2)
library(SnowballC)
library(dplyr)
library(readr)

summaries <- data.frame(read_tsv('data/summaries.txt', col_names=FALSE))
desired_experience <-
      lapply(summaries[,2], function(l)
          do.call(paste, as.list(gsub(' ', '_', unlist(strsplit(l, ', '))))))
## paste the skills to the descriptions
corpus <- VCorpus(VectorSource(
    mapply(paste, gsub('_', '', summaries[,1]), unlist(desired_experience))))
## lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
## remove most punctuation
corpus <- tm_map(corpus, content_transformer(function(x) gsub('[,:’;\\*!?\']', '', x)))
## handle parentheses
corpus <- tm_map(corpus, content_transformer(function(x) gsub('[\\(\\)]', ' ', x)))
## handle periods
corpus <- tm_map(corpus, content_transformer(function(x)
    gsub('\\.\\.+|\\.$|\\.(\\s)', '\\1', x)))
## replace slashes with spaces
corpus <- tm_map(corpus, content_transformer(function(x) gsub('/', ' ', x)))
## get rid of words containing numbers
corpus <- tm_map(corpus, content_transformer(function(x) gsub('[a-z0-9]*[0-9][a-z0-9]*', '', x)))
## remove stopwords
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "youll", "youve", "weve", "can", "will", "data", "engineer"))
## build frequency matrix
frequencies <- DocumentTermMatrix(corpus, control=list(stemming<-TRUE))

## temporary to shorten startup time
frequencies = removeSparseTerms(frequencies, 0.9)

all_postings <- as.data.frame(as.matrix(frequencies))
## only have 1 or 0 to indicate whether word is in document
all_postings <- apply(all_postings, c(1,2), function(x) ifelse(x > 0, 1, 0))
## complete the stemmed words
## (ignoring those containing + since apparently stemCompletion uses
## regexp which is messed up by including the character '+')
colnames(all_postings) <- sapply(colnames(frequencies), function(x) ifelse(grepl('\\+', x), x, stemCompletion(x, corpus)))

## temporary to remove NAs
all_postings <- all_postings[,!is.na(colnames(all_postings))]

filtered_postings <- all_postings
freq_df <- data.frame(word=colnames(all_postings), freq=colSums(all_postings))

shinyServer(function(input, output, session) {
    ## make wordcloud2 use the same seed each time it is called
    wordcloud2_rep <- repeatable(wordcloud2)

    ## make a reactive value so it can be modified on events
    rv <- reactiveValues(filter_list=c(),
                        filtered_postings=all_postings)

    filtered_df <- reactive({
        df <- arrange(data.frame(word=colnames(rv$filtered_postings),
                                freq=colSums(rv$filtered_postings)),
                     desc(freq))
        filter(df, freq < nrow(rv$filtered_postings))
    })
    
    ## ignoreInit so it doesn't raise an error before word() has
    ## been initialized
    observeEvent(word(), {
        w <- word()
        if (length(w) > 0) {
            if (! w %in% rv$filter_list) {
                rv$filter_list <- c(rv$filter_list, w)
                print(rv$filter_list)
            }
            if (w %in% colnames(rv$filtered_postings)) {
                if (sum(rv$filtered_postings[,w]) > 1) {
                    col <- which(colnames(rv$filtered_postings) == w)
                    rv$filtered_postings <-
                        rv$filtered_postings[rv$filtered_postings[,w] > 0, -col]
                    rv$filtered_postings <- rv$filtered_postings[,colSums(rv$filtered_postings) > 0]
                    words_in_all_docs <-
                        colnames(rv$filtered_postings)[all(rv$filtered_postings > 0)]
                }
            }
        }
    }, ignoreInit=TRUE)

    wordcloud <- reactive({
        df <- filtered_df()
        if (nrow(df) > 0) {
            wordcloud2_rep(df, rotateRatio=0)
        }
    })

    output$wordcloud <- renderWordcloud2({wordcloud()})

    observe({print(paste('word:', gsub(":.*", "", input$selected_word)))})
    word <- reactive({gsub(":.*", "", input$selected_word)})

    output$selected_word <- renderText({word()})
})
