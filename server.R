library(shiny)
library(tm)
library(wordcloud2)
library(SnowballC)
library(dplyr)
library(readr)

summaries <- data.frame(read_tsv('data/summaries.txt', col_names=FALSE))
documents <- paste(summaries$X1, summaries$X2, sep='<br/>')
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

## ## temporary to remove NAs
## all_postings <- all_postings[,!is.na(colnames(all_postings))]

filtered_postings <- all_postings
freq_df <- data.frame(word=colnames(all_postings), freq=colSums(all_postings))

#### This prints the filter_list in the UI!!!!!
shinyServer(function(input, output, session) {
    ## make wordcloud2 use the same seed each time it is called
    wordcloud2_rep = repeatable(wordcloud2)

    ## make a reactive value so it can be modified on events
    values = reactiveValues(filter_list=c(),
                            filtered_postings=all_postings)

    filtered_df <- reactive({
        df <- arrange(data.frame(word=colnames(values$filtered_postings),
                                freq=colSums(values$filtered_postings)),
                     desc(freq))
        ## remove the words that appear in all remaining documents
        filter(df, freq < nrow(values$filtered_postings))
    })

    ## ignoreInit so it doesn't raise an error before word() has
    ## been initialized
    observeEvent(word(), {
        w <- word()
        if (! w %in% values$filter_list) {
            values$filter_list = c(values$filter_list, w)
            print(values$filter_list)
        }
        if (word() %in% colnames(values$filtered_postings)) {
            col <- which(colnames(values$filtered_postings) == w)
            ## remove documents not containing w
            values$filtered_postings <-
                values$filtered_postings[values$filtered_postings[,w] > 0,]
            ## remove column corresponding to w
            values$filtered_postings = values$filtered_postings[,-col]
            ## remove columns of words not appearing in remaining documents
            values$filtered_postings <- values$filtered_postings[,colSums(values$filtered_postings) > 0]
        }
    }, ignoreInit=TRUE)

    ## workaround for now since the ui doesn't seem to update
    ## the rendered filter_list when the wordcloud is redrawn
    filter_list <- eventReactive(input$update > 0, {
        values$filter_list
    })
    output$filter_list = renderText(do.call(paste, as.list(filter_list())))
    jobs <- eventReactive(input$update > 0, {
        documents[as.integer(rownames(values$filtered_postings))]
    })
    output$jobs <- renderUI({
        HTML(do.call(paste, c(as.list(jobs()), sep='<br/><br/>')))
    })
    
    wordCloud = reactive({
        wordcloud2_rep(filtered_df(), rotateRatio=0)
    })

    output$wordCloud = renderWordcloud2({wordCloud()})

    observe({print(paste('word:', gsub(":.*", "", input$selected_word)))})
    word = reactive({gsub(":.*", "", input$selected_word)})

    output$selected_word = renderText(word())
})
