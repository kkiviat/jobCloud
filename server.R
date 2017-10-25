library(shiny)
library(tm)
library(wordcloud2)
library(SnowballC)
library(dplyr)
library(readr)

remove_word_from_dtm = function(dtm, w) {
    col <- which(colnames(dtm) == w)
    ## remove documents not containing w
    dtm <- dtm[dtm[, w] > 0,]
    ## remove column corresponding to w
    dtm <- dtm[, -col]
    ## remove columns of words not appearing in remaining documents
    dtm <- dtm[, colSums(dtm) > 0]
    return(dtm)
}

## given a data frame built from a DocumentTermMatrix
## and a list of numbers specifying some of these documents,
## creates a data frame suitable for wordcloud2
create_wc2_input_from_documents <- function(doc_list, df) {
    df = df[doc_list, ]
    data.frame(word=colnames(df),
               freq=colSums(df))
}

summaries <- data.frame(read_tsv('data/summaries.txt', col_names=FALSE))
links <- paste0('<a href=', summaries$X3, '>', summaries$X4, '</a>')
documents <- paste(links, summaries$X5, summaries$X1, summaries$X2, sep='<br/>')
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
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "youll", "youve", "weve", "can", "will"))
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


filtered_postings <- all_postings
freq_df <- data.frame(word=colnames(all_postings), freq=colSums(all_postings))

###########################
## Clustering
###########################
distMatrix <- dist(scale(frequencies))
fit <- hclust(distMatrix, method="ward.D")


###########################
## Server
###########################

shinyServer(function(input, output, session) {
    ## make wordcloud2 use the same seed each time it is called
    wordcloud2_rep <- repeatable(wordcloud2)

    ## make a reactive value so it can be modified on events
    values <- reactiveValues(filter_list=c(),
                            filtered_postings=all_postings,
                            all_doc_words=c())

    filtered_df <- reactive({
        arrange(data.frame(word=colnames(values$filtered_postings),
                           freq=colSums(values$filtered_postings)),
                desc(freq))
    })

    ## ignoreInit so it doesn't raise an error before word() has
    ## been initialized
    observeEvent(word(), {
        w <- word()
        if (! w %in% values$filter_list) {
            values$filter_list <- c(values$filter_list, w)
        }
        if (w %in% colnames(values$filtered_postings)) {
            values$filtered_postings <-
                remove_word_from_dtm(values$filtered_postings, w)

            in_all_docs <-
                colSums(values$filtered_postings) == nrow(values$filtered_postings)

            ## save words in all documents
            values$all_doc_words <- c(values$all_doc_words,
                                     colnames(values$filtered_postings)[in_all_docs])

            ## remove words in all documents
            values$filtered_postings <-
                values$filtered_postings[, !in_all_docs]
        }
    }, ignoreInit=TRUE)

    jobs <- reactive({documents[as.integer(rownames(values$filtered_postings))]})
    output$jobs <- renderUI({
        HTML(do.call(paste, c(as.list(jobs()), sep='<br/><br/>')))
    })
    
    num_docs <- reactive({nrow(values$filtered_postings)})
    output$num_docs <- renderText(num_docs())

    all_doc_words <- reactive({values$all_doc_words})
    output$all_doc_words <- renderText(do.call(paste, as.list(all_doc_words())))

    filter_words <- reactive({as.list(values$filter_list)})
    output$filter_words <- renderText(do.call(paste, filter_words()))
    
    
    wordCloud <- reactive({
        df <- filtered_df()
        if (nrow(df) > 0) {
            wordcloud2_rep(df, rotateRatio=0)
        }
    })
    output$wordCloud <- renderWordcloud2({wordCloud()})

    observe({print(paste('word:', gsub(":.*", "", input$selected_word)))})
    word <- reactive({gsub(":.*", "", input$selected_word)})
    output$selected_word <- renderText(word())


    ## cluster_list <- reactive({
    ##     clusters <- cutree(fit, input$k)
    ##     clusters <- split(names(clusters), clusters)
    ##     print(clusters)
    ##     df_list <- lapply(clusters, function(doc_list) {
    ##         create_wc2_input_from_documents(doc_list, all_postings)
    ##         })
    ##     df_list
    ## })

    ## output$clusters <- renderUI({
    ##     wordcloud2Output_list <- lapply(1:input$k, function(i) {
    ##         wordcloud2Output(paste0("cluster", i))
    ##     })
    ##     print(tagList(wordcloud2Output_list))
    ##     do.call(tagList, wordcloud2Output_list)
    ## })

  ##   ## based on https://gist.github.com/wch/5436415
  ##   for (i in 1:max_clusters) {
  ##   # Need local so that each item gets its own number. Without it, the value
  ##   # of i in the renderPlot() will be the same across all instances, because
  ##   # of when the expression is evaluated.
  ##   local({
  ##     my_i <- i
  ##     cluster_name <- paste0("cluster", my_i)
  ##     output[[cluster_name]] <- renderWordcloud2({
  ##       wordcloud2_rep(cluster_list()[[my_i]])
  ##     })
  ##   })
  ## }

})
