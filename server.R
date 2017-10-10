library(shiny)
library(tm)
library(wordcloud2)
library(SnowballC)

summaries = data.frame(read_tsv('data/summaries.txt', col_names=FALSE))
desired_experience =
      lapply(summaries[,2], function(l)
          do.call(paste, as.list(gsub(' ', '_', unlist(strsplit(l, ', '))))))
## paste the skills to the descriptions
corpus = VCorpus(VectorSource(
    mapply(paste, gsub('_', '', summaries[,1]), unlist(desired_experience))))
## lowercase
corpus = tm_map(corpus, content_transformer(tolower))
## remove most punctuation
corpus = tm_map(corpus, content_transformer(function(x) gsub('[,:’;\\*!?\']', '', x)))
## handle parentheses
corpus = tm_map(corpus, content_transformer(function(x) gsub('[\\(\\)]', ' ', x)))
## handle periods
corpus = tm_map(corpus, content_transformer(function(x)
    gsub('\\.\\.+|\\.$|\\.(\\s)', '\\1', x)))
## replace slashes with spaces
corpus = tm_map(corpus, content_transformer(function(x) gsub('/', ' ', x)))
## get rid of words containing numbers
corpus = tm_map(corpus, content_transformer(function(x) gsub('[a-z0-9]*[0-9][a-z0-9]*', '', x)))
## remove stopwords
corpus = tm_map(corpus, removeWords, c(stopwords("english"), "youll", "youve", "weve", "can", "will", "data", "engineer"))
## build frequency matrix
frequencies = DocumentTermMatrix(corpus, control=list(stemming=TRUE))
allPostings = as.data.frame(as.matrix(frequencies))
## complete the stemmed words
## (ignoring those containing + since apparently stemCompletion uses
## regexp which is messed up by including the character '+')
colnames(allPostings) = sapply(colnames(frequencies), function(x) ifelse(grepl('\\+', x), x, stemCompletion(x, corpus)))
freq_df = data.frame(word=colnames(allPostings), freq=colSums(allPostings))

shinyServer(function(input, output, session) {

    observeEvent(input$clickbox, {
        boxname <- paste0("check", input$clickbox[1])
        updateCheckboxInput(session, boxname, value = !input[[boxname]])
    })

    wordcloud2_rep = repeatable(wordcloud2)

    output$wordCloud = renderWordcloud2({
        wordcloud2_rep(freq_df,
                       rotateRatio=0,
                       hover=NULL)
    })

    observe({print(paste('word:', gsub(":.*", "", input$selected_word)))})
    word = reactive({gsub(":.*", "", as.character(input$selected_word))})
    output$selected_word = renderText(word())
})
