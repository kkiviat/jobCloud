library(shiny)
library(wordcloud2)
library(shinycssloaders)

jscode <- '
$(function() {
  $(document).on(\'click\', \'#canvas\', function() {
    word = document.getElementById("wcSpan").innerHTML;
    Shiny.onInputChange("selected_word", word);
  });
});
'

shinyUI(fluidPage(
    titlePanel("Word cloud for job search results"),
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition="input.conditionedPanels==1",
                             tags$head(tags$script(HTML(jscode))),
                             h3("Number of postings:"),
                             textOutput("num_docs"),
                             h3("Filtered words:"),
                             textOutput("filter_words"),
                             h3("Words in all postings:"),
                             textOutput("all_doc_words")
                             ),
            conditionalPanel(condition="input.conditionedPanels==2",
                             sliderInput("k", "Enter number of clusters", 1, max_clusters, 2)
                             )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("filter", value=1,
                         withSpinner(wordcloud2Output("wordCloud")),
                         htmlOutput("jobs")
                         ),
                tabPanel("cluster", value=2,
                         uiOutput("clusters")),
                         ## wordcloud2Output("cluster1"),
                         ## wordcloud2Output("cluster2")),
                id = "conditionedPanels"
        )
        )
    )
))
