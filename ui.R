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
            tags$head(tags$script(HTML(jscode))),
            actionButton("update", "Update results"),
            h3("Filtered words:"),
            textOutput("filter_list"),
            h3("Words in all documents:"),
            textOutput("all_doc_words")
        ),
        mainPanel(
            withSpinner(wordcloud2Output("wordCloud")),
            htmlOutput("jobs")
        )
    )
))
