library(shiny)

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
            textOutput("selected_word")
        ),
        mainPanel(
            wordcloud2Output("wordCloud")
        )
    )
))
