library(shiny)
library(shinythemes)
library(shinycssloaders)
library(glue)
library(plotly)
library(shinyjs)

# Source all R scripts in the R/ folder
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
sapply(r_files, source)


ui <- fluidPage(
  theme = shinytheme("paper"),
  
  #define fakeClick for buttons
  (tags$head(tags$script(HTML('var fakeClick = function(tabName) {
                                                         var dropdownList = document.getElementsByTagName("a");
                                                         for (var i = 0; i < dropdownList.length; i++) {
                                                         var link = dropdownList[i];
                                                         if(link.getAttribute("data-value") == tabName) {
                                                         link.click();
                                                         };
                                                         }
                                                         };
                                                         '))) ),
  
  
  

  
  uiOutput(outputId = "main_ui")
)


##### server #####

server <- function(input, output, session) {
  # render UI
  output$main_ui <- renderUI({
    useShinyjs()
    main_ui
  })
  
  ##########
  
  # Output modules ----------------------------------------------------------
  fig_articles_server("articles")
  fig_journals_server("journals")
  fig_regions_server("regions")
  fig_annex_server("annex")

}


##### RUN #####

shinyApp(ui, server)