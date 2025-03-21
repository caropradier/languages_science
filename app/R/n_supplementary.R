fig_annex_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    plot_abs <- function(){
      
        d <- results_list$eng_abs_table
    
      
      plot <- d %>% 
        ggplot(aes(x = year, y = value, fill = ind
                   ,text = paste0('</br><b>',ind,'</b>',
                                  '</br>',year,
                                  '</br>N: ',value)
                   ))+
        geom_col(position = "dodge")+
        theme_minimal()+
        theme(legend.position = "top")+
        scale_fill_brewer(palette = "Paired")+
        labs(y = "n Publications", x = "Publication year", fill = "")
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    plot_links <- function(){
      
      d <- results_list$links_table
      
      plot <- d %>% 
        ggplot(aes(x = year, y = p, color = lang, group = lang
                   ,text = paste0('</br><b>',lang,'</b>',
                                  '</br>',year,
                                  '</br>Publications with citation links: ',round(p*100,2),'%')
                   ))+
        geom_line()+
        theme_minimal()+
        scale_color_brewer(palette = "Paired")+
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        theme(legend.position = "top")+
        labs(y = "% Publications with citation links", title = "", x = "Publication year",
             color = "")
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    
    
    output$plot_abs <- renderPlotly({plot_abs()})
    output$plot_links <- renderPlotly({plot_links()})
    

    
  })
}

fig_annex_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Supplementary",
           # fluidRow(
           #   column(12,
           #          textInput(ns("keyword"), "Search keyword:"),
           #          hr()
           #   )
           # ),
           fluidRow(
             column(12,
                    h4("Supplementary material"),
                    #h4(""),
                    fluidRow(
                      column(6,
                             p("Number of published articles and conference proceedings in Dimensions (1990-2023), in English and in total.", style = plot_title_style),
                    plotlyOutput(ns("plot_abs"),
                                 height = "500") %>% withSpinner(type = 5, color = "black")),
                   
                    
                    column(6,
                           p("Percentage of publications in Dimensions (1990-2023) with and without citation links according to language of publication", style = plot_title_style),
                    plotlyOutput(ns("plot_links"),
                                 height = "500") %>% withSpinner(type = 5, color = "black")
                      )),
                    hr(),
                    tags$div(
                      style = "width: 600px; text-align: justify;",
                    p("Our results represent lower-bound estimates of the linguistic distribution of publications and citations. This is due to a number of factors. First, OpenAlex often misclassifies non-English texts as if they were in English, which leads to an overestimation of English in detriment to other languages", 
                      a(href = 'https://doi.org/10.1002/asi.24979',"(Céspedes et al., 2025).") ,
                      "This means that, in reality, the research dissemination ecosystem is more multilingual than our results suggest. Second, citation links data are more limited for non-English publications. Third, bibliographic databases do not provide a complete coverage of non-mainstream circuits, where many publications often do not even have a DOI.",
                      style = plot_title_style))
                
                   
                  
           )
  ))
}