fig_journals_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    plot_comp <- function(field){
      
      d <- results_list$journal_comp %>% 
        filter(main_field == field)
      
      plot <- d %>% 
        ggplot(aes(x = year, y = p, fill = name
                   ,text = paste0('</br><b>',name,'</b>',
                                 '</br>Publications: ',round(p*100,2),'%',
                                 '</br>Publication year: ',year)
                   ))+
        geom_col()+
        theme_minimal()+
        theme(legend.position = c(1.25,.75),
              legend.box = "front")+
        scale_fill_manual(values = color_assign)+
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        labs(y = "% Journals", x = "Publication year", fill = "Language",
             title = paste0(field)
        )+
        theme(panel.grid.major = element_line(color = alpha("grey", 0.01)))+
        theme(panel.grid.minor = element_line(color = alpha("grey", 0.01)))
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    
    plot_multi <- function(){
      
      d <- results_list$multi_comp
      
      plot <- d %>%
        ggplot(aes(x = reorder(name,p), y = p, fill = name
                   ,text = paste0('</br><b>',name,'</b>',
                                  '</br>Fractional proportion: ',round(p*100,2),'%')
                   ))+
        geom_col()+
        scale_fill_manual(values = color_assign)+
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        coord_flip()+
        theme_minimal()+
        #theme_void()+
        theme(legend.position = "none")+
        labs(y = "Fractional proportion of multilingual journals", x = "",
             title = "Multilingual journals")+
        theme(panel.grid.minor = element_blank())+
        theme(panel.grid.major = element_blank())
      
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    plot_box <- function(){
      
      d <- results_list$boxplot
      
      plot <- d %>% 
        ggplot(aes(x = reorder(name,p_ref), y = 100*p_ref, fill = name
                   # ,text = paste0('</br><b>',name,'</b>',
                   #                '</br>Fractional proportion: ',round(p*100,2),'%')
                   ))+
        geom_boxplot(size = .2, outlier.size = .5)+
        theme_minimal()+
        scale_fill_manual(values =color_assign)+
        theme(legend.position = "none")+
        scale_y_continuous(labels = function(x) paste0(x,"%"))+
        labs(y = "% English references", x="Journal main publication language")+
        facet_wrap(~main_field)+
        coord_flip()+
        theme(panel.grid.minor = element_blank())
      
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    
    output$plot_comp1 <- renderPlotly({plot_comp("MED")})
    output$plot_comp2 <- renderPlotly({plot_comp("NSE")})
    output$plot_comp3 <- renderPlotly({plot_comp("SSH")})
    output$plot_multi <- renderPlotly({plot_multi()})
    output$boxplots <- renderPlotly({plot_box()})
    

    
  })
}

fig_journals_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Journals",
           fluidRow(
             column(12,
                    h3("The role of journals as venues for non-English conversations."),
                    h4("Multilingualism at the journal level (1990-2023)."),
                    tabsetPanel(
                      tabPanel("Journals",
                               br(),
                    fluidRow(
                      p("Percentage of journals indexed in Dimensions by language and discipline, excluding English.", style = plot_title_style),
                      column(4,
                    plotlyOutput(ns("plot_comp1"), height = 600) %>% withSpinner(type = 5, color = "black")
                      ),
                    column(4,
                    plotlyOutput(ns("plot_comp2"), height = 600) %>% withSpinner(type = 5, color = "black")
                    ),
                    column(4,
                    plotlyOutput(ns("plot_comp3"), height = 600) %>% withSpinner(type = 5, color = "black")
                    )
                    ),
                    br(),
                    hr(),
                    br(),
                    p("Language composition of multilingual journals.", style = plot_title_style),
                    plotlyOutput(ns("plot_multi"), height = 300) %>% withSpinner(type = 5, color = "black"),
                    br(),
                    hr(),
                    br(),
                      ),
                    tabPanel("Cited references",
                             br(),
                    p("Distribution of English references by discipline and journal publication language.", style = plot_title_style),
                    plotlyOutput(ns("boxplots"), height = 600) %>% withSpinner(type = 5, color = "black"),
                    br(),
                    hr(),
                    br()
             )
           )
             )
           
           )
           )
}