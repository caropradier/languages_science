fig_articles_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    plot_comp <- function(en){
      
      if(en){
        d <- results_list$article_comp
      } else {
        d <- results_list$article_comp %>% 
          filter(name !="English")
      }
        
      
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
        labs(y = "% Publications", x = "Publication year", fill = "Language"
        )+
        theme(panel.grid.major = element_line(color = alpha("grey", 0.01)))+
        theme(panel.grid.minor = element_line(color = alpha("grey", 0.01)))
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    plot_bibliodiv <- function(){
      
      d <- results_list$bibliodiv_table
      
      plot <- d %>% 
        ggplot(aes(x = reorder(pub_type,p), y = p, fill = p
                   ,text = paste0('</br><b>',pub_type,'</b>',
                                  '</br>References in English: ',round(p*100,2),'%')
                   ))+
        geom_col()+
        geom_hline(yintercept = 1, linewidth = .2)+
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        scale_fill_viridis(option = "F",begin = .3, end=.95,direction = 1)+
        coord_flip()+
        theme_minimal()+
        theme(legend.position = "none")+
        labs(x = "Reference type", y = "% References in English")+
        theme(panel.grid.minor = element_blank())+
        theme(panel.grid.major = element_blank())
      
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    plot_same_lang <- function(){
      
      d <- results_list$same_lang_table
      
      plot <- d %>% 
        ggplot(aes(x = reorder(name,p), y = p, fill = name
                   ,text = paste0('</br><b>',name,'</b>',
                                  '</br>References in same language as publication: ',round(p*100,2),'%')
                   ))+
        geom_col()+
        geom_hline(yintercept = 1, linewidth = .2)+
        theme_minimal()+
        theme(legend.position = "none")+
        scale_fill_manual(values = color_assign)+
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        labs(y = "% References in same language as publication",
             x="Publication language", color = "")+
        coord_flip()+
        theme(panel.grid.minor = element_blank())+
        theme(panel.grid.major = element_blank())
      
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    plot_rolp <- function(){
      
      d <- results_list$rolp_table
      
      plot <- d %>% 
        ggplot(aes(x = name, y = norm_p, fill = name
                   ,text = paste0('</br><b>',name,'</b>',
                                  '</br>Relative Own-Language Preference: ',round(norm_p,2))
                   ))+
        geom_col()+
        geom_hline(yintercept = 1, linewidth = .2)+
        theme_minimal()+
        theme(legend.position = "none")+
        scale_fill_manual(values = color_assign)+
        scale_y_continuous(labels = function(x) paste0(x))+
        labs(y = "Relative Own-Language Preference",
             x="Publication language", color = "")+
        coord_flip()+
        theme(panel.grid.minor = element_blank())+
        theme(panel.grid.major = element_blank())
      
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
  
    plot_disciplines1 <- function(){
      
      
      d <- results_list$disc1
      
      plot <- d %>% 
      ggplot(aes(x = main_field, y = value, fill = ind
                 ,text = paste0('</br><b>',main_field,'</b>',
                                '</br>',ind,
                                '</br>Publications: ',round(value*100,2),'%')
                 ))+
      geom_col(position = "stack")+
      geom_hline(yintercept = 1, linewidth = .1)+
      theme_minimal()+
        theme(legend.position = "none")+
      scale_fill_manual(values = wes_palette("GrandBudapest2")[c(2,4)],
                        breaks = c("English","All other languages"))+
      scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
      scale_x_discrete(labels = function(x) str_wrap(x,15))+
      labs(y = "% Publications", x = "", title = "" , fill = "")+
      coord_flip()+
      guides(fill = guide_legend(nrow = 1)
      )+
        theme(panel.grid.minor = element_blank())+
        theme(panel.grid.major = element_blank())
      
        ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
        
    }
    
    plot_disciplines2 <- function(){
      
      
      d <- results_list$disc2
      
      plot <- d %>%
      ggplot(aes(x = main_field, y = p, fill = citing
                 ,text = paste0('</br><b>',main_field,'</b>',
                                '</br>',citing,
                                '</br>References in English: ',round(p*100,2),'%')
                 ))+
      geom_col(position = "dodge")+
      geom_hline(yintercept = 1, linewidth = .1)+
      theme_minimal()+
        theme(legend.position = "none")+
      scale_fill_manual(values = wes_palette("GrandBudapest2")[c(2,4)])+
      scale_y_continuous(labels = function(x) paste0(x*100,"%"),breaks = c(0,.25,.5,.75,1), limits = c(0,1.1))+
      scale_x_discrete(labels = function(x) str_wrap(x,15))+
      theme(legend.text = element_text(size = 12))+
      labs(y = "% References in English", x = "", title = "" , fill = "",subtitle = "")+
      coord_flip()+
      guides(fill = guide_legend(nrow = 1))+
        theme(panel.grid.minor = element_blank())+
        theme(panel.grid.major = element_blank())
        
        ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    plot_disciplines3 <- function(){
      
      d <- results_list$disc3
      
      plot <- d %>%
      ggplot(aes(x = main_field, y = p_norm, fill = citing
                 ,text = paste0('</br><b>',main_field,'</b>',
                                '</br>',citing,
                                '</br>Relative English-Language Preference: ',round(p_norm,2))
                 ))+
      geom_col(position = "dodge")+
      geom_hline(yintercept = 1, linewidth = .1)+
      theme_minimal()+
        theme(legend.position = "none")+
      scale_fill_manual(values = wes_palette("GrandBudapest2")[c(2,4)])+
      scale_x_discrete(labels = function(x) str_wrap(x,15))+
      scale_y_continuous(breaks = c(0,.25,.5,.75,1), limits = c(0,1.1))+
      theme(legend.text = element_text(size = 12))+
      labs(y = "Relative English-Language Preference", x = "", title = "" , fill = "",subtitle = "")+
      coord_flip()+
      guides(fill = guide_legend(nrow = 1))+
        theme(panel.grid.minor = element_blank())+
        theme(panel.grid.major = element_blank())
        
        ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    output$plot_comp1 <- renderPlotly({plot_comp(TRUE)})
    output$plot_comp2 <- renderPlotly({plot_comp(FALSE)})
    
    output$plot_biblio <- renderPlotly({plot_bibliodiv()})
    output$plot_same <- renderPlotly({plot_same_lang()})
    output$plot_rolp <- renderPlotly({plot_rolp()})
    
    output$plot_disciplines1 <- renderPlotly({plot_disciplines1()})
    output$plot_disciplines2 <- renderPlotly({plot_disciplines2()})
    output$plot_disciplines3 <- renderPlotly({plot_disciplines3()})
    

    
  })
}

fig_articles_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Articles",
           fluidRow(
             column(12,
                    h3("To what extent is English the language of science?"),
                    h4("Multilingualism at the article level (1990-2023)."),

                    tabsetPanel(
                      tabPanel("Articles",
                               br(),
                    fluidRow(
                      column(6,
                             p("Percentage of articles indexed in Dimensions by language.", style = plot_title_style),
                    plotlyOutput(ns("plot_comp1"),
                                 height = "500px",) %>% withSpinner(type = 5, color = "black")
                      ),
                    column(6,
                           p("Percentage of articles indexed in Dimensions by language, excluding English.", style = plot_title_style),
                           plotlyOutput(ns("plot_comp2"),
                                        height = "500px",) %>% withSpinner(type = 5, color = "black")
                    )
                    ),
                    br(),
                    hr(),
                    br()
                    ),
                    
                    tabPanel("Bibliodiversity",
                             br(),
                    p("Percentage of English-language references by document type.", style = plot_title_style),
                    plotlyOutput(ns("plot_biblio"),
                                 height = "300",) %>% withSpinner(type = 5, color = "black"),
                    br(),
                    hr(),
                    br()),
                    tabPanel("Own-Language Preference",
                             br(),
                    fluidRow(
                      column(6,
                    p("Percentage of cited references in the same language as the citing publication.", style = plot_title_style),
                    plotlyOutput(ns("plot_same"),
                                 height = "300",) %>% withSpinner(type = 5, color = "black")
                    ),
                    column(6,
                    
                    p("Relative Own-Language Preference", style = plot_title_style),
                    plotlyOutput(ns("plot_rolp"),
                                 height = "300",) %>% withSpinner(type = 5, color = "black")
                      )
             ),
             br(),
             hr(),
             br()
             ),
             tabPanel("Disciplines",
                      br(),
             p("Percentage of publications in English, by discipline.", style = plot_title_style),
             plotlyOutput(ns("plot_disciplines1"),
                          height = "200",) %>% withSpinner(type = 5, color = "black"),
             hr(),
             br(),
             p("Percentage of cited references in English by discipline, by English-language and other language citing documents.", style = plot_title_style),
             plotlyOutput(ns("plot_disciplines2"),
                          height = "200",) %>% withSpinner(type = 5, color = "black"),
             hr(),
             br(),
             p("Relative English-Language Preference by discipline, from English-language and other language citing documents.", style = plot_title_style),
             plotlyOutput(ns("plot_disciplines3"),
                          height = "200",) %>% withSpinner(type = 5, color = "black"),
             hr(),
             p("SSH: Social Sciences and Humanities; NSE: Natural Sciences and Engineering; MED: Biomedical and Health Sciences.")
             )
           
             
                    )
             )
  ))
}