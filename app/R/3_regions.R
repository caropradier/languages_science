fig_regions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    plot_region_pub <- function(){
      
      d <- results_list$en_map_region
      
      plot <- d %>%
        mutate(fill_col = case_when(p >= .95 ~ "a",
                                    p < .95 & p >= .9 ~ "b",
                                    p < .9 ~"c")) %>% 
        ggplot(aes(x = reorder(continent,p,decreasing = TRUE), y = p, fill = fill_col
                   ,text = paste0('</br><b>',continent,'</b>',
                                  '</br>Articles in English: ',round(p*100,2),'%')
                   ))+
        geom_col()+
        geom_hline(yintercept = 1, linewidth = .1)+
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        scale_x_discrete(labels = function(x) str_wrap(x,20))+
        scale_fill_manual(values = c("#D4F1DCFF", "#A6E1BCFF",  "#35A0ABFF" )) +
        theme_minimal()+
        theme(legend.position = "none")+
        labs(x = "", 
             y = "% Articles in English", 
             title = "")+
        theme(panel.grid.major = element_blank())+
        theme(panel.grid.minor = element_blank())
      
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    plot_region_ref <- function(){
      
      d <- results_list$en_map_ref_region
      
      plot <- d %>%
        mutate(fill_col = case_when(p >= .99 ~ "a",
                                    p < .99 & p >= .98 ~ "b",
                                    p < .98 ~"c")) %>% 
        ggplot(aes(x = reorder(continent,p,decreasing = TRUE), y = p, fill = fill_col
                   ,text = paste0('</br><b>',continent,'</b>',
                                  '</br>References in English: ',round(p*100,2),'%')
                   ))+
        geom_col()+
        geom_hline(yintercept = 1, linewidth = .1)+
        scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
        scale_x_discrete(labels = function(x) str_wrap(x,20))+
        scale_fill_manual(values = c("#F8DBC6FF","#F69B71FF", "#E13342FF"))+
        theme_minimal()+
        theme(legend.position = "none")+
        labs(x = "", 
             y = "% References in English", 
             title = "")+
        theme(panel.grid.major = element_blank())+
        theme(panel.grid.minor = element_blank())
      
      
      ggplotly(plot, tooltip = c("text"))%>% layout(font = list(family = "Arial"))
      
    }
    
    plot_map_pub <- function(){

      d <- results_list$en_map

      mako_palette <- viridis(100, option = "mako",begin = .05,end = .95)
      palette <- colorBin(palette = mako_palette, domain = range(d$p, na.rm = TRUE),
                          na.color = "transparent",bins = c(0,.3,0.5,.6,.7,.8,.9,.95,1))

      leaflet(d) %>%
        setView(lat=25, lng=0,zoom = 1.5) %>%
        addTiles(options = tileOptions(maxZoom = 5,minZoom = 1.5)) %>%
        addPolygons(
          fillColor = ~palette(p),
          weight = 1,
          opacity = 1,
          color = "black",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
          label = ~paste0(region, ": ", round(p*100,2),"%")
        ) %>%
        addLegend(pal = palette,
                  values = ~p, opacity = 0.7,
                  title = "% Articles in English", position = "bottomright",
                  labFormat = labelFormat(suffix = "%", transform = function(x) round(x * 100, 2)))

    }
    
    plot_map_ref <- function(){
      
      d <- results_list$en_map_ref
      
      rocket_palette <- viridis(100, option = "rocket",begin = .05,end = .95)
      palette <- colorBin(palette = rocket_palette, domain = range(d$p, na.rm = TRUE),
                          na.color = "transparent",bins = c(.92,.93,.94,.95,.96,.97,.98,.99,1))
      
      leaflet(d) %>%
        setView(lat=25, lng=0,zoom = 1.5) %>%
        addTiles(options = tileOptions(maxZoom = 5,minZoom = 1.5)) %>%
        addPolygons(
          fillColor = ~palette(p),
          weight = 1,
          opacity = 1,
          color = "black",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
          label = ~paste0(region, ": ", round(p*100,2),"%")
        ) %>%
        addLegend(pal = palette,
                  values = ~p, opacity = 0.7,
                  title = "% References in English", position = "bottomright",
                  labFormat = labelFormat(suffix = "%", transform = function(x) round(x * 100, 2)))
      
    }

    output$map_pubs <- renderLeaflet({plot_map_pub()})
    output$region_pubs <- renderPlotly({plot_region_pub()})
    output$map_refs <- renderLeaflet({plot_map_ref()})
    output$region_refs <- renderPlotly({plot_region_ref()})

    
  })
}

fig_regions_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Regions",
           fluidRow(
             column(12,
                    h3("Where is multilingualism coming from? National and regional dissemination circuits."),
                    h4("Multilingualism in publications and cited references across the world (1990-2023)."),
                    tabsetPanel(
                      tabPanel("Publications",
                    fluidRow(
                      column(6,
                    leafletOutput(ns("map_pubs"), height = "700px",width = "700px") %>% withSpinner(type = 5, color = "black")
                    ,
                    plotlyOutput(ns("region_pubs"), height = 300,width = "700px") %>% withSpinner(type = 5, color = "black")
                    )
                    )
                    ),
                    tabPanel("Cited references",
                             fluidRow(
                    column(6,
                           leafletOutput(ns("map_refs"), height = "700px",width = "700px") %>% withSpinner(type = 5, color = "black")
                           ,
                           plotlyOutput(ns("region_refs"), height = 300,width = "700px") %>% withSpinner(type = 5, color = "black")
                    )
                             )
                    )
             )
           )
             )
           )
           
}