library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(gt)
library(markdown)

about_ui <- tabPanel(
  title = "Home",
  
  #h3(tags$em('A smack of all neighbouring languages:')),
  h3('How multilingual is scholarly communication?'),
  HTML('&nbsp;'),
  tags$div(
    style = "width: 600px; text-align: justify;",
  p(abstract, style = plot_title_style)),
  
  br(),
  
  HTML('&nbsp;'),
  
  h5("Contents"),
  
  a(tags$div(style = "width: 600px; text-align: justify;",
             h6('1. To what extent is English the language of science?')),
    onclick="fakeClick('Articles')",
    style ="text-decoration: none !important;"
  ),
  
  a(tags$div(style = "width: 600px; text-align: justify;",
             h6('2. The role of journals as venues for non-English conversations.')),
    onclick="fakeClick('Journals')",
    style ="text-decoration: none !important;"
  ),
  
  
  a(tags$div(style = "width: 600px; text-align: justify;",
             h6('3. Where is multilingualism coming from? National and regional dissemination circuits.')),
    onclick="fakeClick('Regions')",
    style ="text-decoration: none !important;"
  ),

  
  
  a(tags$div(style = "width: 600px; text-align: justify;",
             h6('4. Data & Methods')),
    onclick="fakeClick('Methods')",
    style ="text-decoration: none !important;"
  ),
  
  a(tags$div(style = "width: 600px; text-align: justify;",
             h6('5. Supplementary material')),
    onclick="fakeClick('Supplementary')",
    style ="text-decoration: none !important;"
  ),
  
  HTML('&nbsp;'),
  HTML('&nbsp;'),
  
  tags$div(
    style = "width: 600px; text-align: justify;",
    h4("About the app"),
    p("Our app is designed to help you further explore and understand our results. To read the full research paper, visit this", a(href ='',"site.")) 
  ),
  #add publication site!!!
  
  
)

methods <- tabPanel(
  title = "Methods",
  h4("Dataset and sources"),
  tags$div(
    style = "width: 600px; text-align: justify;",
  p("Data for this article were retrieved from the Dimensions and OpenAlex databases ",
    a(href = 'https://doi.org/10.1162/qss_a_00020',"(Herzog et al., 2020") ,
    "; ",
    a(href = 'http://arxiv.org/abs/2205.01833',"Priem et al., 2022).") ,
    
    "We examine all articles and conference proceedings indexed in Dimensions and published between 1990 and 2023. Language information was retrieved from OpenAlex, and matching between both bibliometric databases was based on Digital Object Identifier (DOI) matching.")),
  
  tags$div(
    style = "width: 600px; text-align: justify;",
    p("Country-level authorship is computed using fractional counting. Each publication is divided by its number of authors, and these fractions are then assigned to each country according to each author’s first institutional affiliation (in cases of multiple affiliations). These fractions are later aggregated to determine the proportion of the articles authored by each country (the sum of all fractions equals the number of publications in our dataset). ")
    ),
  
  tags$div(
    style = "width: 600px; text-align: justify;",
    p("Finally, articles in our corpus were published in 107,259 distinct journals. Journal language was defined in terms of the most frequent language of publication. In cases where the most frequent language accounted for less than 90% of publications, the journal was considered multilingual.")
  )
  
)


main_ui <- {
  navbarPage(
    "Languages in science",
    about_ui,
    fig_articles_ui("articles"),
    fig_journals_ui("journals"),
    fig_regions_ui("regions"),
    methods,
    fig_annex_ui("annex")
    
  )
}