library(tidyverse)
library(plotly)
library(ggthemes)
library(scales)
library(wesanderson)
library(ggrepel)
library(viridis)
library(stats)
library(ggbump)
library(GGally)
library(openxlsx)
library(ggridges)
library(plotly)
library(wesanderson)
library(RColorBrewer)
library(leaflet)
library(htmltools)
library(sf)

options(scipen = 9999)
options(shiny.timeout = 300000)
options(shiny.maxRequestSize = 30*1024^2)

results_list <-  readRDS("www/results_list.RDS")

abstract <- "Language is a major source of systemic inequities in science, particularly among scholars whose first language is not English. Studies have examined scientists’ linguistic practices in specific contexts; few, however, have provided a global analysis of multilingualism in science. Using two major bibliometric databases (OpenAlex and Dimensions), we provide a large-scale analysis of linguistic diversity in science, considering both the language of publications (N=87,577,942) and of cited references (N=1,480,570,087)  For the 1990-2023 period, we find that only Indonesian, Portuguese and Spanish have expanded at a faster pace than English. Country-level analyses show that this trend is due to the growing strength of the Latin American and Indonesian academic circuits. Our results also confirm the own-language preference phenomenon–particularly for languages other than English–, the strong connection between multilingualism and bibliodiversity, and that social sciences and humanities are the least English-dominated fields. Our findings suggest that policies recognizing the value of both national-language and English-language publications have had a concrete impact on the distribution of languages in the global field of scholarly communication."

color_assign <- c("English" =  "#CAB2D6",
                  "French" =  "#FB9A99",
                  "Spanish" = "#B2DF8A",
                  "Japanese" = "#1F78B4",
                  "German" = "#33A02C",
                  "Portuguese" ="#E31A1C",
                  "Indonesian" = "#FDBF6F",
                  "Other" =  "#A6CEE3",
                  "Multilingual"="#6A3D9A"
                  #,"Italian" = "#FF7F00"
)

plot_title_style <- "color: black; font-size: 14px;"
