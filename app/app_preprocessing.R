library(tidyverse)
library(arrow)
library(readr)
library(readxl)
library(countrycode)
library(sf)
library(leaflet)
library(viridis)
library(htmlwidgets)
library(rmapshaper)

results_list <- list()

language_trans <- readRDS("data/language_codes.RDS") 

all_pubs_summary <- read_parquet("results/all_summary.parquet")%>% 
  filter(year!=2024) 

top_languages <- rev(all_pubs_summary %>% 
                       group_by(lang) %>% 
                       summarise(n = sum(n)) %>% 
                       arrange(-n) %>% 
                       slice_max(n = 8,order_by = n) %>% 
                       filter(lang != "ru") %>%
                       pull(lang))
top_languages_name <- language_trans %>%
  filter(language %in% top_languages) %>%
  arrange(match(language, top_languages)) %>%
  pull(name)

main_fields <- read_excel("data/discipline_regroup.xlsx",sheet = "Sheet1") %>% 
  select(for_division_id, "main_field" = "GROUP")


journal_language_data <- read_parquet("results/journal_lang.parquet")%>% 
  filter(year!=2024)
journal_reference_data <- read_parquet("results/journal_reference_lang.parquet")%>% 
  filter(year!=2024)

meta_journals<- read_parquet("results/journal_discipline.parquet")
meta_journals <- meta_journals %>% 
  left_join(main_fields, by ="for_division_id")

country_all_pubs <- read_parquet("results/all_country.parquet")%>% 
  filter(year!=2024) 

country_freqs <- read_parquet("results/country_lang.parquet")%>% 
  filter(year!=2024) 

reference_type<- read_parquet("results/reference_type.parquet")

summary_table <- read_parquet("results/reference_freq.parquet")%>% 
  filter(year!=2024) 
summary_table_citing <- read_parquet("results/citing_freq.parquet") %>% 
  filter(year!=2024) 

disc_all_pubs <- read_parquet("results/all_disc.parquet")%>% 
  filter(year!=2024) 
meta_freqs <- read_parquet("results/discipline_lang.parquet") %>% 
  left_join(main_fields,by ="for_division_id")%>% 
  filter(year!=2024) 


disc_country_all_pubs <- read_parquet("results/all_country_disc.parquet")%>% 
  filter(year!=2024) 
disc_country_freqs <- read_parquet("results/country_disc.parquet")%>% 
  filter(year!=2024) 

###basics####

aux_map <- ggplot2::map_data('world') %>% 
  select(region,subregion) %>% 
  unique()

world_data <- ggplot2::map_data('world') %>%
  group_by(group, region) %>%
  summarize(geometry = st_combine(st_polygon(list(cbind(long, lat))))) %>%
  ungroup() %>%
  st_as_sf() %>% 
  left_join(aux_map, by = "region") %>% 
  mutate(iso2c = countrycode(region, 'country.name', 'iso2c')) %>% 
  mutate(iso2c = case_when( region == "Western Sahara" ~ "MA",
                                                                                          TRUE ~iso2c)) 

results_list$map <- world_data

###article comp######

article_comp <- all_pubs_summary%>% 
  left_join(language_trans, by = c("lang" = "language")) %>% 
  filter(year != 2024) %>% 
  mutate(name = case_when(lang %in% top_languages ~ name,
                          TRUE ~paste0("Other"))) %>% 
  mutate(name = factor(name, levels = c("Other",top_languages_name))) %>% 
  group_by(year) %>% 
  mutate(p = n/sum(n)) %>% 
  select(-n, -lang) %>% 
  group_by(year,name) %>% 
  summarise(p = sum(p)) 

results_list$article_comp <- article_comp

####bibliodiversity####


pub_type_composition <- reference_type %>% 
  filter(!is.na(cited_lang)) %>% 
  group_by(cited_lang, pub_type) %>% 
  summarize(n = sum(n)) %>% 
  group_by(pub_type) %>% 
  mutate(p = n/sum(n)) %>% 
  group_by(pub_type) %>% 
  slice_max(n = 5, order_by = p)

bibliodiv_table <- pub_type_composition %>% 
  filter(cited_lang == "en") %>% 
  mutate(pub_type = case_when(pub_type == 1 ~ "Article",
                              pub_type == 2 ~ "Book",
                              pub_type == 3 ~ "Chapter",
                              pub_type == 4 ~ "Monograph",
                              pub_type == 5 ~ "Preprint",
                              pub_type == 6 ~ "Proceeding"))

results_list$bibliodiv_table <- bibliodiv_table

####language preference####

same_lang_table <- summary_table %>% 
  filter(citing_lang %in% top_languages) %>% 
  group_by(citing_lang,cited_lang) %>% 
  summarise(n = sum(n)) %>% 
  group_by(citing_lang) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(cited_lang == citing_lang) %>% 
  left_join(language_trans, by = c("citing_lang" = "language")) 

results_list$same_lang_table <- same_lang_table

expected <- all_pubs_summary %>% 
  group_by(lang) %>% 
  summarise(n = sum(n)) %>% 
  mutate(p_ex = n/sum(n)) %>% 
  select(-n)

observed <- summary_table %>% 
  filter(citing_lang %in% top_languages) %>% 
  group_by(citing_lang,cited_lang) %>% 
  summarise(n = sum(n)) %>% 
  group_by(citing_lang) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(cited_lang == citing_lang) %>% 
  select(-n) %>% 
  left_join(expected, by = c('cited_lang' = 'lang')) %>% 
  mutate(norm_p = p/p_ex) %>% 
  left_join(language_trans, by = c("citing_lang" = "language"))

rolp_table <- observed %>% 
  mutate(name = factor(name, levels = rev(c("English","Indonesian","Portuguese","French","German","Spanish","Japanese")))) 

results_list$rolp_table <- rolp_table

####disciplines#####

disc1 <- disc_all_pubs %>% 
  left_join(main_fields,by ="for_division_id") %>%
  group_by(main_field, lang) %>% 
  summarise(n = sum(n)) %>% 
  group_by(main_field) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(lang == "en") %>% 
  select(-n, -lang)%>% 
  mutate(rest = 1-p) %>% 
  rename("English" = "p", "All other languages" = "rest") %>% 
  pivot_longer(-main_field, names_to = "ind", values_to = "value") 

results_list$disc1 <- disc1


disc2 <- meta_freqs %>% 
  mutate(citing = factor(case_when(citing_lang == "en" ~ "English",
                                   TRUE ~ "All other languages"),
                         levels=c("English","All other languages"))) %>% 
  group_by(main_field,cited_lang,citing) %>% 
  summarise(n = sum(n)) %>% 
  group_by(main_field,citing) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(cited_lang == "en") 

results_list$disc2 <- disc2

expected <- disc_all_pubs %>% 
  left_join(main_fields,by ="for_division_id") %>%
  group_by(main_field, lang) %>% 
  summarise(n = sum(n)) %>% 
  group_by(main_field) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(lang == "en") %>% 
  select(-n, -lang)%>% 
  rename("p_ex" = "p")

disc3<- meta_freqs %>% 
  mutate(citing = factor(case_when(citing_lang == "en" ~ "English",
                                   TRUE ~ "All other languages"),
                         levels=c("English","All other languages"))) %>% 
  group_by(main_field,cited_lang,citing) %>% 
  summarise(n = sum(n)) %>% 
  group_by(main_field,citing) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(cited_lang == "en") %>% 
  left_join(expected, by = "main_field") %>% 
  mutate(p_norm = p/p_ex)

results_list$disc3 <- disc3




###journal comp######

simple <- journal_language_data %>% 
  group_by(source_id,year) %>% 
  slice_max(n = 1, order_by = p)

simple_90 <- simple %>% 
  mutate(lang = ifelse(p < 0.90, "multi", lang)) %>% 
  unique()

simple_90 <- simple_90 %>% 
  mutate(lang = case_when(lang %in% top_languages | lang == "multi" ~ lang,
                          TRUE ~paste0("other"))) %>% 
  mutate(lang = factor(lang, levels = c("other","multi",top_languages)))


journal_comp <- simple_90 %>% 
  left_join(meta_journals,by ="source_id") %>% 
  filter(!is.na(main_field)) %>% 
  group_by(year,lang,main_field) %>% 
  summarise(n = n_distinct(source_id)) %>% 
  group_by(year,main_field) %>% 
  mutate(p=n/sum(n)) %>% 
  left_join(language_trans, by = c("lang" = "language")) %>% 
  mutate(name = case_when(lang %in% top_languages ~ name,
                          lang == "multi" ~ paste0("Multilingual"),
                          TRUE ~paste0("Other"))) %>% 
  select(-lang) %>% 
  group_by(year , main_field,name) %>% 
  summarise(n = sum(n),
            p = sum(p)) %>% 
  mutate(name = factor(name, levels = c("Other",top_languages_name, "Multilingual"))) %>% 
  filter(name !="English") 

results_list$journal_comp <- journal_comp

####multi journal comp######

multi_90 <- simple_90 %>% 
  ungroup() %>% 
  filter(lang =="multi") %>% 
  select(source_id,"journal_lang" = "lang") %>% 
  left_join(journal_language_data, by = "source_id") %>% 
  group_by(lang) %>% 
  summarize(n = sum(p)) %>% 
  mutate(p = n/sum(n)) %>% 
  slice_max(n = 7, order_by = p)%>% 
  left_join(language_trans, by = c("lang" = "language"))

results_list$multi_comp <- multi_90


####journal boxplot#####

sub_samp_90 <- simple_90 %>% 
  filter(lang %in% top_languages) %>% 
  left_join(journal_reference_data %>% rename("p_ref" = "p"), by = c("source_id","year"))

boxplot_table <- sub_samp_90 %>% 
  filter(cited_lang == "en") %>% 
  left_join(meta_journals,by ="source_id") %>% 
  filter(!is.na(main_field)) %>% 
  left_join(language_trans, by = c("lang" = "language")) %>% 
  select(-p.x,-p.y,-for_division_id,-lang)

results_list$boxplot <- boxplot_table

###map publications#####

en_map_region <- country_all_pubs %>% 
  filter(country_code != "UNK") %>% 
  mutate(continent = countrycode(sourcevar = country_code,
                                 origin = "iso2c",
                                 destination = "un.regionsub.name")) %>% 
  #corrections
  mutate(continent = case_when(country_code == "XK" ~ "Eastern Europe",
                               continent %in% c("Melanesia","Micronesia","Polynesia","Australia and New Zealand") ~"Oceania",
                               continent %in% c("Central Asia","Eastern Asia")|country_code == "TW"  ~"North-eastern Asia",
                               TRUE ~ continent)) %>% 
  #regroup
  mutate(continent = case_when(continent %in% c("Western Europe","Northern Europe","Eastern Europe","Southern Europe" ) ~"Europe",
                               continent %in% c("Southern Asia","North-eastern Asia","South-eastern Asia","Western Asia") ~"Asia",
                               continent %in% c("Sub-Saharan Africa","Northern Africa") ~"Africa",
                               
                               TRUE ~ continent)) %>% 
  group_by(continent,lang) %>%
  summarize(n = sum(n)) %>%
  group_by(continent) %>%
  mutate(p = n/sum(n)) %>%
  filter(lang == "en")

results_list$en_map_region <- en_map_region


en_map <- country_all_pubs %>% 
  group_by(country_code,lang) %>% 
  summarize(n = sum(n)) %>% 
  group_by(country_code) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(lang == "en") %>% 
  filter(n>30)

mapi <- world_data %>% 
  left_join(en_map, by = c("iso2c" = "country_code")) 
mapi$p[is.na(mapi$p)] <- 0

results_list$en_map <- mapi
results_list$en_map <- ms_simplify(results_list$en_map, keep = 0.1)

# d <- results_list$en_map
# 
# mako_palette <- viridis(100, option = "mako",begin = .05,end = .95)
# palette <- colorBin(palette = mako_palette, domain = range(d$p, na.rm = TRUE), 
#                     na.color = "transparent",bins = c(0,.3,0.5,.6,.7,.8,.9,1))
# 
# #interactive_region_map <- 
#   leaflet(d) %>% 
#   setView(lat=25, lng=0,zoom = 1.5) %>%
#   addTiles(options = tileOptions(maxZoom = 5,minZoom = 1.5)) %>%
#   addPolygons(
#     fillColor = ~palette(p),
#     weight = 1,
#     opacity = 1,
#     color = "black",
#     fillOpacity = 0.7,
#     highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
#     label = ~paste0(region, ": ", round(p*100,2),"%")
#   ) %>%
#   addLegend(pal = palette, 
#             values = ~p, opacity = 0.7,
#             title = "% Articles in English", position = "bottomright",
#             labFormat = labelFormat(suffix = "%", transform = function(x) round(x * 100, 2)))

#saveWidget(interactive_region_map, file="app/www/map_publications.html")

###map references#####

en_map_ref <- country_freqs %>% 
  group_by(country_code,cited_lang) %>% 
  summarize(n = sum(n)) %>% 
  group_by(country_code) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(cited_lang == "en")%>% 
  filter(n>30)

mapi <- world_data %>% 
  left_join(en_map_ref, by = c("iso2c" = "country_code")) 
mapi$p[is.na(mapi$p)] <- 0

results_list$en_map_ref <- mapi
results_list$en_map_ref <- ms_simplify(results_list$en_map_ref, keep = 0.1)

en_map_ref_region <- country_freqs %>% 
  filter(country_code != "UNK") %>% 
  mutate(continent = countrycode(sourcevar = country_code,
                                 origin = "iso2c",
                                 destination = "un.regionsub.name")) %>% 
  #corrections
  mutate(continent = case_when(country_code == "XK" ~ "Eastern Europe",
                               continent %in% c("Melanesia","Micronesia","Polynesia","Australia and New Zealand") ~"Oceania",
                               continent %in% c("Central Asia","Eastern Asia")|country_code == "TW"  ~"North-eastern Asia",
                               TRUE ~ continent)) %>% 
  #regroup
  mutate(continent = case_when(continent %in% c("Western Europe","Northern Europe","Eastern Europe","Southern Europe" ) ~"Europe",
                               continent %in% c("Southern Asia","North-eastern Asia","South-eastern Asia","Western Asia") ~"Asia",
                               continent %in% c("Sub-Saharan Africa","Northern Africa") ~"Africa",
                               
                               TRUE ~ continent)) %>% 
  group_by(continent,cited_lang) %>%
  summarize(n = sum(n)) %>%
  group_by(continent) %>%
  mutate(p = n/sum(n)) %>%
  filter(cited_lang == "en")

results_list$en_map_ref_region <- en_map_ref_region


# d <- results_list$en_map_ref
# 
# rocket_palette <- viridis(100, option = "rocket",begin = .05,end = .95)
# palette <- colorBin(palette = rocket_palette, domain = range(d$p, na.rm = TRUE),
#                     na.color = "transparent",bins = c(.9,.91,.92,.93,.94,.95,.96,.97,.98,.99,1))
# 
# #interactive_reference_map <- 
#   leaflet(d) %>%
#   setView(lat=25, lng=0,zoom = 1.5) %>%
#   addTiles(options = tileOptions(maxZoom = 5,minZoom = 1.5)) %>%
#   addPolygons(
#     fillColor = ~palette(p),
#     weight = 1,
#     opacity = 1,
#     color = "black",
#     fillOpacity = 0.7,
#     highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
#     label = ~paste0(region, ": ", round(p*100,2),"%")
#   ) %>%
#   addLegend(pal = palette,
#             values = ~p, opacity = 0.7,
#             title = "% References in English", position = "bottomright",
#             labFormat = labelFormat(suffix = "%", transform = function(x) round(x * 100, 2)))

#saveWidget(interactive_reference_map, file="app/www/map_references.html")


#####discipline maps#######
# got an out of memory error

# 
# en_map_disc <- disc_country_all_pubs %>% 
#   left_join(main_fields,by ="for_division_id") %>% 
#   group_by(country_code,lang,main_field) %>% 
#   summarize(n = sum(n)) %>% 
#   group_by(country_code,main_field) %>% 
#   mutate(p = n/sum(n)) %>% 
#   filter(lang == "en") %>% 
#   filter(n>30)
# 
# mapi <- world_data %>% 
#   left_join(en_map_disc, by = c("iso2c" = "country_code")) 
# 
# results_list$en_map_disc <- mapi
# results_list$en_map_disc <- ms_simplify(results_list$en_map, keep = 0.1)
# 
# en_map_ref_disc <- disc_country_freqs %>% 
#   left_join(main_fields,by ="for_division_id") %>% 
#   group_by(country_code,cited_lang,main_field) %>% 
#   summarize(n = sum(n)) %>% 
#   group_by(country_code,main_field) %>% 
#   mutate(p = n/sum(n)) %>% 
#   filter(cited_lang == "en")%>% 
#   filter(n>30)
# 
# mapi <- world_data %>% 
#   left_join(en_map_ref_disc, by = c("iso2c" = "country_code")) 
# 
# results_list$en_map_ref_disc <- mapi
# results_list$en_map_ref_disc <- ms_simplify(results_list$en_map_ref_disc, keep = 0.1)

###annex#####

eng_abs_table <- all_pubs_summary %>% 
  filter(year != 2024) %>% 
  group_by(year) %>% 
  summarise(Total = sum(n),
            English = sum(n[lang == "en"])) %>% 
  pivot_longer(!year, names_to = "ind", values_to = "value") 

results_list$eng_abs_table <- eng_abs_table

links_table <- all_pubs_summary %>% 
  rename("full_n" = "n") %>% 
  left_join(summary_table_citing, by = c("lang" = "citing_lang", "year")) %>% 
  mutate(lang = ifelse(lang == "en", "English", "Other languages")) %>% 
  group_by(lang,year) %>% 
  summarise(n = sum(n),
            full_n = sum(full_n)) %>% 
  mutate(p = n/full_n) 

results_list$links_table <- links_table

#####save#####

saveRDS(results_list,"app/www/results_list.RDS")
#results_list <-  readRDS("app/www/results_list.RDS")

