library(tidyverse)
library(arrow)
library(viridis)
library(wesanderson)
library(readxl)
library(patchwork)
library(countrycode)
library(RColorBrewer)
options(scipen = 9999)

results_list <-  readRDS("app/www/results_list.RDS")

all_pubs_summary <- read_parquet("results/all_summary.parquet")%>% 
  filter(year!=2024) 

language_trans <- readRDS("data/language_codes.RDS") 

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

color_assign <- c("English" =  "#CAB2D6",
                  "French" =  "#FB9A99",
                  "Spanish" = "#B2DF8A",
                  "Japanese" = "#1F78B4",
                  "German" = "#33A02C",
                  "Portuguese" ="#E31A1C",
                  "Indonesian" = "#FDBF6F",
                  "Other" =  "#A6CEE3",
                  "Multilingual"="#6A3D9A"
)

summary_table <- read_parquet("results/reference_freq.parquet")%>% 
  filter(year!=2024) 
summary_table_citing <- read_parquet("results/citing_freq.parquet") %>% 
  filter(year!=2024) 

country_all_pubs <- read_parquet("results/all_country.parquet")%>% 
  filter(year!=2024)
country_freqs <- read_parquet("results/country_lang.parquet")%>% 
  filter(year!=2024) 

####0 method section checks####

sum(all_pubs_summary$n)
sum(summary_table_citing$n)
sum(summary_table$n)
sum(summary_table$n[summary_table$cited_lang == "en"])/sum(summary_table$n)

rm(summary_table,summary_table_citing)

###1 articles#####

#####by language and year#####

a <- results_list$article_comp %>% 
  ggplot(aes(x = year, y = p, fill = name))+
  geom_col()+
  theme_minimal()+
  theme(legend.position = c(1.25,.75),
        legend.box = "front")+
  scale_fill_manual(values = color_assign)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(y = "% Publications", x = "Publication year", fill = "Language"
       ,title = "A"
  )+
  theme(panel.grid.major = element_line(color = alpha("grey", 0.01)))+
  theme(panel.grid.minor = element_line(color = alpha("grey", 0.01)))

a

b <-results_list$article_comp %>% 
  filter(name !="English") %>% 
  ggplot(aes(x = year, y = p, fill = name))+
  geom_col()+
  theme_minimal()+
  scale_fill_manual(values = color_assign)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"),position = "right")+
  theme(legend.position = "none")+
  labs(y = "% Publications", x = "Publication year", fill = "Language"
       ,title = "B"
  )+
  theme(panel.grid.major = element_line(color = alpha("grey", 0.01)))+
  theme(panel.grid.minor = element_line(color = alpha("grey", 0.01)))


a+b


plot_lang_year <- a+b

#####reference type #####

bibliodiv <- results_list$bibliodiv_table %>% 
  ggplot(aes(x = reorder(pub_type,p), y = p, fill = p))+
  geom_col()+
  geom_hline(yintercept = 1, linewidth = .2)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  scale_fill_viridis(option = "F",begin = .3, end=.95,direction = 1)+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none")+
  labs( x = "",
        y = "% References in English", title = "E")+
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank())

bibliodiv

#####discipline#####

dis_plot <- results_list$disc1%>% 
  filter(ind == "English") %>% 
  rename("lang" = "ind") %>% 
  ggplot(aes(x = reorder(main_field,-value), y = value, fill = lang))+
  geom_col()+
  geom_hline(yintercept = 1, linewidth = .1)+
  theme_minimal()+
  scale_fill_manual(values = color_assign)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  scale_x_discrete(labels = function(x) str_wrap(x,15))+
  theme(legend.position = "none")+
  labs(y = "% Publications", x = "", title = "F" , fill = "")+
  coord_flip()+
  guides(fill = guide_legend(nrow = 1)
         
  )+
  theme(panel.grid.minor = element_blank())

other_aspects <- bibliodiv + dis_plot


#####rolp#####

c <- results_list$same_lang_table%>% 
  ggplot(aes(x = reorder(name,p), y = p, fill = name))+
  geom_col()+
  geom_hline(yintercept = 1, linewidth = .2)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_manual(values = color_assign)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(y = "% References in same language as publication",
       x="Publication language", color = "", title = "C")+
  coord_flip()+
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank())


d <-  results_list$rolp_table %>% 
  ggplot(aes(x = name, y = norm_p, fill = name))+
  geom_col()+
  geom_hline(yintercept = 1, linewidth = .2)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_manual(values = color_assign)+
  scale_y_continuous(labels = function(x) paste0(x))+
  labs(y = "Relative Own-Language Preference",
       x="Publication language", color = "", title = "D")+
  coord_flip()+
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_blank())

plot_rolp <- c+d

plot_lang_year / plot_rolp / other_aspects  + plot_layout(heights = c(.65, .2,.15))


ggsave("results/first_submission/article_figure.png",bg="white",height = 9,width = 10)


###2 journals#####


###i need english to appear on the labels
dummy_data <- data.frame(
  year = NA,
  p = NA,
  name = "English",
  main_field = unique(results_list$journal_comp$main_field)[1] # Use one of the existing main_field values
)

a <- bind_rows(results_list$journal_comp,dummy_data) %>%
  mutate(name = factor(name,levels=c("Other",top_languages_name,"Multilingual"))) %>% 
  ggplot(aes(x = year, y = p, fill = name))+
  geom_col()+
  theme_minimal()+
  scale_fill_manual(values = color_assign)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%")
                     ,position = "right"
  )+
  theme(legend.position = c(.07,.7))+
  labs(y = "% Journals", title = "A", x = "Publication year", fill = "Language")+
  facet_wrap(~main_field)+
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank())

b <- bind_rows(results_list$multi_comp %>% select(-lang,-n),
               data.frame("p" = (1-sum(results_list$multi_comp$p)),
                          "name" = "Other")) %>%
  mutate(v = "Multi") %>%
  ggplot(aes(x = v, y = p, fill = reorder(name,p)))+
  geom_col(position = "stack")+
  scale_fill_manual(values = color_assign)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%")
                     ,position = "right"
  )+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(y = "Fractional proportion of multilingual journals", x = "",
       title = "B")+
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(axis.text.y = element_blank())

c <- results_list$boxplot %>% 
  ggplot(aes(x = reorder(name,p_ref), y = p_ref, fill = name))+
  geom_boxplot(size = .2, outlier.size = .5)+
  theme_minimal()+
  scale_fill_manual(values =color_assign)+
  theme(legend.position = "none")+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  labs(y = "% English references", x="Journal main publication language")+
  facet_wrap(~main_field)+
  coord_flip()+
  theme(panel.grid.minor = element_blank())


a/b/c + plot_layout(heights = c(.6,.05, .35))

ggsave("results/first_submission/journal_figure.png",bg="white",height =9,width = 10)


###3 maps#####

map <- ggplot2::map_data('world')%>%
  filter(region != "Antarctica") %>%
  mutate(iso2c = countrycode(region, 'country.name', 'iso2c')) %>%
  mutate(iso2c = case_when(region == "Kosovo" ~"XK",
                           region == "Micronesia" ~"FM",
                           region == "Western Sahara" ~ "MA",
                           TRUE ~ iso2c))

#####regions articles########

en_map <- country_all_pubs %>% 
  group_by(country_code,lang) %>% 
  summarize(n = sum(n)) %>% 
  group_by(country_code) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(lang == "en") %>% 
  filter(n>30)


# Define custom bins
bins <- c(0, 0.3, 0.5,.6, 0.7, .8, .9, .95, 1)
labels <- c("0-30", "30-50", "50-60", "60-70", "70-80", "80-90", "90-95", "95-100")

#mako(50)

colors <- c( "#2A1B34FF",
             "#403871FF",
             "#3A599AFF",
             "#3572A1FF",
             "#35A0ABFF",
             "#53C9ADFF",
             "#A6E1BCFF"  ,
             "#D4F1DCFF")


a <- map %>%
  left_join(en_map, by = c("iso2c" = "country_code")) %>%
  mutate(p = ifelse(is.na(p), 0, p)) %>% 
  mutate(p_bin = cut(p, breaks = bins, labels = labels, include.lowest = TRUE)) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = p_bin)) +
  geom_polygon(color = "black", size = .2) +
  scale_fill_manual(values = colors, labels = function(x) paste0(x, "%")) +
  theme_void() +
  labs(title = "A",
       fill = "% Articles \n in English",
       y = "", x = "") +
  coord_fixed(1.3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1, keywidth = 2, keyheight = 0.5, label.position = "bottom"))

a

b <-  results_list$en_map_region %>% 
  mutate(fill_col = case_when(p >= .95 ~ "a",
                              p < .95 & p >= .9 ~ "b",
                              p < .9 ~"c")) %>% 
  ggplot(aes(x = reorder(continent,p,decreasing = TRUE), y = p, fill = fill_col))+
  geom_col()+
  geom_hline(yintercept = 1, linewidth = .1)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  scale_x_discrete(labels = function(x) str_wrap(x,20))+
  scale_fill_manual(values = c("#D4F1DCFF", "#A6E1BCFF",  "#35A0ABFF" )) +
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "", 
       #y = "% Articles in English", 
       y = "",
       title = "B")+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())


g_a <- ggplotGrob(a) # Convert plot a to grob
g_b <- ggplotGrob(b) # Convert plot b to grob

g_a$layout$clip <- g_b$layout$clip <- "off"


marginb <- grid::textGrob("  ", gp = grid::gpar(fontsize = 10), rot = 90)
marginbs <- grid::textGrob("  ", gp = grid::gpar(fontsize = 1), rot = 90)

png("results/first_submission/region_map_lang_alt.png", width = 8, height = 7, units = "in", res = 300)
grid::grid.draw(
  gridExtra::grid.arrange(
    gridExtra::arrangeGrob(g_a, top = NULL, bottom = NULL, left = marginb, right = marginb),
    gridExtra::arrangeGrob(g_b,left = NULL, right = marginb, top = NULL, bottom = NULL),
    ncol = 1, heights = c(8, 3))
)
dev.off()


#####regions references########

en_map_ref <- country_freqs %>% 
  group_by(country_code,cited_lang) %>% 
  summarize(n = sum(n)) %>% 
  group_by(country_code) %>% 
  mutate(p = n/sum(n)) %>% 
  filter(cited_lang == "en")%>% 
  filter(n>30)



bins <- c(.92,.93, .94, .95,.96, 0.97, .98, .99, 1)
labels <- c("92-93","93-94","94-95","95-96","96-97","97-98","98-99","99-100")

rocket(50)
colors <- c("#3E1B43FF","#571E4FFF" , "#841E5AFF" , "#9C1B5BFF", "#CB1B4FFF", "#E63B40FF", "#F48059FF", "#F7C4A4FF")


a <- map%>%
  left_join(en_map_ref, by = c("iso2c" = "country_code")) %>%
  mutate(p = ifelse(is.na(p), 1, p)) %>% 
  mutate(p_bin = cut(p, breaks = bins, labels = labels, include.lowest = TRUE)) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = p_bin)) +
  geom_polygon(color = "black", size = .2) +
  scale_fill_manual(values = colors, labels = function(x) paste0(x, "%")) +
  theme_void() +
  labs(title = "A",
       fill = "% References \n in English",
       y = "", x ="")+
  coord_fixed(1.3) +
  theme(legend.position = "bottom")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1, keywidth = 2, keyheight = 0.5, label.position = "bottom"))

a

b <- results_list$en_map_ref_region %>% 
  mutate(fill_col = case_when(p >= .99 ~ "a",
                              p < .99 & p >= .98 ~ "b",
                              p < .98 ~"c")) %>% 
  ggplot(aes(x = reorder(continent,p,decreasing = TRUE), y = p, fill = fill_col))+
  geom_col()+
  geom_hline(yintercept = 1, linewidth = .1)+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  scale_x_discrete(labels = function(x) str_wrap(x,20))+
  scale_fill_manual(values = c("#F7C4A4FF","#F48059FF", "#CB1B4FFF"))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "", 
       y = "",
       title = "B")+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())

b


g_a <- ggplotGrob(a) # Convert plot a to grob
g_b <- ggplotGrob(b) # Convert plot b to grob

g_a$layout$clip <- g_b$layout$clip <- "off"


marginb <- grid::textGrob("  ", gp = grid::gpar(fontsize = 10), rot = 90)
marginbs <- grid::textGrob("  ", gp = grid::gpar(fontsize = 1), rot = 90)

png("results/first_submission/ref_region_map_lang_alt.png", width = 8, height = 7, units = "in", res = 300)
grid::grid.draw(
  gridExtra::grid.arrange(
    gridExtra::arrangeGrob(g_a, top = NULL, bottom = NULL, left = marginb, right = marginb),
    gridExtra::arrangeGrob(g_b,left = NULL, right = marginb, top = NULL, bottom = NULL),
    ncol = 1, heights = c(8, 3))
)
dev.off()

####annex#####

results_list$eng_abs_table%>% 
  ggplot(aes(x = year, y = value, fill = ind))+
  geom_col(position = "dodge")+
  theme_minimal()+
  theme(legend.position = "top")+
  scale_fill_brewer(palette = "Paired")+
  labs(y = "Publications",  x = "Publication year", fill = "")

ggsave("results/first_submission/annex_n_eng_year.png",bg="white",height = 6,width = 6)

results_list$links_table %>% 
  ggplot(aes(x = year, y = p, color = lang, group = lang))+
  geom_line()+
  theme_minimal()+
  scale_color_brewer(palette = "Paired")+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))+
  theme(legend.position = "top")+
  labs(y = "% Publications with citation links", x = "Publication year",
       color = "")

ggsave("results/first_submission/art_lang_year_by_cit_alt.png",bg="white",height = 6,width = 6)


