#join up the dataset
library(tidyverse)
library(readr)
library(rvest)
library(arrow)
library(viridis)
options(scipen = 9999)

# page <- read_html('https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes')
# 
# language_trans <- html_table(html_node(page, ".wikitable"), fill = TRUE) |> 
#   select(name=`ISO Language Names`,language='Set 1' ) |> 
#   mutate(name = str_extract(name, "[^,]+")) |> 
#   filter(language != "Set 1")
# saveRDS(language_trans,"data/language_codes.RDS")

language_trans <- readRDS("data/language_codes.RDS")

year <- 1990

reference_pairs <- read_delim(paste0("data/data_raw/reference_pairs_",year,".csv"), 
                               delim = ";", escape_double = FALSE, 
                               col_names = c("citing_pub_id","cited_pub_id"),
                               na = c("NULL",""),
                               trim_ws = TRUE)

citing_lang <- read_delim(paste0("data/data_raw/citing_lang_",year,".csv"), 
                               delim = ";", escape_double = FALSE, 
                               col_names = c("citing_pub_id","citing_lang"),
                               na = c("NULL",""),
                               trim_ws = TRUE) %>% 
  mutate(citing_lang = case_match(citing_lang, .default = citing_lang,
                               'zh-tw'~'zh',
                               'zh-cn' ~'zh'))

reference_lang <- read_delim(paste0("data/data_raw/reference_lang_",year,".csv"), 
                               delim = ";", escape_double = FALSE, 
                               col_names = c("cited_pub_id","cited_lang","pub_type"),
                               na = c("NULL",""),
                               trim_ws = TRUE)%>% 
  mutate(cited_lang = case_match(cited_lang, .default = cited_lang,
                           'zh-tw'~'zh',
                           'zh-cn' ~'zh'))

all(citing_lang$citing_pub_id %in% reference_pairs$citing_pub_id)
all(reference_lang$cited_pub_id %in% reference_pairs$cited_pub_id)

#how many citing documents can be matched to openalex using their DOIs?
1-length(unique(citing_lang$citing_pub_id[is.na(citing_lang$citing_lang)]))/length(unique(citing_lang$citing_pub_id))

#how many cited documents can be matched to openalex using their DOIs?
length(unique(reference_lang$cited_pub_id))/length(unique(reference_pairs$cited_pub_id))
length(unique(reference_lang$cited_pub_id[!is.na(reference_lang$cited_lang)]))/length(unique(reference_pairs$cited_pub_id))

##checks
freqs_citing <- citing_lang %>% 
  group_by(citing_lang) %>% 
  summarise(n =n()) %>% 
  mutate(p = n/sum(n))

freqs_cited <- reference_lang %>% 
  group_by(cited_lang) %>% 
  summarise(n =n()) %>% 
  mutate(p = n/sum(n))

reference_lang %>% 
  group_by(pub_type) %>% 
  summarise(n =n()) %>% 
  mutate(p = n/sum(n))


######loop of the summary table########

# summary_table <- data.frame("citing_lang" = character(),
#                             "cited_lang" = character(), 
#                             "n"=numeric(), 
#                             "p_references" = numeric(), 
#                             "year" = numeric())

#year_vector <- c(1990:2023)

summary_table <- read_parquet("results/reference_freq.parquet")

year_vector <- c(2022:2023)

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)
  
  reference_pairs <- read_delim(paste0("data/data_raw/reference_pairs_",year,".csv"), 
                                delim = ";", escape_double = FALSE, 
                                col_names = c("citing_pub_id","cited_pub_id"),
                                na = c("NULL",""),
                                trim_ws = TRUE)
  
  #some cases have two languages assigned
  citing_lang <- read_delim(paste0("data/data_raw/citing_lang_",year,".csv"), 
                            delim = ";", escape_double = FALSE, 
                            col_names = c("citing_pub_id","citing_lang"),
                            na = c("NULL",""),
                            trim_ws = TRUE) %>% 
    mutate(citing_lang = case_match(citing_lang, .default = citing_lang,
                                    'zh-tw'~'zh',
                                    'zh-cn' ~'zh'))
  
  reference_lang <- read_delim(paste0("data/data_raw/reference_lang_",year,".csv"), 
                               delim = ";", escape_double = FALSE, 
                               col_names = c("cited_pub_id","cited_lang","pub_type"),
                               na = c("NULL",""),
                               trim_ws = TRUE)%>% 
    mutate(cited_lang = case_match(cited_lang, .default = cited_lang,
                                   'zh-tw'~'zh',
                                   'zh-cn' ~'zh'))
  
  full <- reference_pairs %>% 
    left_join(reference_lang %>% select(-pub_type), by = "cited_pub_id") %>% 
    left_join(citing_lang, by = "citing_pub_id")%>% 
    mutate(year = y)
  
  write_parquet(full, paste0("data/data_parquet/full_table_",year,".parquet"))
  
  summary <- full %>% 
    filter(!is.na(citing_lang)) %>% 
    filter(!is.na(cited_lang)) %>% 
    group_by(citing_lang,cited_lang) %>% 
    summarise(n = n()) %>% 
    group_by(citing_lang) %>% 
    mutate(t = sum(n)) %>% 
    mutate(p_references = n/t) %>% 
    select(-t) %>% 
    mutate(year = year_vector[y])
  
  summary_table <- bind_rows(summary_table,summary)
  
}

write_parquet(summary_table, "results/reference_freq.parquet")

#####second summary loop#####

# summary_table_citing <- data.frame("citing_lang" = character(),
#                                    "n"=numeric(),
#                                    "year" = numeric())

#year_vector <- c(1990:2023)

summary_table_citing <- read_parquet("results/citing_freq.parquet")

year_vector <- c(2022:2023)

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)
  
  citing_lang <- read_delim(paste0("data/data_raw/citing_lang_",year,".csv"), 
                            delim = ";", escape_double = FALSE, 
                            col_names = c("citing_pub_id","citing_lang"),
                            na = c("NULL",""),
                            trim_ws = TRUE) %>% 
    mutate(citing_lang = case_match(citing_lang, .default = citing_lang,
                                    'zh-tw'~'zh',
                                    'zh-cn' ~'zh'))
  

  
  summary <- citing_lang %>% 
    filter(!is.na(citing_lang)) %>% 
    group_by(citing_lang) %>% 
    summarise(n = n()) %>% 
    mutate(year = year_vector[y])
  
    summary_table_citing <- bind_rows(summary_table_citing,summary)
  
}

write_parquet(summary_table_citing, "results/citing_freq.parquet")


######data availability#######


# data_quality <- data.frame("n_citing"=numeric(),
#                            "n_cited"=numeric(),
#                            "n_citing_lang_data"=numeric(),
#                            "n_cited_lang_data"=numeric(),
#                            "year" = numeric())

#year_vector <- c(1990:2023)

data_quality <- read_parquet("results/data_quality.parquet")

year_vector <- c(2022:2023)

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)
  
  reference_pairs <- read_delim(paste0("data/data_raw/reference_pairs_",year,".csv"), 
                                delim = ";", escape_double = FALSE, 
                                col_names = c("citing_pub_id","cited_pub_id"),
                                na = c("NULL",""),
                                trim_ws = TRUE)
  
  n_citing <- n_distinct(reference_pairs$citing_pub_id)
  n_cited <- n_distinct(reference_pairs$cited_pub_id)
  rm(reference_pairs)
  
  citing_lang <- read_delim(paste0("data/data_raw/citing_lang_",year,".csv"), 
                            delim = ";", escape_double = FALSE, 
                            col_names = c("citing_pub_id","citing_lang"),
                            na = c("NULL",""),
                            trim_ws = TRUE)
  
  n_citing_lang_data <- n_distinct(citing_lang$citing_pub_id[!is.na(citing_lang$citing_lang)])
  rm(citing_lang)
  
  reference_lang <- read_delim(paste0("data/data_raw/reference_lang_",year,".csv"), 
                               delim = ";", escape_double = FALSE, 
                               col_names = c("cited_pub_id","cited_lang","pub_type"),
                               na = c("NULL",""),
                               trim_ws = TRUE)
  
  n_cited_lang_data <- n_distinct(reference_lang$cited_pub_id[!is.na(reference_lang$cited_lang)])
  rm(reference_lang)
  
t <- data.frame("n_citing" = n_citing,
            "n_cited" = n_cited,
            "n_citing_lang_data" = n_citing_lang_data,
            "n_cited_lang_data" = n_cited_lang_data,
            "year" = year_vector[y])

data_quality <- bind_rows(data_quality,t)
}

write_parquet(data_quality, "results/data_quality.parquet")

####reference type loop########

# reference_type_table <- data.frame("cited_lang" = character(),
#                                    "pub_type" = numeric(),
#                                    "n"=numeric(),
#                                    "year" = numeric())

#year_vector <- c(1990:2023)

reference_type_table <- read_parquet("results/reference_type.parquet")

year_vector <- c(2022:2023)

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)

reference_lang <- read_delim(paste0("data/data_raw/reference_lang_",year,".csv"), 
                             delim = ";", escape_double = FALSE, 
                             col_names = c("cited_pub_id","cited_lang","pub_type"),
                             na = c("NULL",""),
                             trim_ws = TRUE)%>% 
  mutate(cited_lang = case_match(cited_lang, .default = cited_lang,
                                 'zh-tw'~'zh',
                                 'zh-cn' ~'zh'))


type_summary <- reference_lang %>% 
  group_by(cited_lang,pub_type) %>% 
  summarise(n = n()) %>% 
  mutate(year = year_vector[y])

reference_type_table <- bind_rows(reference_type_table,type_summary)

}

write_parquet(reference_type_table, "results/reference_type.parquet")


####all pubs loop#####

# all_pubs_summary <- data.frame("lang" = character(),
#                                "n"=numeric(),
#                                "year" = numeric())

all_pubs_summary <- read_parquet("results/all_summary.parquet")

year_vector <- c(1991:2023)

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)
  
  all_lang <- read_delim(paste0("data/data_raw/all_lang_",year,".csv"), 
                         delim = ";", escape_double = FALSE, 
                         col_names = c("pub_id","lang"), 
                         na = c("NULL",""),
                         trim_ws = TRUE) %>% 
    filter(!is.na(lang))
  
  freq <- all_lang %>% 
    group_by(lang) %>% 
    summarise(n = n_distinct(pub_id)) %>% 
    mutate(year = year_vector[y])
  
  all_pubs_summary  <- bind_rows(all_pubs_summary,freq)
  
}

write_parquet(all_pubs_summary, "results/all_summary.parquet")
