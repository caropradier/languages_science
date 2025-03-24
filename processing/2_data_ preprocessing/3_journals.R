library(tidyverse)
library(arrow)
library(viridis)
library(wesanderson)
library(readxl)
library(patchwork)
library(countrycode)
options(scipen = 9999)

journal_pub_1990_2023 <- read_delim("data/journal_pub_1990_2023.csv", 
                                    na = c("NULL", ""),
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  filter(!is.na(source_id))

########journal language#######


journal_language_data <- data.frame("source_id" = character(),
                               "lang" = character(),
                               "p"=numeric(),
                               "year" = numeric())

year_vector <- c(1990:2023)

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)

all_lang <- read_delim(paste0("data/data_raw/all_lang_",year,".csv"), 
                       delim = ";", escape_double = FALSE, 
                       col_names = c("pub_id","lang"), 
                       na = c("NULL",""),
                       trim_ws = TRUE) %>% 
  filter(!is.na(lang))


journal_language <- all_lang %>% 
  left_join(journal_pub_1990_2023, by = "pub_id") %>% 
  group_by(source_id,lang) %>% 
  summarise(n = n_distinct(pub_id)) %>% 
  group_by(source_id) %>% 
  mutate(p = n/sum(n)) %>% 
  select(-n) %>% 
  group_by(source_id) %>% 
  slice_max(order_by = p, n = 2) %>% 
  mutate(year = year_vector[y])

journal_language_data <-  bind_rows(journal_language_data,journal_language)

write_parquet(journal_language_data,"results/journal_lang.parquet")

}


########journal references language#######


journal_reference_data <- data.frame("source_id" = character(),
                                    "cited_lang" = character(),
                                    "p"=numeric(),
                                    "year" = numeric())

year_vector <- c(1990:2023)

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)

lang_data <- read_parquet(paste0("data/data_parquet/full_table_",year,".parquet")) %>% 
  select(-year,-citing_lang) %>% 
  filter(!is.na(cited_lang)) 

journal_language <- lang_data %>% 
  left_join(journal_pub_1990_2023, by = c("citing_pub_id" = "pub_id")) %>% 
  group_by(source_id,cited_lang) %>% 
  summarise(n = n_distinct(cited_pub_id)) %>% 
  group_by(source_id) %>% 
  mutate(p = n/sum(n)) %>% 
  select(-n) %>% 
  group_by(source_id) %>% 
  slice_max(order_by = p, n = 2) %>% 
  mutate(year = year_vector[y])

journal_reference_data <- bind_rows(journal_reference_data,journal_language)

write_parquet(journal_reference_data,"results/journal_reference_lang.parquet")

}

######journal discipline#######

meta_table <- read_parquet("data/meta_table.parquet") %>% 
  filter(!is.na(for_division_id))

meta_journals <- journal_pub_1990_2023 %>% 
  left_join(meta_table, by =c("pub_id"="Pub_ID")) %>% 
  filter(!is.na(for_division_id)) %>% 
  group_by(source_id, for_division_id) %>% 
  summarise(n = n_distinct(pub_id)) %>% 
  group_by(source_id) %>% 
  mutate(p = n/sum(n)) %>% 
  group_by(source_id) %>% 
  slice_max(n=1, order_by = p)

write_parquet(meta_journals,"results/journal_discipline.parquet")
  