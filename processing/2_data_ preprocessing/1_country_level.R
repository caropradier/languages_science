library(tidyverse)
library(readr)
library(rvest)
library(arrow)
library(viridis)
library(countrycode)
library(sf)
options(scipen = 9999)

#####fractional data#####

author_info <- read_delim("data/author_data/full_author_info.csv",
                          na = c("", "NULL")) %>% 
  mutate(author_id = as.character(author_id))

fractional_country_table <- data.frame("Pub_ID" = NA,"country_code" = NA)

year_vector <- c(1990:2023)

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)
  
  table <- read_delim(paste0("data/author_data/global_authors_",year,".csv"), 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE,
                      na = c("", "NULL")) %>% 
    mutate(author_id = as.character(author_id)) %>% 
    select(-author_seq) %>% 
    left_join(.,author_info, by = "author_id") %>% 
    filter(!is.na(country_code)) %>% 
    distinct(Pub_ID,author_id,.keep_all = TRUE) %>% 
    group_by(Pub_ID,country_code) %>% 
    summarise(n = n()) %>% 
    group_by(Pub_ID) %>% 
    mutate(p = n/sum(n)) %>% 
    select(-n)
  
  
  fractional_country_table <- bind_rows(fractional_country_table,table)
  
}

fractional_country_table <- fractional_country_table %>% 
  filter(!is.na(Pub_ID))

write_parquet(fractional_country_table,"data/paper_country_fractional.parquet")


####language - country#####


lang_data <- read_parquet(paste0("data/data_parquet/full_table_",year,".parquet"))

full_citing_country <- lang_data %>% 
  filter(!is.na(citing_lang)) %>% 
  filter(!is.na(cited_lang)) %>% 
  left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>% 
  filter(!is.na(country_code))

country_cited_freq <- full_citing_country %>% 
  group_by(country_code,citing_lang,cited_lang) %>% 
  summarise(n = sum(p)) %>% 
  group_by(country_code, citing_lang) %>% 
  mutate(p =n/sum(n)) %>% 
  select(-n) %>% 
  left_join(country_citing_freq, by = c("country_code","citing_lang")) %>% 
  mutate(n=p*n) %>% 
  select(-p)

######loops#######

paper_country_fractional <- read_parquet("data/paper_country_fractional.parquet") %>% mutate(country_code = case_when(country_code == "SJ" ~ "NO",                                                    country_code == "AX" ~ "FI",                                                                TRUE ~country_code))

country_all_pubs <-  data.frame("lang" = character(),
                                     "country_code" = character(),
                              "n"=numeric(),
                              "year" = numeric())

country_all_pubs <- read_parquet("results/all_country.parquet")

year_vector <- c(1991:2024)

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)
  
  all_lang <- read_delim(paste0("data/data_raw/all_lang_",year,".csv"), 
                         delim = ";", escape_double = FALSE, 
                         col_names = c("pub_id","lang"), 
                         na = c("NULL",""),
                         trim_ws = TRUE) %>% 
    filter(!is.na(lang))
  
  all_data <- all_lang %>% 
    left_join(paper_country_fractional, by = c("pub_id" = "Pub_ID")) %>% 
    filter(!is.na(country_code))
  
  full_freq <- all_data %>% 
    group_by(lang,country_code) %>% 
    summarise(n = sum(p)) %>% 
    mutate(year = year_vector[y])
  
  country_all_pubs  <- bind_rows(country_all_pubs,full_freq)
  
  rm(all_data)
  gc()
  
}

write_parquet(country_all_pubs, "results/all_country.parquet")







#-------------

# country_freqs_citing <- data.frame("country_code" = character(),
#                             "citing_lang" = character(),
#                             "n"=numeric(),
#                             "year" = numeric())

year_vector <- c(1990:2024)

country_freqs_citing <- read_parquet("results/citing_country_lang.parquet")

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
  
  citing_country <- citing_lang %>% 
    filter(!is.na(citing_lang)) %>% 
    left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>% 
    filter(!is.na(country_code))
  
  country_citing_freq <- citing_country %>% 
    group_by(country_code,citing_lang) %>% 
    summarise(n = sum(p))%>% 
    mutate(year = year_vector[y])
  
  rm(citing_lang,citing_country)
  
  country_freqs_citing <- bind_rows(country_freqs_citing,country_citing_freq)
  
}


write_parquet(country_freqs_citing, "results/citing_country_lang.parquet")

# country_freqs <- data.frame("country_code" = character(),
#                             "citing_lang" = character(),
#                             "cited_lang" = character(),
#                             "n"=numeric(),
#                             "year" = numeric())

#year_vector <- c(1990:2024)

country_freqs <- read_parquet("results/country_lang.parquet")

year_vector <- c(2018:2024)
#crashed :( --- solved with alt version

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)
  
  lang_data <- read_parquet(paste0("data/data_parquet/full_table_",year,".parquet"))
  
  if(year_vector[y] < 2018){
  
  full_citing_country <- lang_data %>%
    filter(!is.na(citing_lang)) %>%
    filter(!is.na(cited_lang)) %>%
    left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>%
    filter(!is.na(country_code))
    
  } else {
  
  chunk_size <- 10000000
  full_citing_country <- data.frame("citing_pub_id" = character(),
                                    "cited_pub_id" = character(),
                                    "cited_lang" = character(),
                                    "citing_lang" = character(),
                                    "year" = numeric(),
                                    "country_code" = character(),
                                    "p" = numeric())
  
  for (i in seq(1, nrow(lang_data), by = chunk_size)) {
    chunk <- lang_data[i:min(i + chunk_size - 1, nrow(lang_data)), ]
    
    print(i)
    
    chunk <- chunk %>%
      filter(!is.na(citing_lang)) %>%
      filter(!is.na(cited_lang)) %>%
      left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>%
      filter(!is.na(country_code))
    
    full_citing_country <- bind_rows(full_citing_country, chunk)
  }
  
  }
  
  country_cited_freq <- full_citing_country %>% 
    group_by(country_code,citing_lang,cited_lang) %>% 
    summarise(n = sum(p)) %>% 
    mutate(year = year_vector[y])
  
  rm(lang_data,full_citing_country)
  gc()
  
  country_freqs <- bind_rows(country_freqs,country_cited_freq)
  
}

write_parquet(country_freqs, "results/country_lang.parquet")

#####alt version####

library(data.table)
library(fs)

country_freqs <- read_parquet("results/country_lang.parquet")

year_vector <- c(2023:2024)


for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)
  
  lang_data <- read_parquet(paste0("data/data_parquet/full_table_",year,".parquet")) %>% select(-year)
  
  if(year_vector[y] < 2018){
    
    full_citing_country <- lang_data %>%
      filter(!is.na(citing_lang)) %>%
      filter(!is.na(cited_lang)) %>%
      left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>%
      filter(!is.na(country_code))
    
  } else {
    
    #extreme 
    paper_country_fractional <- read_parquet("data/paper_country_fractional.parquet") %>% mutate(country_code = case_when(country_code == "SJ" ~ "NO",                                                    country_code == "AX" ~ "FI",                                                                TRUE ~country_code))
    
    chunk_size <- 2000000
    full_citing_country <- data.frame("citing_pub_id" = character(),
                                      "cited_pub_id" = character(),
                                      "cited_lang" = character(),
                                      "citing_lang" = character(),
                                      "year" = numeric(),
                                      "country_code" = character(),
                                      "p" = numeric())
    
    for (i in seq(1, nrow(lang_data), by = chunk_size)) {
      chunk <- lang_data[i:min(i + chunk_size - 1, nrow(lang_data)), ]
      
      print(i)
      
      chunk <- chunk %>%
        filter(!is.na(citing_lang)) %>%
        filter(!is.na(cited_lang)) %>%
        left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>%
        filter(!is.na(country_code))
      
      full_citing_country <- bind_rows(full_citing_country, chunk)
      
      # Write intermediate results to disk
      write_parquet(full_citing_country, paste0("aux/full_citing_country_", year, "_", i, ".parquet"))
      full_citing_country <- data.frame("citing_pub_id" = character(),
                                        "cited_pub_id" = character(),
                                        "cited_lang" = character(),
                                        "citing_lang" = character(),
                                        "year" = numeric(),
                                        "country_code" = character(),
                                        "p" = numeric())
    }
    
    rm(lang_data)
    #extreme - try
    rm(paper_country_fractional)
    
    gc()
    
    # List all intermediate files
    file_paths <- list.files("aux", pattern = paste0("full_citing_country_", year, "_.*\\.parquet$"), full.names = TRUE)
    
    # Read all intermediate results back
    if (length(file_paths) > 0) {
      full_citing_country <- open_dataset(file_paths) %>% collect()
    } else {
      full_citing_country <- tibble()  # Return an empty tibble if no files found
    }
    
  }
  
  country_cited_freq <- full_citing_country %>% 
    group_by(country_code,citing_lang,cited_lang) %>% 
    summarise(n = sum(p)) %>% 
    mutate(year = year_vector[y])
  
  rm(full_citing_country)
  gc()
  
  country_freqs <- bind_rows(country_freqs,country_cited_freq)
  
}

write_parquet(country_freqs, "results/country_lang.parquet")
