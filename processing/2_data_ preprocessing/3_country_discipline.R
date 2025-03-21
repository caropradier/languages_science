library(tidyverse)
library(readr)
library(rvest)
library(arrow)
library(viridis)
library(countrycode)
library(sf)
library(data.table)
library(fs)
options(scipen = 9999)

######loops#######

meta_table <- read_parquet("data/meta_table.parquet") %>% 
  filter(!is.na(for_division_id))

paper_country_fractional <- read_parquet("data/paper_country_fractional.parquet") %>% mutate(country_code = case_when(country_code == "SJ" ~ "NO",                                                    country_code == "AX" ~ "FI",                                                                TRUE ~country_code))


# disc_country_all_pubs <-  data.frame("lang" = character(),
#                                      "for_division_id" = integer(),
#                                      "country_code" = character(),
#                               "n"=numeric(),
#                               "year" = numeric())

disc_country_all_pubs <- read_parquet("results/all_country_disc.parquet")

year_vector <- c(2023:2023)

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
  left_join(meta_table, by = c("pub_id" = "Pub_ID")) %>% 
  filter(!is.na(for_division_id)) %>% 
  left_join(paper_country_fractional, by = c("pub_id" = "Pub_ID")) %>% 
  filter(!is.na(country_code))

full_freq <- all_data %>% 
  group_by(lang,for_division_id,country_code) %>% 
  summarise(n = sum(p)) %>% 
  mutate(year = year_vector[y])

disc_country_all_pubs  <- bind_rows(disc_country_all_pubs,full_freq)

rm(all_data)
gc()

}

write_parquet(disc_country_all_pubs, "results/all_country_disc.parquet")



# disc_country_freqs_citing <- data.frame("country_code" = character(),
#                                         "for_division_id" = integer(),
#                             "citing_lang" = character(),
#                             "n"=numeric(),
#                             "year" = numeric())

#year_vector <- c(1990:2023)

disc_country_freqs_citing <- read_parquet("results/citing_country_disc.parquet")

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
  
  citing_disc_country <- citing_lang %>% 
    filter(!is.na(citing_lang)) %>% 
    left_join(meta_table, by = c("citing_pub_id" = "Pub_ID")) %>% 
    filter(!is.na(for_division_id)) %>% 
    left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>% 
    filter(!is.na(country_code))
  
  country_citing_freq <- citing_disc_country %>% 
    group_by(country_code,for_division_id,citing_lang) %>% 
    summarise(n = sum(p))%>% 
    mutate(year = year_vector[y])
  
  
  disc_country_freqs_citing <- bind_rows(disc_country_freqs_citing,country_citing_freq)
  
}


write_parquet(disc_country_freqs_citing, "results/citing_country_disc.parquet")

# disc_country_freqs <- data.frame("country_code" = character(),
#                                  "for_division_id" = integer(),
#                             "citing_lang" = character(),
#                             "cited_lang" = character(),
#                             "n"=numeric(),
#                             "year" = numeric())

#year_vector <- c(1990:2023)

disc_country_freqs <- read_parquet("results/country_disc.parquet")

year_vector <- c(2016)
#crashed :(

for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)
  
  lang_data <- read_parquet(paste0("data/data_parquet/full_table_",year,".parquet")) %>% 
    select(-year) %>% 
    filter(!is.na(citing_lang)) %>%
    filter(!is.na(cited_lang)) 

  
  if(year_vector[y] < 2016){
    
    lang_data <- lang_data%>% 
      left_join(meta_table, by = c("citing_pub_id" = "Pub_ID")) %>% 
      filter(!is.na(for_division_id))
    
    full_citing_country <- lang_data %>%
      left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>%
      filter(!is.na(country_code))
    
  } else {
    
    chunk_size <- 10000000
    
    full_citing_country <- data.frame("citing_pub_id" = character(),
                                      "cited_pub_id" = character(),
                                      "cited_lang" = character(),
                                      "citing_lang" = character(),
                                      "for_division_id" = integer(),
                                      "country_code" = character(),
                                      "p" = numeric(),
                                      "year" = numeric())
    
    for (i in seq(1, nrow(lang_data), by = chunk_size)) {
      chunk <- lang_data[i:min(i + chunk_size - 1, nrow(lang_data)), ]
      
      print(i)
      
      chunk <- chunk %>%
        left_join(meta_table, by = c("citing_pub_id" = "Pub_ID")) %>% 
        filter(!is.na(for_division_id)) %>% 
        left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>%
        filter(!is.na(country_code))
      
      full_citing_country <- bind_rows(full_citing_country, chunk)
    }
    
  }
  
  country_cited_freq <- full_citing_country %>% 
    group_by(country_code,for_division_id,citing_lang,cited_lang) %>% 
    summarise(n = sum(p)) %>% 
    mutate(year = year_vector[y])
  
  rm(lang_data,full_citing_country)
  
  disc_country_freqs <- bind_rows(disc_country_freqs,country_cited_freq)
  
}

write_parquet(disc_country_freqs, "results/country_disc.parquet")

#####alt version####

disc_country_freqs <- read_parquet("results/country_disc.parquet")

year_vector <- c(2022:2023)


for (y in seq(length(year_vector))) {
  year <- as.character(year_vector[y])
  
  print(year)
  
  #extreme 
  meta_table <- read_parquet("data/meta_table.parquet") %>% 
    filter(!is.na(for_division_id))
  
  paper_country_fractional <- read_parquet("data/paper_country_fractional.parquet") %>% mutate(country_code = case_when(country_code == "SJ" ~ "NO",                                                    country_code == "AX" ~ "FI",                                                                TRUE ~country_code))
  
  
  lang_data <- read_parquet(paste0("data/data_parquet/full_table_",year,".parquet")) %>% 
    select(-year) %>% 
    filter(!is.na(citing_lang)) %>%
    filter(!is.na(cited_lang)) 
  
  
  if(year_vector[y] < 2016){
    
    lang_data <- lang_data%>% 
      left_join(meta_table, by = c("citing_pub_id" = "Pub_ID")) %>% 
      filter(!is.na(for_division_id))
    
    full_citing_country <- lang_data %>%
      left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>%
      filter(!is.na(country_code))
    
  } else {
    
    chunk_size <- 1000000
    
    full_citing_country <- data.frame("citing_pub_id" = character(),
                                      "cited_pub_id" = character(),
                                      "cited_lang" = character(),
                                      "citing_lang" = character(),
                                      "for_division_id" = integer(),
                                      "country_code" = character(),
                                      "p" = numeric(),
                                      "year" = numeric())
    
    for (i in seq(1, nrow(lang_data), by = chunk_size)) {
      chunk <- lang_data[i:min(i + chunk_size - 1, nrow(lang_data)), ]
      
      print(i)
      
      chunk <- chunk %>%
        left_join(meta_table, by = c("citing_pub_id" = "Pub_ID")) %>% 
        filter(!is.na(for_division_id)) %>% 
        left_join(paper_country_fractional, by = c("citing_pub_id" = "Pub_ID")) %>%
        filter(!is.na(country_code))
      
      full_citing_country <- bind_rows(full_citing_country, chunk)
      
      write_parquet(full_citing_country, paste0("aux/full_citing_country_", year, "_", i, ".parquet"))
      
      full_citing_country <- data.frame("citing_pub_id" = character(),
                                        "cited_pub_id" = character(),
                                        "cited_lang" = character(),
                                        "citing_lang" = character(),
                                        "for_division_id" = integer(),
                                        "country_code" = character(),
                                        "p" = numeric(),
                                        "year" = numeric())
      
    }
    
    rm(lang_data)
    #extreme
    rm(meta_table,paper_country_fractional)
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
    group_by(country_code,for_division_id,citing_lang,cited_lang) %>% 
    summarise(n = sum(p)) %>% 
    mutate(year = year_vector[y])
  
  rm(full_citing_country)
  gc()
  
  disc_country_freqs <- bind_rows(disc_country_freqs,country_cited_freq)
  
}

write_parquet(disc_country_freqs, "results/country_disc.parquet")
