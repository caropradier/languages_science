library(tidyverse)
library(readr)
library(arrow)
library(readxl)


#####meta table#####

meta_table <- data.frame("Pub_ID" = character(),
                         "for_division_id" = integer())

year_vector <- c(1990:2023)

for (y in seq(length(year_vector))) {
  
  print(year_vector[y])
  
  year <- read_delim(paste0("data/discipline_",year_vector[y],".csv"),
                     na = c("", "NULL")) 
  
  meta_table <- bind_rows(meta_table,year)
  
}

meta_table <- meta_table %>% 
  distinct(Pub_ID,.keep_al = TRUE)

#write_parquet("data/meta_table.parquet")

meta_table <- read_parquet("data/meta_table.parquet") %>% 
  filter(!is.na(for_division_id))

#####all pubs######

disc_all_pubs <-  data.frame("lang" = character(),
                              "for_division_id" = integer(),
                              "n"=numeric(),
                              "year" = numeric())

disc_all_pubs <- read_parquet("results/all_disc.parquet")

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
  
  all_data <- all_lang %>% 
    left_join(meta_table, by = c("pub_id" = "Pub_ID")) %>% 
    filter(!is.na(for_division_id)) 
  
  full_freq <- all_data %>% 
    group_by(lang,for_division_id) %>% 
    summarise(n = n_distinct(pub_id)) %>% 
    mutate(year = year_vector[y])
  
  disc_all_pubs  <- bind_rows(disc_all_pubs,full_freq)
  
  rm(all_data)
  gc()
  
}

write_parquet(disc_all_pubs, "results/all_disc.parquet")

######citation pairs#######

# meta_freqs_citing <- data.frame("for_division_id" = integer(),
#                          "citing_lang" = character(),
#                          "n"=numeric(),
#                          "year" = numeric())
# 
# meta_freqs <- data.frame("for_division_id" = integer(),
#                             "citing_lang" = character(),
#                             "cited_lang" = character(),
#                             "n"=numeric(),
#                             "year" = numeric())

meta_freqs <- read_parquet("results/discipline_lang.parquet")
meta_freqs_citing <- read_parquet("results/citing_discipline_lang.parquet")

#year_vector <- c(1990:2023)
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
if(year_vector[y] < 2022){

citing_meta <- citing_lang %>% 
  filter(!is.na(citing_lang)) %>% 
  left_join(meta_table, by = c("citing_pub_id" = "Pub_ID")) %>% 
  filter(!is.na(for_division_id))

} else {
  
  chunk_size <- 10000000
  citing_meta <- data.frame("citing_pub_id" = character(),
                            "citing_lang" = character(),
                            "for_division_id" = integer())
  
  for (i in seq(1, nrow(citing_lang), by = chunk_size)) {
    chunk <- citing_lang[i:min(i + chunk_size - 1, nrow(citing_lang)), ]
    
    print(i)
    
    chunk <- chunk %>%
      filter(!is.na(citing_lang)) %>% 
      left_join(meta_table, by = c("citing_pub_id" = "Pub_ID")) %>% 
      filter(!is.na(for_division_id))
    
    citing_meta <- bind_rows(citing_meta, chunk)
  }
  
}

meta_citing_freq <- citing_meta %>% 
  group_by(for_division_id,citing_lang) %>% 
  summarise(n = n_distinct(citing_pub_id))%>% 
  mutate(year = year_vector[y])

meta_freqs_citing <- bind_rows(meta_freqs_citing,meta_citing_freq)

rm(citing_lang,citing_meta)  

lang_data <- read_parquet(paste0("data/data_parquet/full_table_",year,".parquet")) %>% 
  select(-year)

if(year_vector[y] < 2022){

full_meta <- lang_data %>% 
  filter(!is.na(citing_lang)) %>% 
  filter(!is.na(cited_lang)) %>% 
  left_join(meta_table, by = c("citing_pub_id" = "Pub_ID"))%>% 
  filter(!is.na(for_division_id))

} else {
  
  chunk_size <- 10000000
  full_meta <- data.frame("citing_pub_id" = character(),
                          "cited_pub_id" = character(),
                            "cited_lang" = character(),
                          "citing_lang" = character(),
                            "for_division_id" = integer())
  
  for (i in seq(1, nrow(lang_data), by = chunk_size)) {
    chunk <- lang_data[i:min(i + chunk_size - 1, nrow(lang_data)), ]
    
    print(i)
    
    chunk <- chunk %>% 
      filter(!is.na(citing_lang)) %>% 
      filter(!is.na(cited_lang)) %>% 
      left_join(meta_table, by = c("citing_pub_id" = "Pub_ID"))%>% 
      filter(!is.na(for_division_id))
    
    full_meta <- bind_rows(full_meta, chunk)
  }
  
}

meta_cited_freq <- full_meta %>% 
  group_by(for_division_id,citing_lang,cited_lang) %>% 
  summarise(n = n_distinct(citing_pub_id,cited_pub_id)) %>% 
  mutate(year = year_vector[y])

rm(lang_data,full_meta)

meta_freqs <- bind_rows(meta_freqs,meta_cited_freq)

}


write_parquet(meta_freqs, "results/discipline_lang.parquet")
write_parquet(meta_freqs_citing, "results/citing_discipline_lang.parquet")
