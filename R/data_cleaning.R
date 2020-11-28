library(dplyr)
library(magrittr)
library(readr)
library(tidyr)

source("R/functions.R")

datapath = "../input/valresultat/"
year = 2018L
valid_parties = c('C', 'KD', 'L', 'M', 'MP', 'S', 'SD', 'V')
summary_level = 'VALKRETSKOD'
float_seats = 39L
fixed_seats = read_csv(paste0(datapath, 'fasta_mandat_',summary_level, '_', year, '.csv'),
                          col_types = list(col_character(), col_integer()), 
                          progress = FALSE)

rawdata = readxl::read_xlsx(path = paste0(datapath, year, '_R_per_valdistrikt.xlsx'),
                            sheet = 'R antal')

districts  = rawdata[, 1:8] %>% inner_join(fixed_seats, by = c('VALKRETSNAMN'='Valkrets'))
votes      = rawdata[, c('LÄNSKOD', 'KOMMUNKOD', 'VALKRETSKOD', 'VALDISTRIKTSKOD', valid_parties)] %>% 
  pivot_longer(cols = all_of(valid_parties),names_to = "name", values_to = "votes")

# Tally votes according to chosen level
votes_by_level <- votes %>% 
  group_by(.dots=c(summary_level, "name")) %>% 
  select(!!summary_level, name, votes) %>% 
  summarise(votes = sum(votes, na.rm=TRUE) ) %>% # Votes per block and level
  group_by(.dots=c(summary_level)) %>% 
  summarise(wvotes = max(votes), name = name[which(votes == wvotes)]) # Winner per level after summary

fixed_seats_assigned <- districts %>%  
  group_by(VALKRETSKOD) %>% 
  summarise(seats =  max(seats)) %>%
  inner_join(votes_by_level) %>% 
  group_by(name) %>%
  summarise(fixed_seats = sum(seats))

# Calculate 'utjämningsmandat' on national level
votes_total <- votes %>% 
  select(name, votes) %>% 
  group_by(name) %>% 
  summarise(votes = sum(votes, na.rm=TRUE) )

float_seats_assigned <- sainte_lague_seat_dist(votes_total, float_seats)

# Join float and fixed seats
total_seats_assigned <- fixed_seats_assigned %>% 
  full_join(float_seats_assigned, by = "name") %>% 
  group_by(name) %>% 
  summarize(seats = sum(fixed_seats, float_seats, na.rm = TRUE))

total_seats <- sum(total_seats$seats)

# TODO:
# Gruppera polygoner per summary level
