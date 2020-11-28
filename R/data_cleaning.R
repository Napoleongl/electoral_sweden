library(dplyr)
library(magrittr)
library(readr)
library(tidyr)

source("R/functions.R")

datapath = "../input/valresultat/"
year = 2018L
valid_parties = c('C', 'KD', 'L', 'M', 'MP', 'S', 'SD', 'V')
party_colours <- setNames(c('#009933', '#231977', '#6BB7EC', '#1B49DD', '#83CF39', '#EE2020', '#DDDD00', '#AF0000'), 
                          valid_parties)
valid_parties = c('V', 'MP', 'S', 'C', 'L', 'M', 'KD', 'SD')
party_colours <- setNames(c('#AF0000','#83CF39', '#EE2020', '#009933', '#6BB7EC', '#1B49DD', '#231977',  '#EE2020'), 
                          valid_parties)
block_colours <- c(party_colours, "GAL" = "#22ccd8", "TAN" = "#010777", "Röd-Gröna" = "red", "Alliansen"  = "blue")

summary_level = 'VALKRETSKOD'
float_seats = 39L
fixed_seats = read_csv(paste0(datapath, 'fasta_mandat_',summary_level, '_', year, '.csv'),
                          col_types = list(col_character(), col_integer()), 
                          progress = FALSE)

rawdata <- readxl::read_xlsx(path = paste0(datapath, year, '_R_per_valdistrikt.xlsx'),
                            sheet = 'R antal')

districts  <- rawdata[, 1:8] %>% inner_join(fixed_seats, by = c('VALKRETSNAMN'='Valkrets'))
votes      <- rawdata[, c('LÄNSKOD', 'KOMMUNKOD', 'VALKRETSKOD', 'VALDISTRIKTSKOD', valid_parties)] %>% 
  pivot_longer(cols = all_of(valid_parties),names_to = "party", values_to = "votes")

party_mapping <- tibble(party = valid_parties, 
                        raw = valid_parties, 
                        galtan = c(rep("GAL", 5), rep("TAN",3)),
                        val2014 =c(rep("Röd-Gröna", 3), rep("Alliansen", 4), "SD")
                        )

selected_mapping = 'val2014'
party_to_block <- party_mapping %>%  
  select(party,  !!selected_mapping) %>% 
  rename(block = !!selected_mapping)

# Tally votes according to chosen level
votes_by_level <- votes %>% 
  inner_join(party_to_block ) %>% 
  group_by(.dots=c(summary_level, "block")) %>% 
  select(!!summary_level, block, votes) %>% 
  summarise(votes = sum(votes, na.rm=TRUE) ) %>% # Votes per block and level
  group_by(.dots=c(summary_level)) %>% 
  summarise(wvotes = max(votes), block = block[which(votes == wvotes)]) # Winner per level after summary

fixed_seats_assigned <- districts %>%  
  group_by(VALKRETSKOD) %>% 
  summarise(seats =  max(seats)) %>%
  inner_join(votes_by_level) %>% 
  group_by(block) %>%
  summarise(fixed_seats = sum(seats))

# Calculate 'utjämningsmandat' on national level
votes_total <- votes %>% 
  select(party, votes) %>%
  inner_join(party_to_block) %>% 
  group_by(block) %>% 
  summarise(votes = sum(votes, na.rm=TRUE) )

float_seats_assigned <- sainte_lague_seat_dist(votes_total, float_seats)

# Join float and fixed seats
total_seats_assigned <- fixed_seats_assigned %>% 
  full_join(float_seats_assigned, by = "block") %>% 
  group_by(block) %>% 
  summarize(seats = sum(fixed_seats, float_seats, na.rm = TRUE))

total_seats <- sum(total_seats_assigned$seats)

parliament_plot(total_seats_assigned, block_colours)

float_seat_plot(float_seats, block_colours)

# TODO:
# Gruppera polygoner per summary level
# Plotta Fördelnign av platser
