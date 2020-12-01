library(dplyr)
library(magrittr)
library(readr)
library(tidyr)
library(patchwork)
library(sf)

source("R/functions.R")
source("R/graphs.R")

datapath = "../input/valresultat/"
map_path <- "../input/sweden-maps/"
year = 2018L
float_seats = 39L
seat_rows = 18
summary_level = summary_levels[3]
summary_levels <- c("KOMMUNNAMN", "LÄNSNAMN", "VALKRETSNAMN")

valid_parties = c('V', 'MP', 'S', 'C', 'L', 'M', 'KD', 'SD')
party_colours <- setNames(c('#AF0000','#83CF39', '#EE2020', '#009933', 
                            '#6BB7EC', '#1B49DD', '#231977',  '#DDDD00'),
                          valid_parties)

party_mapping <- tibble(party = valid_parties, 
                        raw = valid_parties, 
                        galtan = c(rep("GAL", 5), 
                                   rep("TAN",3)),
                        val2014 =c(rep("Röd-Gröna", 3),
                                   rep("Alliansen", 4),
                                   "SD")
                        )
block_colours <- list(raw     = party_colours,
                      galtan  = c("GAL" = "#22ccd8", 
                                  "TAN" = "#010777"),
                      val2014 = c("Röd-Gröna" = "#EE2020", 
                                  "Alliansen"  = "lightblue", 
                                  party_colours["SD"])
                      )

available_mappings <- names(block_colours)
selected_mapping = available_mappings[3]

fixed_seats = read_csv(paste0(datapath, 'fasta_mandat_',summary_level, '_', year, '.csv'),
                          col_types = list(col_character(), col_integer()), 
                          progress = FALSE)

rawdata <- readxl::read_xlsx(path = paste0(datapath, year, '_R_per_valdistrikt.xlsx'),
                            sheet = 'R antal')

districts  <- rawdata[, 1:8] %>% inner_join(fixed_seats)
votes      <- rawdata[, c('LÄNSNAMN', 'KOMMUNNAMN', 'VALKRETSNAMN', 'VALDISTRIKTSKOD', valid_parties)] %>% 
  pivot_longer(cols = all_of(valid_parties),names_to = "party", values_to = "votes")

swe_map_data <- read_sf(paste0(map_path, "gadm36_SWE_shp/", "gadm36_SWE_2L.shp")) %>% 
  mutate(NAME_2 = replace(NAME_2, NAME_2=="Malung", "Malung-Sälen")) %>% 
  mutate(NAME_2 = replace(NAME_2, NAME_2=="Upplands-Väsby", "Upplands Väsby")) %>% 
  mutate(kommun_join = tolower(NAME_2)) %>% 
  inner_join(districts %>% 
               group_by(KOMMUNNAMN) %>% 
               select(!!summary_levels) %>% 
               unique() %>% 
               mutate(kommun_join = tolower(KOMMUNNAMN))
  ) 



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
  group_by(.dots=c(summary_level)) %>% 
  summarise(seats =  max(seats)) %>%
  inner_join(votes_by_level) %>% 
  group_by(block) %>%
  summarise(seats = sum(seats))

# Calculate 'utjämningsmandat' on national level
votes_total <- votes %>% 
  select(party, votes) %>%
  inner_join(party_to_block) %>% 
  group_by(block) %>% 
  summarise(votes = sum(votes, na.rm=TRUE) )

float_seats_assigned <- sainte_lague_seat_dist(votes_total, float_seats) %>% 
  arrange(match(block, names(block_colours[[selected_mapping]]))) # Sorting necessary on data since waffle-plots don't look up colours by name.

# Join float and fixed seats
total_seats_assigned <- fixed_seats_assigned %>% 
  union(float_seats_assigned, by = "block") %>% 
  group_by(block) %>% 
  summarize(seats = sum(seats, na.rm = TRUE)) %>%
  arrange(match(block, names(block_colours[[selected_mapping]])))

total_seats <- sum(total_seats_assigned$seats)

votes_map_data <- swe_map_data %>% 
  group_by(.dots=c(summary_level)) %>% 
  summarise() %>% 
  inner_join(votes_by_level)

votes_map(votes_map_data, block_colours[[selected_mapping]]) +
 
parliament_plot(total_seats_assigned, block_colours[[selected_mapping]], seat_rows) +

plot_layout(nrow = 1, ncol = 2)
  
# manual version of waffles, displays fontawesome but varies in width
#parliament_plot2(fixed_seats_assigned, block_colours[[selected_mapping]], seat_rows) + theme(legend.position =  "none")+
#parliament_plot2(float_seats_assigned, block_colours[[selected_mapping]], ceiling(seat_rows/3)) +