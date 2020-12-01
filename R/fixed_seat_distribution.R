# Calculation of seat distribution for kommun and län

seats_to_assign <- 310
year <- 2018L
source("R/functions.R")
datapath = "../input/valresultat/"

rawdata <- readxl::read_xlsx(path = paste0(datapath, year, '_R_per_valdistrikt.xlsx'),
                             sheet = 'R antal')

rawdata %>% select(KOMMUNNAMN, RÖSTBERÄTTIGADE) %>% 
  group_by(KOMMUNNAMN) %>% 
  summarise(votes = sum(RÖSTBERÄTTIGADE)) %>% 
  sainte_lague_seat_dist(seats_to_assign, min_seats = 1) %>% 
  write_csv(paste0(datapath, 'fasta_mandat_KOMMUNNAMN_', year, '.csv'))

rawdata %>% select(LÄNSNAMN, RÖSTBERÄTTIGADE) %>% 
  group_by(LÄNSNAMN) %>% 
  summarise(votes = sum(RÖSTBERÄTTIGADE)) %>% 
  sainte_lague_seat_dist(seats_to_assign, min_seats = 2) %>% 
  write_csv(paste0(datapath, 'fasta_mandat_LÄNSNAMN_', year, '.csv'))


