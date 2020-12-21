
library(shiny)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Elektorsröster i Sverige"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "summary_level",
                  label = "Välj hur rösterna fördelas",
                  choices = c("VALKRETSNAMN","LÄNSNAMN", "KOMMUNNAMN")),
      
      # Input: Numeric entry for number of obs to view ----
      selectInput(inputId = "selected_mapping",
                  label = "Välj partigruppering",
                  choices = c("Ingen", "GalTan", "Val2014"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      plotlyOutput(outputId = "seats_chart")
      
      
      
    )
  )
)
server <- function(input, output) {
  # ====================== PREWORK ==========================
  library(dplyr)
  library(magrittr)
  library(readr)
  library(tidyr)
  library(sf)
  
  source("R/functions.R")
  source("R/graphs.R")
  
  datapath = "data/valresultat/"
  map_path <- "data/sweden-maps/"
  year = 2018L
  float_seats = 39L
  seat_rows = 16
  
  valid_parties <- c('V', 'MP', 'S', 'C', 'L', 'M', 'KD', 'SD')
  
  party_colours <- setNames(c('#AF0000','#83CF39', '#EE2020', '#009933', 
                              '#6BB7EC', '#1B49DD', '#231977',  '#DDDD00'),
                            valid_parties)
  
  party_mapping <- tibble(party = valid_parties, 
                          Ingen = valid_parties, 
                          GalTan = c(rep("GAL", 5), 
                                     rep("TAN",3)),
                          Val2014 =c(rep("Röd-Gröna", 3),
                                     rep("Alliansen", 4),
                                     "SD")
  )
  block_colours <- list(Ingen = party_colours,
                        GalTan   = c("GAL" = "#22ccd8", 
                                     "TAN" = "#010777"),
                        Val2014  = c("Röd-Gröna" = "#EE2020", 
                                     "Alliansen"  = "lightblue", 
                                     party_colours["SD"])
  )
  
  rawdata <- readxl::read_xlsx(path = paste0(datapath, year, '_R_per_valdistrikt.xlsx'),
                               sheet = 'R antal')
  
  votes      <- rawdata[, c('LÄNSNAMN', 'KOMMUNNAMN', 'VALKRETSNAMN', 'VALDISTRIKTSKOD', valid_parties)] %>% 
    pivot_longer(cols = all_of(valid_parties),names_to = "party", values_to = "votes")
  
  raw_map_data <- read_sf(paste0(map_path, "gadm36_SWE_shp/", "gadm36_SWE_2LR.shp")) %>% 
    mutate(NAME_2 = replace(NAME_2, NAME_2=="Malung", "Malung-Sälen")) %>% 
    mutate(NAME_2 = replace(NAME_2, NAME_2=="Upplands-Väsby", "Upplands Väsby")) %>% 
    mutate(kommun_join = tolower(NAME_2))
  
  
  summary_levels <- c("KOMMUNNAMN", "LÄNSNAMN", "VALKRETSNAMN")
  available_mappings <- names(block_colours)
  # ====================== SETTINGS =========================
  #summary_level <- summary_levels[3]
  #selected_mapping <- available_mappings[1]
  
  # ====================== FUNCTION DEFINITION ==============
  seats_chart <- function(summary_level, selected_mapping){
    # ====================== DATA WRANGLING ===================
    fixed_seats <- read_csv(paste0(datapath, 'fasta_mandat_',summary_level, '_', year, '.csv'),
                            col_types = list(col_character(), col_integer()), 
                            progress = FALSE)
    
    districts  <- rawdata[, 1:8] %>% inner_join(fixed_seats)
    
    
    swe_map_data <- raw_map_data %>% 
      inner_join(districts %>% 
                   group_by(KOMMUNNAMN) %>% 
                   select(!!summary_levels, seats) %>% 
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
      summarise(seats =  max(seats)) %>% #using max since the number of sets was precalced and joined by summary_level already
      inner_join(votes_by_level) %>% 
      group_by(block) %>%
      summarise(fixed_seats = sum(seats))
    
    # Calculate 'utjämningsmandat' on national level
    votes_total <- votes %>% 
      select(party, votes) %>%
      inner_join(party_to_block) %>% 
      group_by(block) %>% 
      summarise(votes = sum(votes, na.rm=TRUE) )
    
    float_seats_assigned <- sainte_lague_seat_dist(votes_total, float_seats) %>% 
      arrange(match(block, names(block_colours[[selected_mapping]]))) %>% # Sorting necessary on data since waffle-plots don't look up colours by name.
      rename(float_seats = seats)
    
    # Join float and fixed seats and turn into df of single seats
    total_seats_assigned <- fixed_seats_assigned %>% 
      full_join(float_seats_assigned, by = "block") %>%
      replace_na(list(fixed_seats = 0, float_seats = 0)) %>% 
      mutate(total_seats = fixed_seats + float_seats) %>% 
      arrange(match(block, names(block_colours[[selected_mapping]])))  
    
    total_seats <- sum(total_seats_assigned$total_seats)
    
    votes_map_data <- swe_map_data %>% 
      group_by(.dots=c(summary_level)) %>% 
      summarise(seats = max(seats)) %>% 
      inner_join(votes_by_level) %>% 
      st_cast("MULTIPOLYGON") %>% 
      rename(area = !!summary_level)  
    
    # ====================== GRAPHING =========================
    subplot(
      votes_map_data %>% 
        plotly_votes_map(block_colours[[selected_mapping]]) ,
      total_seats_assigned %>% 
        plotly_parliament_plot("block", c("fixed_seats", "float_seats") , seat_rows,
                               block_colours[[selected_mapping]]),
      widths = c(0.4,0.6)) %>% 
      layout(title = list(text = paste0("Mandatfördelning","\nPartigruppering: ", selected_mapping, 
                                        ", Röstfördelning: ", summary_level)))
  }
  
output$seats_chart <- renderPlotly(seats_chart(summary_level = input$summary_level,
                                               selected_mapping = input$selected_mapping))
}


shinyApp(ui, server)
