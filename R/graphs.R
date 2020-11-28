library(waffle)
library(emojifont)
parliament_plot <- function(seats, colours){
  waffle(tibble::deframe(seats), rows = 20, colors = colours)
  
}

float_seat_plot <- function(seats, colours){
  waffle(tibble::deframe(seats), rows = 2, colors = colours)
}

