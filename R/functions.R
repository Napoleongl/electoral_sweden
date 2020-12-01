sainte_lague_seat_dist <- function(.data, seats_to_assign, init_div = 1.2, min_seats = 0L){
  divisors <- seq(0L, seats_to_assign) * 2 + 3
  .data %<>% mutate(quotient = votes/init_div, seats = min_seats)
  seats_to_assign <- seats_to_assign - min_seats * nrow(.data)
  while(seats_to_assign > 0){
    # For each seat, find the current highest quotient 
    max_quotient_idx <- which(.data$quotient == max(.data$quotient))
    if(length(max_quotient_idx) > 1) {
      max_quotient_idx <- sample(max_quotient_idx, 1)
      }
    # Add seat to obs with highest quotient
    .data[max_quotient_idx, "seats"] <- .data[max_quotient_idx, "seats"] + 1
    # And recalc quotient by dividing original votes by next divisor
    .data[max_quotient_idx, "quotient"] <- .data[max_quotient_idx, "votes"]/divisors[unlist(.data[max_quotient_idx, "seats"])-min_seats]
    seats_to_assign <- seats_to_assign - 1
  }
  .data %>% select(-votes, -quotient)
}

# tests:
# sainte_lague_seat_dist(tibble(name = LETTERS[1:3], votes = c(1200, 900, 10)), 7)# = 4,3,0
# sainte_lague_seat_dist(tibble(name = LETTERS[1:3], votes = c(1200, 20, 10)), 7)# = 7,0,0
# set.seed(1)
# sainte_lague_seat_dist(tibble(name = LETTERS[1:3], votes = c(1200, 1200, 10)), 7)# = 4,3,0
# set.seed(2)
# sainte_lague_seat_dist(tibble(name = LETTERS[1:3], votes = c(1200, 1200, 10)), 7)# = 3,4,0