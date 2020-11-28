sainte_lague_seat_dist <- function(vote_df, seats_to_assign, init_div = 1.2 ){
  divisors <- seq(0L, seats_to_assign) * 2 + 3
  vote_df %<>% mutate(quotient = votes/init_div, float_seats = 0)
  while(seats_to_assign > 0){
    # For each seat, find the current highest quotient 
    max_quotient_idx <- which(vote_df$quotient == max(vote_df$quotient))
    if(length(max_quotient_idx) > 1) {
      max_quotient_idx <- sample(max_quotient_idx, 1)
      }
    # Add seat to obs with highest quotient
    vote_df[max_quotient_idx, "float_seats"] <- vote_df[max_quotient_idx, "float_seats"] + 1
    # And recalc quotient by dividing original votes by next divisor
    vote_df[max_quotient_idx, "quotient"] <- vote_df[max_quotient_idx, "votes"]/divisors[unlist(vote_df[max_quotient_idx, "float_seats"])]
    seats_to_assign <- seats_to_assign - 1
  }
  vote_df %>% select(-votes, -quotient)
}

# tests:
# sainte_lague_seat_dist(tibble(name = LETTERS[1:3], votes = c(1200, 900, 10)), 7)# = 4,3,0
# sainte_lague_seat_dist(tibble(name = LETTERS[1:3], votes = c(1200, 20, 10)), 7)# = 7,0,0
# set.seed(1)
# sainte_lague_seat_dist(tibble(name = LETTERS[1:3], votes = c(1200, 1200, 10)), 7)# = 4,3,0
# set.seed(2)
# sainte_lague_seat_dist(tibble(name = LETTERS[1:3], votes = c(1200, 1200, 10)), 7)# = 3,4,0