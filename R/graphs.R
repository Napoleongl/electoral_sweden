library(waffle)
library(emojifont)
parliament_plot <- function(seats, colours, rows){
  waffle(tibble::deframe(seats), rows = rows, colors = colours, legend_pos = "none")
}

float_seat_plot <- function(seats, colours, rows){
  waffle(tibble::deframe(seats), rows = rows, colors = colours)+ 
    ggtitle("Float seats assigned")
}


seats <- tibble(block = valid_parties[4:7], seats = c(5,3,6,7))

parliament_plot2 <- function(seats, colours, ncol){
  seat_seqs <- sapply(seats$seats, function(x){
    seq.int(1,x)
    })
  seat_xy <- lapply(seat_seqs, function(x){
    tibble(X = ((x-1)%%ncol)+1,
           Y = ((x-1)%/%ncol)+1)
    })

  row_max <- 0
  for(main_id in seq(1L,length(seat_xy))){
    seat_xy[[main_id]]$Y <- seat_xy[[main_id]]$Y + row_max
    seat_xy[[main_id]]$block <- seats$block[main_id]
    row_max <- max(seat_xy[[main_id]]$Y)
  }
  ggg<-bind_rows(seat_xy)
  ggg$Y <- max(ggg$Y) - ggg$Y
  ggplot(ggg) + theme_void() +
    aes(x=X, y=Y, colour = block) + 
    geom_tile(size = 3, fill = "#FFFFFF", colour = "#FFFFFF") +
    geom_text(label = fontawesome("fa-user"), family = 'fontawesome-webfont')+
    scale_colour_manual(values = colours)  
}

votes_map <- function(.data, fills){
  ggplot(.data) + 
    aes(fill = block) +
    geom_sf(colour = "grey10", size = 0.1) + 
    theme_void() +
    theme(legend.position = "left") +
    scale_fill_manual(values = fills, drop = FALSE)}



