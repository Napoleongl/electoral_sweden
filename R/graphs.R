library(waffle)
#library(emojifont)
library(plotly)
parliament_plot <- function(seats, colours, rows){
  waffle(tibble::deframe(seats), rows = rows, colors = colours)
}

float_seat_plot <- function(seats, colours, rows){
  waffle(tibble::deframe(seats), rows = rows, colors = colours)+ 
    ggtitle("Float seats assigned")
}


#seats <- tibble(block = valid_parties[4:7], seats = c(5,3,6,7))

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
    theme(legend.position = "none") +
    scale_fill_manual(values = fills, drop = FALSE)}


plotly_votes_map <- function(.data, fills){
  plot_ly(.data, color =  ~block, stroke = I("black"), split=~area, 
          height = 550, width = 500, span = I(0.1), colors = fills, 
          text = ~paste("<b>", area, ":</b><br>", block, "vinner", seats, "mandat."), 
          hoveron = "fills", hoverinfo = "text", showlegend = FALSE)
}

plotly_parliament_plot <- function(.data, group_col, type_cols, nrows = as.integer(sqrt(nrow(.data)))+1, colours = NULL){
  if(is.null(colours)){ 
    colours <-RColorBrewer::brewer.pal(length(unique(.data[[group_col]])), "Set2") 
  }
  
  noax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  .data %>% 
    pivot_longer(cols = {{type_cols}}, 
                 names_to = "type", values_to = "values") %>% 
    group_by(across(group_col)) %>% 
    summarise(values = sum(values))-> group_sums
  
  group_text <- paste0(group_sums[[group_col]],": ", group_sums$values)
  
  .data %>%   
    pivot_longer(cols = {{type_cols}}, 
                 names_to = "type", values_to = "values") %>% 
    uncount(values) %>% 
    select(!!group_col, type)%>% 
    mutate(rowid = ((row_number()-1) %/% nrows) +1,
           colid = ((row_number()-1) %% nrows) +1) %>% 
    plot_ly(height = 450, width = 600, 
            hovertemplate = " ") %>%  
    add_markers(type = "scatter", x =~colid, y = ~rowid, 
                color = ~block, colors = colours,
                symbol = ~type, symbols =c("square", "cross"), 
                size = 1, showlegend = FALSE) %>% 
    layout(xaxis = noax, yaxis= noax,
           annotations = 
             list(x = 1, y = -0.05, text = paste(group_text, collapse = ", "), 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='middle', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=12, color="grey80")))
}


