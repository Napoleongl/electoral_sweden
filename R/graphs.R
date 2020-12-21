#library(waffle)
#library(emojifont)
library(plotly)

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


