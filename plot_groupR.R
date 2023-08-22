plot_groupR <- function(inputRef){
  library(httr)
  library(jsonlite)
  library(ggplot2)
  
  #Go through each reference group
  refgroups <- c("Datasets", "Multimedia", "Projects", "GeospatialData")
  #removed "Documents"
  amounts <- c()
  
  for (type in refgroups){
    filterGroup <- list(list(
      order = 0,
      logicOperator = "",
      group = type
    ))
    
    params <- list(
      units = query_refuge(inputRef),
      referenceGroups = filterGroup
    )
    
    json_output <- api_call(params)
    count <- json_output$pageDetail$totalCount
    
    amounts <- append(amounts, count)
  }
  
  #Make the plot
  df <- data.frame(refgroups, amounts)
  groupPlot <- ggplot(df, aes(x=refgroups, y=amounts, fill=refgroups)) +
    geom_bar(stat="identity", show.legend = FALSE) +
    coord_flip() +
    labs(title = NULL, x = NULL, y = "Number of References") +
    theme(
      text=element_text(family = "sans"),
      axis.text.y = element_text(hjust = 1, size = 13),
      panel.background = element_rect(fill = "lemonchiffon2"),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size = 16, face = "bold",color = "darkgreen"),
      plot.margin = margin(1,1,1,0.3, "cm")
    ) + 
    #optional
    scale_fill_brewer(palette="Greens")
  
  return(groupPlot)
}