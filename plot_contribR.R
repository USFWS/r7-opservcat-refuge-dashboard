plot_contribR <- function(inputRef, whichPlot){
  library(httr)
  library(jsonlite)
  library(ggplot2)
  
  files <- c()
  
  #Iterate through refuges
  for(i in 1:length(return_refuge_df()$names)){
    if(whichPlot == "arlis"){
      params <- list(
        units = query_refuge(return_refuge_df()$names[i]),
        people = query_ARLIScreators()
      )
    }else{
      params <- list(
        units = query_refuge(return_refuge_df()$names[i])
      )
    }
    
    json_output <- api_call(params)
    count <- json_output$pageDetail$totalCount
    
    files <- append(files, count)
  }
  
  #Make data frame for just refuge file counts
  df <- data.frame(return_refuge_df()$names, files)
  colnames(df) <- c("name","fileCount")
  
  #Reorder based on drop down selection
  df <- df[c(which(df$name == inputRef), which(df$name != inputRef)),]
  rownames(df) <- 1:nrow(df)
  df$name <- factor(df$name, levels = unique(df$name))
  
  #Make the plot
  filesbyrefuge <- ggplot(df,aes(x=name,y=fileCount,fill=name))+ 
    geom_bar(stat="identity", show.legend = FALSE) +
    geom_hline(yintercept=df$fileCount[1],linetype=2) +
    scale_fill_manual(values = c("forestgreen", rep("gray80", 12))) +
    labs(title = NULL, x = NULL, y = "References Added") +
    theme(
      text=element_text(family = "sans"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 13, face = c("bold", rep("italic", 12))),
      panel.background = element_rect(fill = "lemonchiffon2"),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(size = 15, face = "bold",color = "darkgreen"),
      plot.margin = margin(1,1.5,1,1, "cm")
    )
  
  return(filesbyrefuge)
}
