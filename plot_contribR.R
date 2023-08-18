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

plot_contrib_sort <- function(inputRef){
  years <- 2011:as.integer(format(Sys.Date(), "%Y"))
  
  yearCounts <- c()
  prev <- 0
  for(i in 1:length(dfs_by_year)){
    df <- subset_by_refuge(dfs_by_year[[i]], inputRef)
    yearCounts <- append(yearCounts, nrow(df) + prev)
    prev <- yearCounts[length(yearCounts)]
  }
  
  change <- c(0)
  for(i in 2:length(yearCounts)){
    change <- append(change, yearCounts[i] - yearCounts[i-1])
  }
  
  #Make graph
  df <- data.frame(years, yearCounts)
  library(ggplot2)
  yearPlot <- ggplot(df,aes(x=years,y=yearCounts,group=1,text=paste0("Cumulative Total: ",yearCounts," \nNew Additions: +",change)))+ 
    #dodgerblue4, cee8f0
    geom_line(color = "#0c3e6f") +
    geom_point(color = "#0c3e6f") +
    scale_x_continuous(breaks = df$years, labels = df$years) +
    labs(title = NULL, x = NULL, y = "Total References in ServCat\n") +
    theme(
      #text=element_text(family = "mono"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "white"),
      axis.text.y = element_text(size = 12, color = "white"),
      panel.background = element_rect(fill = "#dceef4"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#0072B2", size = 1.2),
      axis.title.y = element_text(color = "white", size = 14, face = "bold"),
      plot.margin = margin(1,1.5,1,1, "cm"),
      plot.background = element_blank()
    )
  p <- ggplotly(yearPlot, tooltip = c("text"), height = 550) %>% config(displayModeBar = FALSE) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", hoverlabel = list(font=list(size=17)))
  return(p)
}
