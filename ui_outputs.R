get_new_sort <- function(inputRef){
  df <- subset_by_refuge(dfs_by_year[[length(dfs_by_year)]], inputRef)
  return(nrow(df))
}

get_last_year <- function(inputRef){
  df <- subset_by_refuge(dfs_by_year[[length(dfs_by_year)-1]], inputRef)
  return(nrow(df))
}

get_remaining <- function(inputRef){
  remaining <- get_last_year(inputRef) - get_new_sort(inputRef) + 1
  if(remaining < 0){
    remaining <- 0
  }
  return(remaining)
}

get_total_sort <- function(inputRef){
  df <- subset_by_refuge(df_total, inputRef)
  return(nrow(df))
}

plot_arlis_sort <- function(inputRef){
  #inputRef <- "Arctic"
  arlis <- nrow(subset_by_refuge(df_arlis, inputRef))
  total <- nrow(subset_by_refuge(df_total, inputRef))
  other <- total - arlis
  
  group <- c("ARLIS", "Other")
  value <- c(arlis, other)
  percent <- c(paste(round(100*arlis/total, 0),"%", sep=""), paste(round(100*other/total, 0),"%", sep=""))
  percent
  
  df <- data.frame(group, value)
  
  pie2 <- plot_ly(
    df,
    type="pie",
    showlegend = FALSE,
    labels = ~group,
    values = ~value,
    textposition = 'outside',
    #textinfo = 'percent+label',
    textinfo = 'text',
    text = ~paste(percent, group),
    hoverinfo = 'text',
    hovertext = ~paste("\nCount: ", value),
    height = 185, width = 300,
    marker = list(colors = c("#FFFFFF", "black")),
    textfont = list(color = "#FFFFFF", size = 14)
  ) %>% config(displayModeBar = FALSE) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", hoverlabel = list(font=list(size=15)), margin = list(l = 20, r = 35, t = 25, b = 45))
  
  return(pie2)
}

get_arlis_sort <- function(inputRef){
  df <- subset_by_refuge(df_arlis, inputRef)
  return(nrow(df))
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
  yearPlot <- ggplot(df,aes(x=years,y=yearCounts,group=1,text=paste0("Year: ", years, "\nCumulative Total: ",yearCounts," \nNew Additions: +",change))) + 
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

get_project_count <- function(inputRef){
  df <- subset_by_refuge(df_total, inputRef)
  count <- length(which(df$Type == "Project"))
  return(count)
}

plot_topics <- function(inputRef){
  categories <- c("Mammals", "Birds", "Fish", "More")
  counts <- c()
  for(x in categories){
    counts <- append(counts, return_title_count(inputRef, x))
  }
  categories[4] <- "More Topics"
  df <- data.frame(categories, counts)
  df$categories <- factor(df$categories, levels = df$categories)
  topicPlot <- ggplot(df,aes(x=categories,y=counts,fill=categories))+ 
    geom_bar(stat="identity", show.legend = FALSE) +
    scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73", "#CC79A7")) +
    labs(title = NULL, x = NULL, y = "ServCat References\n") +
    theme(
      text=element_text(family = "sans"),
      axis.text.x = element_text(angle = 30, hjust = 1, size = 20),
      axis.text.y = element_text(size = 15),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(size = 17, face = "bold"),
      plot.margin = margin(1,1.5,1,1, "cm"),
      plot.background = element_blank()
    )
  # topicPlot <- ggplotly(topicPlot)
  return(topicPlot)
}

get_most_recent <- function(inputRef){
  index <- which(df_recents$names == inputRef)
  return(df_recents$dates[index])
}

#plot_topics("Kenai")
#get_most_recent("Kenai")

plot_remaining <- function(inputRef){
  #inputRef <- "Togiak"
  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  years <- c(currentYear - 1, currentYear)
  counts <- c(get_last_year(inputRef), get_new_sort(inputRef))
  df <- data.frame(years, counts)
  remaining <- ggplot(df, aes(x=factor(years), y=counts, fill = years)) +
    geom_bar(stat="identity", show.legend = FALSE, fill = c("black", "white")) +
    geom_text(aes(label=counts), vjust=1.4, color=c("white","black"), size=5, fontface='bold') +
    #geom_hline(yintercept = counts[1], col = "red", size = 1.3, linetype = "dashed") +
    labs(x = NULL, y = "References Added") +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.text.x = element_text(size = 17, color = "white"),
      axis.text.y = element_text(size = 16, color = "white"),
      axis.title.y = element_text(size = 15, color = "white", face = "bold", margin = margin(r = 10)),
      plot.margin = unit(c(0.2,0.3,0.3,0.3), "cm")
    )
  #remaining
  return(remaining)
}

get_recent_link <- function(inputRef){
  df <- df_recents
  code <- df$ids[which(df$names == inputRef)]
  return(code)
}

get_savedsearch_link <- function(inputRef){
  names <- return_refuge_df()$names
  nums <- c(2589, 2590, 2591, 2592, 2593, 2594, 2588, 2595, 2587, 2586, 2596, 2597, 2598)
  df <- data.frame(names, nums)
  code <- df$nums[which(df$names == inputRef)]
  return(code)
}