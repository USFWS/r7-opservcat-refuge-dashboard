plot_subjectsR <- function(inputRef){
  library(httr)
  library(jsonlite)
  library(stringr)
  library(ggplot2)
  library(treemapify)
  
  #inputRef <- "Togiak"
  
  #Make dataframe of refuge ccc's
  refCodes <- c("RAM0","RAP0","RARC","RIZM","RKAN","RKNA","RKU0","RKDK","RSWK","RTET","RTGK","RYKD","RYKF")
  refShorts <- c("AM","APB","Arc","Iz","Kan","Ken","KNI","Kod","Sel","Tet","Tog","YKD","YKF")
  refNames <- c("Alaska Maritime",
                "APB",
                "Arctic",
                "Izembek",
                "Kanuti",
                "Kenai",
                "KNI",
                "Kodiak",
                "Selawik",
                "Tetlin",
                "Togiak",
                "Yukon Delta",
                "Yukon Flats")
  refDir <- data.frame(refCodes, refShorts, refNames)
  
  #Get subject category list
  url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/FixedList/SubjectCategories"
  response <- GET(url = url, config = authenticate(":",":","ntlm"), encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  
  if(http_error(response) == TRUE){
    stop("This request has failed.")
  }
  
  subcatsdf <- fromJSON((content(response, as = "text")))
  
  #Remove extra NULL description column
  subcatsdf <- subcatsdf[,-3]
  
  
  #API request for category counts by refuge
  #bio = 1009 to 1038
  refuge_df <- data.frame()
  
  ccc <- paste("FF07", refDir$refCodes[refDir$refNames == inputRef], "00", sep = "")
  
  for(x in 1009:1038){
    filterRefuge <- list(
      order = 0,
      logicOperator = "",
      unitCode = ccc
    )
    
    filterSubCat <- list(
      order = 0,
      logicOperator = "",
      subjectCategory = x
    )
    
    #Define url and params for API request
    url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/AdvancedSearch"
    params <- list(
      units = list(filterRefuge),
      subjectCategories = list(filterSubCat)
    )
    body <- toJSON(params, auto_unbox = TRUE)
    response <- POST(url = url, config = authenticate(":",":","ntlm"), body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
    
    #Halt code if error
    if(http_error(response) == TRUE){
      stop("This request has failed.")
    }
    
    #Continue if no error
    json_output <- fromJSON((content(response, as = "text")))
    count <- json_output$pageDetail$totalCount
    
    refuge_df<- rbind(refuge_df, c(x,subcatsdf$label[subcatsdf$key == x],count))
  }
  
  colnames(refuge_df) <- c("key","label","count")
  
  #Top 10
  refuge_df$count <- as.numeric(refuge_df$count)
  sorted <- refuge_df[order(refuge_df$count, decreasing=TRUE),]
  refuge_df_top10 <- sorted[1:10,]
  
  #Format category names
  for (i in 1:10){
    refuge_df_top10$label[i] <- str_trim(unlist(strsplit(refuge_df_top10$label[i], split = "|", fixed = TRUE))[3])
  }
  
  #Make bar chart
  refuge_df_top10$label <- factor(refuge_df_top10$label, levels=refuge_df_top10$label)
  catplot <- ggplot(refuge_df_top10, aes(x="", y=count, fill=label)) +
    geom_bar(width = 1, stat = "identity") +
    labs(title = NULL, x = NULL, y = "Number of References", fill = "Subject") +
    theme(
      text=element_text(family = "sans"),
      axis.title.y = element_text(size = 15, face = "bold",color = "darkslateblue")
    )
  
  #Alternative tree plot
  legend <- c(paste(refuge_df_top10$label, " (", refuge_df_top10$count, ")", sep=""))
  legend
  treeplot <- ggplot(refuge_df_top10, aes(area=count, fill=label, label=label, subgroup = label)) +
    geom_treemap() +
    geom_treemap_subgroup_border() +
    geom_treemap_text(
      colour = "white",
      place = "centre"
    ) +
    scale_fill_discrete(labels = legend) +
    labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
    theme(
      text=element_text(family = "sans"),
      legend.position = "bottom",
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA)
    )
  #treeplot
  
  return(treeplot)
}
