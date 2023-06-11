plot_groupR <- function(inputRef){
  library(httr)
  library(jsonlite)
  library(ggplot2)
  
  #Get org filter
  #Store info for all refuges
  refCodes <- c("AM0","AP0","ARC","IZM","KAN","KNA","KU0","KDK","SWK","TET","TGK","YKD","YKF")
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
  
  #Get refuge index
  index <- which(refNames == inputRef)
  
  #Special cases
  codes <- c()
  if (refShorts[index] == "APB"){
    codes <- c("AP0", "APN", "APB")
  }else if (refShorts[index] == "KNI"){
    codes <- c("KU0", "KUK", "KUN", "INN")
  }
  
  if(length(codes) > 0){
    filterRefuge <- list()
    for (j in 1:length(codes)){
      ccc <- paste("FF07R", codes[j], "00", sep = "")
      if(j==1){
        logic <- ""
      }else{
        logic <- "OR"
      }
      filterRefuge <- append(filterRefuge, list(list(order = j-1, logicOperator = logic, unitCode = ccc)))
    }
    #Typical case
  }else{
    ccc <- paste("FF07R", refCodes[index], "00", sep = "")
    filterRefuge <- list(list(
      order = 0,
      logicOperator = "",
      unitCode = ccc
    ))
  }
  
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
    
    #Define url and params for API request
    url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/AdvancedSearch"
    params <- list(
      units = filterRefuge,
      referenceGroups = filterGroup
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