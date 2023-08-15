get_newR <- function(inputRef){
  library(httr)
  library(jsonlite)
  
  #Filter by date
  current <- as.integer(format(Sys.Date(), "%Y"))
  filterDate <- list(list(
    order = 0,
    logicOperator = "",
    fieldName = "DateCreated",
    filter = "AfterDate",
    startDate = paste(current,"-01-01",sep="")
  ))

  params <- list(
    units = query_refuge(inputRef),
    dates = filterDate
  )
  
  json_output <- api_call(params)
  count <- json_output$pageDetail$totalCount
  
  return(count)
}

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