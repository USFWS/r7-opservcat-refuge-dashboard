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
