get_totalR <- function(inputRef){
  library(httr)
  library(jsonlite)

  params <- list(
    units = query_refuge(inputRef)
  )

  json_output <- api_call(params)
  count <- json_output$pageDetail$totalCount
  
  return(count)
}
