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

get_total_sort <- function(inputRef){
  df <- subset_by_refuge(df_total, inputRef)
  return(nrow(df))
}