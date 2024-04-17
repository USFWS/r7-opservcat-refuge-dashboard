#API calls made to obtain data frames when initializing app

#Function: api_call_long(params)
#Takes list of parameters formatted as json and returns the result of an
#Advanced Search API call with page length 5000
api_call_long <- function(params){
  #Make call
  url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/AdvancedSearch/Composite?top=5000"
  body <- toJSON(params, auto_unbox = TRUE)
  response <- POST(url = url, config = authenticate(":",":","ntlm"), body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  #response <- POST(url = url, body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  
  #Halt code if error
  if(http_error(response) == TRUE){
    stop("This request has failed.")
  }
  
  #Convert output from json for parsing
  json_output <- fromJSON((content(response, as = "text")))
  return(json_output)
}

#Function: api_call_profile(codelist)
#Takes list of reference codes and returns the result of a Reference Profile
#get request
api_call_profile <- function(codelist){
  #Buid URL
  url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/Profile?q="
  for (i in 1:length(codelist)){
    if (i == 1){
      url <- paste(url,codelist[i],sep="")
    }else{
      url <- paste(url,"%2C",codelist[i],sep="")
    }
  }
  
  #Make call
  response <- GET(url = url, config = authenticate(":",":","ntlm"), encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  #response <- GET(url = url, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  
  #Halt code if error
  if(http_error(response) == TRUE){
    stop("This request has failed.")
  }
  
  #Convert output from json for parsing
  json_output <- fromJSON((content(response, as = "text")))
  return(json_output)
}

#Function: return_df(params)
#Return master data frame for Advanced Search results given params list
return_df <- function(params){
  json_output <- api_call_long(params)
  
  titles <- json_output$items$title
  ids <- json_output$items$referenceId
  types <- json_output$items$referenceType
  dates <- json_output$items$dateOfIssue
  orgs <- json_output$items$units
  
  df <- data.frame(ids, types, titles, dates, I(orgs))
  colnames(df) <- c("RefID","Type","Title","Date","Units")
  return(df)
}

#Function: return_df_arlis()
#Return data frame for Advanced Search results for all references of
#region 7 NWRS created by ARLIC staff
return_df_arlis <- function(){
  params <- list(
    units = query_orgs(unlist(return_refuge_df()$codes)),
    people = query_ARLIScreators()
  )
  return(return_df(params))
}

#Function: return_dflist_year()
#Return list of data frames by year (2011-current) for Advanced Search results
#for all references of region 7 NWRS 
return_dflist_year <- function(){
  by_year <- list()
  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  for (i in 2011:currentYear) {
    params <- list(
      units = query_orgs(unlist(return_refuge_df()$codes)),
      dates = query_year(i)
    )
    by_year <- append(by_year, list(return_df(params)))
  }
  return(by_year)
  #by_year[[1]]$Title[1]
}

#Function: merge_dfs(df_list)
#Given a list of dataframes, merge them into one
merge_dfs <- function(df_list){
  df <- df_list[[1]]
  for (i in 2:length(df_list)){
    df <- rbind(df, df_list[[i]])
  }
  return(df)
}

#Function: return_df_recents()
#Returns dataframe with each of the refuge names and the corresponding most recent date
#for creating a ServCat reference - by calling API for reference profiles of the
#highest reference code values
return_df_recents <- function(){
  refuges <- return_refuge_df()
  max_codes <- c()
  for(i in 1:length(refuges$names)){
    most_recent <- max(subset_by_refuge(df_total, refuges$names[i])$RefID)
    max_codes <- append(max_codes, most_recent)
  }
  
  df_maxcodes <- data.frame(names = refuges$names, ids = max_codes)
  
  json_output <- api_call_profile(df_maxcodes$ids)
  api_ids <- json_output$referenceId
  api_dates <- json_output$history$created
  
  df_dates <- data.frame(ids = api_ids, dates = api_dates)
  
  ordered_dates <- c()
  for (i in 1:length(df_maxcodes$ids)){
    ordered_dates <- append(ordered_dates, df_dates$dates[which(df_dates$ids == df_maxcodes$ids[i])])
  }
  
  df <- cbind(df_maxcodes, dates = ordered_dates)
  df$dates <- paste(substring(df$dates,6,7), "/", substring(df$dates,9,10), "/", substring(df$dates,1,4), sep="")
  return(df)
}
