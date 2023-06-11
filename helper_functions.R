#Function: return_refuge_df()
#Returns data frame with refuge names, abbreviations, and cost center codes

return_refuge_df <- function(){
  refNames <- c("Alaska Maritime",
                "Alaska Peninsula/Becharof",
                "Arctic",
                "Izembek",
                "Kanuti",
                "Kenai",
                "Kodiak",
                "Koyukuk/Nowitna/Innoko",
                "Selawik",
                "Tetlin",
                "Togiak",
                "Yukon Delta",
                "Yukon Flats")
  refCodes <- list("FF07RAM000",
                   c("FF07RAP000", "FF07RAPN00", "FF07RAPB00"),
                   "FF07RARC00",
                   "FF07RIZM00",
                   "FF07RKAN00",
                   "FF07RKNA00",
                   "FF07RKDK00",
                   c("FF07RKU000", "FF07RKUK00", "FF07RKUN00", "FF07RINN00"),
                   "FF07RSWK00",
                   "FF07RTET00",
                   "FF07RTGK00",
                   "FF07RYKD00",
                   "FF07RYKF00")
  
  #I()is used so that list is treated as is instead of flattened, to allow for a vector
  #in a single data frame cell.
  df <- data.frame(names = refNames, codes = I(refCodes))
  return(df)
}
#Example test...
#return_refuge_df()$codes[[2]][3]
#length(return_refuge_df()$codes[[7]])


#Function: return_arlis_list()
#Returns list of all reference creators associated with the ARLIS Team
return_arlis_list <- function(){
  arlis <- c("CeliaatARLIS", "CSwansonARLIS", "stevejarlis", "mwillis", "saddison2", "lohman.lucas", "Mwjohnson2", "ErinBentley", "Valerie-ARLIS", "thodges")
  return(arlis)
}

#Function: return_refprogram_list()
#Returns list of all cost center codes associated with the region 7 refuge program
return_refprogram_list <- function(){
  refCCC <- c("AM0","AP0","APN","APB","ARC","IZM","KAN","KNA","KU0","KUK","KUN","INN","KDK","SWK","TET","TGK","YKD","YKF")
  otherCCC <- c("000","010","020","02M","02W","030","040","050","060","080","090","091","092","AMT","0MD","YFY")
  allCCC <- append(refCCC, otherCCC)
  for (x in 1:length(allCCC)){
    allCCC[x] <- paste("FF07R", allCCC[x], "00", sep = "")
  }
  return(allCCC)
}

#Function: api_call(params)
#Takes list of parameters formatted as json and returns the result of an
#Advanced Search API call
api_call <- function(params){
  #Make call
  url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/AdvancedSearch/Composite"
  body <- toJSON(params, auto_unbox = TRUE)
  response <- POST(url = url, config = authenticate(":",":","ntlm"), body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  
  #Halt code if error
  if(http_error(response) == TRUE){
    stop("This request has failed.")
  }
  
  #Convert output from json for parsing
  json_output <- fromJSON((content(response, as = "text")))
  return(json_output)
}

#Function: query_refuge(refugeName)
#Create parameter to filter search by references pertaining to a specific refuge
query_refuge <- function(refugeName){
  df <- return_refuge_df()
  index <- which(df$name == refugeName)
  
  #case - refuge with multiple cost center codes (complexed)
  if(length(df$codes[[index]]) > 1){
    filterRefuge <- list()
    for (j in 1:length(df$codes[[index]])){
      if(j==1){
        logic <- ""
      }else{
        logic <- "OR"
      }
      filterRefuge <- append(filterRefuge, list(list(order = j-1, logicOperator = logic, unitCode = df$codes[[index]][j])))
    }
  #case - typical
  }else{
    filterRefuge <- list(list(order = 0, logicOperator = "", unitCode = df$codes[[index]]))
  }
  
  return(filterRefuge)
}

#Function: query_creators()
#Create parameter to filter search by references created by ARLIS staff
query_ARLIScreators <- function(){
  filterPeople <- list()
  for (i in 1:length(return_arlis_list())){
    if(i==1){
      logic <- ""
    }else{
      logic <- "OR"
    }
    filterPeople <- append(filterPeople, list(list(order = i-1, logicOperator = logic, fieldName = "Creator", searchText = return_arlis_list()[i])))
  }
  return(filterPeople)
}

#Function: query_year()
#Create parameter to filter search by references created during a certain year
query_year <- function(year){
  filterDate <- list(list(
    order = 0,
    logicOperator = "",
    fieldName = "DateCreated",
    filter = "BetweenDates",
    startDate = paste(year,"-01-01",sep = ""),
    endDate = paste(year,"-12-31",sep = "")
  ))
  return(filterDate)
}

#Function: query_orgs(ccc_list)
#Create parameter to filter search by a list of organization cost center codes
query_orgs <- function(ccc_list){
  filterRefuge <- list()
  for (x in 1:length(ccc_list)){
    if(x==1){
      logic <- ""
    }else{
      logic <- "OR"
    }
    filterRefuge <- append(filterRefuge, list(list(order = x-1, logicOperator = logic, unitCode = ccc_list[x])))
  }
  return(filterRefuge)
}

#Function: subset_by_refuge(inputDf, inputRef)
#Takes a data frame (with a "Units" column of reference organization codes) and
#subsets it into a data frame of references corresponding to a single, provided
#refuge name
subset_by_refuge <- function(inputDf, inputRef){
  directory <- return_refuge_df()
  index <- which(directory$names == inputRef)
  cccs <- directory$codes[[index]]
  single_refuge_subset <- inputDf[sapply(inputDf$Units, function(x) any(x %in% cccs)), ]
  return(single_refuge_subset)
}

#Preexisting Function: nrow(inputDf)
#Returns count of entries (rows) in a provided dataframe

#Function: subset_by_keywords(inputDf, keywords, exclusions)
#Given a dataframe, returns a subset of entries with titles that include the
#provided keywords (as vector) and exclude the provided excluded words (as
#vector)
subset_by_title_keywords <- function(inputDf, keywords, exclusions){
  directory <- return_refuge_df()
  index <- which(directory$names == inputRef)
  cccs <- directory$codes[[index]]
  keyword_subset <- inputDf[grepl(paste(keywords, collapse = "|"), inputDf$Title, ignore.case = TRUE), ]
  if (length(exclusions)>=1){
    with_exclusions <- keyword_subset[!grepl(paste(exclusions, collapse = "|"), keyword_subset$Title, ignore.case = TRUE), ]
  }else{
    with_exclusions <- keyword_subset
  }
  return(with_exclusions)
}

#Function: return_r7NWRs_df()
#Return master data frame for Advanced Search results for all references for
#region 7 NWRS
return_r7NWRs_df <- function(){
  #Query for all R7 NWR references
  params <- list(
    units = query_orgs(unlist(return_refuge_df()$codes))
  )
  #Set up master data frame
  json_output <- api_call(params)
  titles <- json_output$items$title
  ids <- json_output$items$referenceId
  dates <- json_output$items$dateOfIssue
  orgs <- json_output$items$units
  df <- data.frame(ids, titles, dates, I(orgs))
  colnames(df) <- c("RefID","Title","Date","Units")
  #Return
  return(df)
}

#Function: add_link_column(inputDf)
#Given a data frame with a "RefID" column, returns the same data frame with
#an added column of ServCat links using the reference ID values
add_link_column <- function(inputDf){
  links <- c()
  for(i in 1:length(inputDf$RefID)){
    id <- inputDf$RefID[i]
    links <- append(links, paste("https://ecos.fws.gov/ServCat/Reference/Profile/", id, sep=""))
  }
  new_df <- cbind(inputDf, links)
  colnames(new_df)[colnames(new_df) == "links"] <- "Link"
  return(new_df)
}