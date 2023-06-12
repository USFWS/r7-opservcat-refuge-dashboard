library(httr)
library(jsonlite)

get_oldestR <- function(inputRef){
  params <- list(
    units = query_refuge(inputRef)
  )
  
  json_output <- api_call_sort(params)
  date <- json_output$item[3][1,]
  year <- substr(date, 1, 4)
  return(year)
}









# #IGNORE
# OLDget_oldestR <- function(inputRef){
#   library(httr)
#   library(jsonlite)
#   
#   #Get refuge filter
#   
#   #Store info for all refuges
#   refCodes <- c("AM0","AP0","ARC","IZM","KAN","KNA","KU0","KDK","SWK","TET","TGK","YKD","YKF")
#   refShorts <- c("AM","APB","Arc","Iz","Kan","Ken","KNI","Kod","Sel","Tet","Tog","YKD","YKF")
#   refNames <- c("Alaska Maritime",
#                 "APB",
#                 "Arctic",
#                 "Izembek",
#                 "Kanuti",
#                 "Kenai",
#                 "KNI",
#                 "Kodiak",
#                 "Selawik",
#                 "Tetlin",
#                 "Togiak",
#                 "Yukon Delta",
#                 "Yukon Flats")
#   
#   #Get refuge index
#   index <- which(refNames == inputRef)
#   
#   #Special cases
#   codes <- c()
#   if (refShorts[index] == "APB"){
#     codes <- c("AP0", "APN", "APB")
#   }else if (refShorts[index] == "KNI"){
#     codes <- c("KU0", "KUK", "KUN", "INN")
#   }
#   
#   if(length(codes) > 0){
#     filterRefuge <- list()
#     for (j in 1:length(codes)){
#       ccc <- paste("FF07R", codes[j], "00", sep = "")
#       if(j==1){
#         logic <- ""
#       }else{
#         logic <- "OR"
#       }
#       filterRefuge <- append(filterRefuge, list(list(order = j-1, logicOperator = logic, unitCode = ccc)))
#     }
#     #Typical case
#   }else{
#     ccc <- paste("FF07R", refCodes[index], "00", sep = "")
#     filterRefuge <- list(list(
#       order = 0,
#       logicOperator = "",
#       unitCode = ccc
#     ))
#   }
#   
#   #Find decade
#   year <- 2050
#   prev <- 1
#   continue <- TRUE
#   
#   while(continue == TRUE){
#     filterDate <- list(list(
#       order = 0,
#       logicOperator = "",
#       fieldName = "DateOfIssue",
#       filter = "BeforeDate",
#       startDate = paste(year,"-01-01",sep = "")
#     ))
#     
#     #Define url and params for API request
#     url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/AdvancedSearch"
#     params <- list(
#       units = filterRefuge,
#       dates = filterDate
#     )
#     
#     body <- toJSON(params, auto_unbox = TRUE)
#     response <- POST(url = url, config = authenticate(":",":","ntlm"), body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
#     
#     #Halt code if error
#     if(http_error(response) == TRUE){
#       stop("This request has failed.")
#     }
#     
#     #Continue if no error
#     json_output <- fromJSON((content(response, as = "text")))
#     count <- json_output$pageDetail$totalCount
#     
#     print(count)
#     print(prev)
#     print(year)
#     
#     if((prev == 0)&&(count > 0)){
#       continue <- FALSE
#     }else if(count > 0){
#       year <- year-50
#     }else{
#       year <- year+10
#     }
#     
#     prev <- count
#   }
# 
#   decade <- year - 10
#   decade <- paste(decade, "s", sep = "")
#   
#   return(decade)
# }