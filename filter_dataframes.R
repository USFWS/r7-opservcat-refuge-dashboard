library(data.tree)

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

#Function: subset_by_keywords(inputDf, keywords, exclusions)
#Given a dataframe, returns a subset of entries with titles that include the
#provided keywords (as vector) and exclude the provided excluded words (as
#vector)
subset_by_title_keywords <- function(inputDf, keywords, exclusions){
  keyword_subset <- inputDf[grepl(paste(keywords, collapse = "|"), inputDf$Title, ignore.case = TRUE), ]
  if (length(exclusions)>0){
    with_exclusions <- keyword_subset[!grepl(paste(exclusions, collapse = "|"), keyword_subset$Title, ignore.case = TRUE), ]
  }else{
    with_exclusions <- keyword_subset
  }
  return(with_exclusions)
}

#Function: format_df(inputDf)
#Given a data frame with a "RefID" column, returns the same data frame with
#an added column of ServCat links using the reference ID values. The Date column
#is modified to show only the year. The RefID and and Units columns are removed.
format_df <- function(inputDf){
  links <- c()
  for(i in 1:length(inputDf$RefID)){
    id <- inputDf$RefID[i]
    links <- append(links, paste("<a href='", "https://ecos.fws.gov/ServCat/Reference/Profile/", id, "' target='blank'>", "View in ServCat", "</a>", sep=""))
    inputDf$Date[i] <- strtoi(substr(inputDf$Date[i], 1, 4))
  }
  new_df <- cbind(inputDf, links)
  colnames(new_df)[colnames(new_df) == "links"] <- "Link"
  new_df <- new_df[-c(1,2,5)]
  return(new_df)
}

format_df_download <- function(inputDf){
  links <- c()
  for(i in 1:length(inputDf$RefID)){
    id <- inputDf$RefID[i]
    links <- append(links, paste("https://ecos.fws.gov/ServCat/Reference/Profile/", id, sep=""))
    inputDf$Date[i] <- strtoi(substr(inputDf$Date[i], 1, 4))
  }
  new_df <- cbind(inputDf, links)
  colnames(new_df)[colnames(new_df) == "links"] <- "Link"
  new_df <- new_df[-c(1,2,5)]
  return(new_df)
}

return_title_count <- function(inputRef, buttonName){
  df <- subset_by_refuge(df_total, inputRef)
  df <- subset_by_title_keywords(df, keywords = get_button_keywords(buttonName), exclusions = get_button_exclusions(buttonName))
  return(nrow(df))
}

build_tree <- function(){
  tree <- Node$new("Tree")
    mam <- tree$AddChild("Mammals", keywords = c("mammal"), exclusions = c())
      fur <- mam$AddChild("Furbearers", keywords = c("furbearer"), exclusions = c())
        fur$AddChild("Beavers", keywords = c("beaver"), exclusions = c())
        fur$AddChild("Wolves", keywords = c("wolf", "wolves"), exclusions = c())
        fur$AddChild("Weasels, Marten, Ermine, and Mink", keywords = c("weasel", "marten", "ermine", "mink", "stoat"), exclusions = c("determine"))
        fur$AddChild("Bears", keywords = c("bear"), exclusions = c("furbearer"))
        fur$AddChild("Hares", keywords = c("hare"), exclusions = c("share"))
        fur$AddChild("Wolverines", keywords = c("wolverine"), exclusions = c())
        fur$AddChild("Lynx", keywords = c("lynx"), exclusions = c())
        fur$AddChild("Otters", keywords = c("otter"), exclusions = c())
        fur$AddChild("Coyote", keywords = c("coyote"), exclusions = c())
        fur$AddChild("Muskrat", keywords = c("muskrat"), exclusions = c())
        fur$AddChild("Lemming", keywords = c("lemming"), exclusions = c())
      ung <- mam$AddChild("Ungulates", keywords = c("ungulate"), exclusions = c())
        ung$AddChild("Moose", keywords = c("moose"), exclusions = c("national moose range"))
        ung$AddChild("Caribou", keywords = c("caribou", "reindeer"), exclusions = c())
        ung$AddChild("Elk", keywords = c("elk"), exclusions = c())
        ung$AddChild("Bison", keywords = c("bison"), exclusions = c())
        ung$AddChild("Goats", keywords = c("goat"), exclusions = c())
        ung$AddChild("Sheep", keywords = c("sheep"), exclusions = c())
        ung$AddChild("Muskoxen", keywords = c("muskox"), exclusions = c())
        ung$AddChild("Deer", keywords = c("deer"), exclusions = c("killdeer"))
      cet <- mam$AddChild("Cetaceans and Pinnipeds", keywords = c("cetacean", "pinniped"), exclusions = c())
        cet$AddChild("Seals and Sea Lions", keywords = c("seal", "ugruk", "square flipper", "squareflipper", "sea lion"), exclusions = c("sealing"))
        cet$AddChild("Walruses", keywords = c("walrus"), exclusions = c("walrus island"))
        cet$AddChild("Whales", keywords = c("whale, beluga, bowhead, porpoise"), exclusions = c())
    bird <- tree$AddChild("Birds", keywords = c("bird", "avian"), exclusions = c())
      land <- bird$AddChild("Landbirds", keywords = c("landbird", "land bird"), exclusions = c())
        land$AddChild("Raptors and Owls", keywords = c("raptor", "eagle", "falcon", " owl", "osprey"), exclusions = c("lowland", "chowlet"))
        land$AddChild("Passerine", keywords = c("passerine", "songbird", "song bird", "christmas bird", "cardinal", "sparrow", "chickadee", "corvid", "magpie", "raven", "flycatcher", "blackbird"), exclusions = c())
        land$AddChild("Landfowl", keywords = c("landfowl", "gamefowl", "grouse", "ptarmigan"), exclusions = c())
      water <- bird$AddChild("Waterbirds", keywords = c("waterbird", "water bird"), exclusions = c())
        water$AddChild("Waterfowl", keywords = c("duck", "goose", "geese", "swan", "brant", "shoveler", "pintail", "scaup", "eider", "scoter", "goldeneye", "dabbler", "canvasback"), exclusions = c("duck island"))
        water$AddChild("Seabirds", keywords = c("sea bird", "seabird", "marine bird", "pelagic bird", "auk", "puffin", "murrelet", "gull", "tern"), exclusions = c("eastern", "pattern"))
        water$AddChild("Shorebirds/Wading Birds", keywords = c("shorebird", "shore bird", "wading bird", "godwit", "sandpiper", "whimbrel", "oystercatcher", "turnstone", "curlew", "dunlin", "crane"), exclusions = c())
        water$AddChild("Cormorants and Loons", keywords = c("cormorant", "loon"), exclusions = c())
    fish <- tree$AddChild("Fish", keywords = c("fish", "creel"), exclusions = c("fish and wildlife service"))
      fish$AddChild("Salmon", keywords = c("salmon", "sockeye", "chinook"), exclusions = c("king salmon"))
      fish$AddChild("Other Salmonids", keywords = c("salmonid", "char ", "grayling", "trout", "whitefish", "sheefish", "inconnu", "dolly vardan"), exclusions = c("becharof", "character"))
      fish$AddChild("Non-Salmonids", keywords = c("blackfish", "burbot", "smelt", "pike"), exclusions = c())
    more <- tree$AddChild("More", keywords = c(), exclusions = c())
      more$AddChild("Fire", keywords = c("fire", "burn", "flame", "ignition"), exclusions = c("lisburne", "burnt island", "fire island"))
      more$AddChild("Snow", keywords = c("snow", "snotel"), exclusions = c("snowshoe", "snowgeese", "snow geese"))
      more$AddChild("Contaminants", keywords = c("metal","toxic", "water quality", "chemistry", "contaminant", "contamination", "oil spill", "concentration", "pesticide", "herbicide"), exclusions = c())
      more$AddChild("Harvest", keywords = c("harvest", "subsistence", "sealing", "hunt"), exclusions = c())
      more$AddChild("Plants", keywords = c("plant", "berry", "grass", "elodea", "lichen", "sphagnum", "phytomass", "graminoid", "fern", "vegetation", "FIA", "algae", "flower", "weed", "flora"), exclusions = c("transplant"))
      more$AddChild("Invasive Species", keywords = c("invasive", "non-native", "pest"), exclusions = c("pesticide"))
      more$AddChild("Amphibians", keywords = c("frog", "amphibian"), exclusions = c())
      more$AddChild("Invertebrates", keywords = c("pollinator", "invertebrate", "bee ", "dragonfly", "arthropod", "spider", "beetle", "mussel"), exclusions = c())
      more$AddChild("Disease", keywords = c("disease", "influenza", "virus"), exclusions = c())
      more$AddChild("Disturbance", keywords = c("disturbance", "vehicle", "seismic", "petroleum", "oil spill", "hydroelectric", "hydrocarbon"), exclusions = c("national petroleum reserve"))
      more$AddChild("Climate/Climate Change", keywords = c("carbon", "permafrost", "thermokarst", "glacial retreat", "thaw slump", "climate", "gloria", "ecological change"), exclusions = c("hydrocarbon"))
      more$AddChild("Wilderness", keywords = c("wilderness"), exclusions = c())
      
  #print(tree)
  return(tree)
}

get_button_keywords <- function(buttonName){
  tree <- build_tree()
  node <- FindNode(tree, buttonName)
  keywords <- node$keywords
  if (length(node$leaves) > 1){
    keywords <- append(keywords, unique(unlist(sapply(node$leaves, function(child) child$keywords))))
  }
  return(keywords)
}

get_button_exclusions <- function(buttonName){
  tree <- build_tree()
  node <- FindNode(tree, buttonName)
  exclusions <- node$exclusions
  if (length(node$leaves) > 1){
    exclusions <- append(exclusions, unique(unlist(sapply(node$leaves, function(child) child$exclusions))))
  }
  return(exclusions)
}

return_title_table <- function(inputRef, buttonName){
  df <- subset_by_refuge(df_total, inputRef)
  df <- subset_by_title_keywords(df, keywords = get_button_keywords(buttonName), exclusions = get_button_exclusions(buttonName))
  df <- format_df(df)
  return(df)
}

return_title_table_download <- function(inputRef, buttonName){
  df <- subset_by_refuge(df_total, inputRef)
  df <- subset_by_title_keywords(df, keywords = get_button_keywords(buttonName), exclusions = get_button_exclusions(buttonName))
  df <- format_df_download(df)
  return(df)
}

return_unformatted_df <- function(inputRef, buttonName){
  df <- subset_by_refuge(df_total, inputRef)
  df <- subset_by_title_keywords(df, keywords = get_button_keywords(buttonName), exclusions = get_button_exclusions(buttonName))
  return(df)
}

#df <- subset_by_title_keywords(df_test, keywords = get_button_keywords("Fish"), exclusions = get_button_exclusions("Fish"))

make_refcode_file <- function(inputDf, file){
  connection <- file(file, "w")

  codes <- gsub(" ", "", toString(inputDf$RefID))
  codes <- paste(codes, sep=",")
  cat(codes, file = connection)

  close(connection)
  
  #writeLines(toString(inputDf$RefID), sep=",", file)
}

#make_refcode_file(df_arlis, "output.txt")

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
      axis.title.y = element_text(size = 17),
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
    labs(x = NULL, y = "References Added") +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.text.x = element_text(size = 16, color = "white"),
      axis.text.y = element_text(size = 15, color = "white"),
      axis.title.y = element_text(size = 15, color = "white", face = "bold")
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
