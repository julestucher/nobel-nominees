library(tidyverse)
library(RSelenium)
library(rvest)
library(stringr)

# select category
selectCategory <- function(remDr, categoryIndex) {
  
  menu <- remDr$findElement("name", 'prize')
  menu$clickElement()
  prize <- menu$findChildElements("tag name", "option")[[categoryIndex]]
  prize$clickElement()
  
  return(remDr)
}

# enter year
enterYear <- function(remDr, year){
  
  yearInput <- remDr$findElement("name", 'year')
  yearInput$clearElement()
  yearInput$sendKeysToElement(list(paste(year)))
  
  return(remDr)
}

# search
searchArchive <- function(remDr){
  
  # list button is third of element type 'input'
  listButton <- remDr$findElements("tag name", "input")[[3]]
  listButton$clickElement()
  
  return(remDr)
}

# get name of current category
getCategoryName <- function(remDr, categoryIndex){
  page <- read_html(remDr$getPageSource()[[1]])
  categories <- page %>% html_elements("option")
  selecteds <- !is.na(categories %>% html_attr("selected"))
  return(categories[selecteds] %>% html_text() %>% trimws())
}

# get info df
getInfoData <- function(i, numNominees, infoList, fields, type){
  
  # get sublist
  if(numNominees == 1){
    nList <- infoList[match(paste0(type, ":"), infoList):length(infoList)]
  } else if (i < numNominees) {
    nList <- infoList[match(paste0(type, " ", i, ":"), infoList):match(paste0(type, " ", i+1, ":"), infoList)]
  } else {
    nList <- infoList[match(paste0(type, " ", i, ":"), infoList):length(infoList)]
  }
  
  # set up empty output list
  out <- list()
  for(field in fields){
    out[[field]] <- nList[match(field, nList) + 1] %>%
      ifelse(!is.null(.), ., NA)
  }
  
  if(type == "Nominee") {
    out[["Prize:"]] = infoList[grepl("the Nobel Prize in ", infoList)][i] %>%
      ifelse(!is.null(.), ., NA)
  }
  
  return(do.call("cbind.data.frame", out))
}

# get details for a nominee
getDetails <- function(remDr, link){
  
  if(!is.na(link)){
    
    ## Get nominee info ##
    page <- read_html(paste0("https://www.nobelprize.org/nomination/archive/", link))
    infoList <- page %>% html_elements("td") %>% html_text() 
    if(!is.na(match("Comments:", infoList))){
      infoList <- infoList[1:match("Comments:", infoList)]
    }
  
    # general info
    Year <- infoList[match("Year:", infoList) + 1]
    Number <- infoList[match("Number:", infoList) + 1]
    
    # nominee info
    fields <- c("Name:", "Gender:", "Year, Birth:", "Year, Death:", "Profession:", "University:", "City:", "Country:")
    numNominees <- table(grepl("Nominee", infoList))[["TRUE"]]
    nomineeDf <- map(1:numNominees, ~getInfoData(., numNominees, infoList, fields, "Nominee")) %>%
      do.call("rbind.data.frame", .) %>%
      rename_with(function(.x) substr(.x, 1, nchar(.x)-1)) %>%
      rename(BirthYear = "Year, Birth", DeathYear = "Year, Death") %>%
      mutate(Year=Year, Number=Number,
             NumberOfNomineesInGroup=numNominees,
             Winner=(str_split(Prize, " ") %>% tail(1)) == Year)

    # nominator info
    fields <- list("Name:", "Gender:", "Profession:", "City:", "Country:")
    numNominators <- tryCatch({
      table(grepl("Nominator", infoList))[["TRUE"]]
    }, error = function(e) 0)
    if(numNominators > 0){
      nominatorDf <- map(1:numNominators, ~getInfoData(., numNominators, infoList, fields, "Nominator")) %>%
        do.call("rbind.data.frame", .) %>%
        rename_with(function(.x) paste0("Nominator", substr(.x, 1, nchar(.x)-1))) %>%
        mutate(NumberOfNominatorsInGroup=numNominators,
               Year=Year)
    } else {
      nominatorDf <- data.frame(Year=Year, NominatorName=NA, NominatorGender=NA,
                                NominatorProfession=NA, NominatorCity=NA, NominatorCountry=NA, NumberOfNominatorsInGroup=NA)
    }
    
    detailsDf <- inner_join(nomineeDf, nominatorDf, by="Year")
    
  } else {
    detailsDf <- data.frame(Name=NA, Gender=NA, BirthYear=NA, DeathYear=NA, Profession=NA, University=NA, City=NA, Country=NA,
                            Prize=NA, Year=NA, Number=NA, NumberOfNomineesInGroup=NA, Winner=NA, NominatorName=NA, NominatorGender=NA,
                            NominatorProfession=NA, NominatorCity=NA, NominatorCountry=NA, NumberOfNominatorsInGroup=NA)
  }
  
  
  return(detailsDf)
}

# get nominees for the category/year combo
getNominees <- function(year, categoryIndex, remDr) {
  
  # navigate to archive home
  remDr$navigate("https://www.nobelprize.org/nomination/archive/database.php")
  
  # set search parameters and enter search
  remDr <- remDr %>%
    selectCategory(categoryIndex) %>%
    enterYear(year) %>%
    searchArchive()
  
  categoryName <- getCategoryName(remDr)
  
  # get number of nominees found
  results <- remDr$findElement("xpath",
                               "/html/body/main/section[2]/section/article/div/div/p")$getElementText()[[1]]
  numResults <- str_split(results, " ")[[1]][1] %>% as.numeric
  
  if(numResults > 0){
    print(paste(numResults, " nominees for ", categoryName, " in ", year))
    links <- read_html(remDr$getPageSource()[[1]]) %>% 
      html_elements("a") %>% 
      html_attr("href") %>%
      keep(~grepl("show.php?", .x))
    df <- map(links, ~getDetails(remDr, .)) %>%
      do.call("rbind.data.frame", .)
  } else {
    df <- getDetails(remDr, link=NA)
  }
  
  # add category name to output
  df$CategoryName <- categoryName
  
  return(df)
}

getYearInfo <- function(i, years){
  # open remote driver
  remDr <- remoteDriver(port=4445L, browserName="chrome")
  remDr$open()
  
  # get nominees for each year available
  dfNominees <- map(years, ~getNominees(., i, remDr)) %>%
    do.call("rbind.data.frame", .)
  
  local <- dirname(rstudioapi::getSourceEditorContext()$path)
  write.csv(dfNominees, file.path(local, paste0(i, "_nominees.csv")), row.names = F)
  
  print(paste0("Wrote ", nrow(dfNominees), " nominees to file"))
  
  tryCatch({remDr$close()}, error=function(e) e)
  
  return(dfNominees)
}

# main 
categories <- 1:5
years <- 1901:1966

finalDf <- map(categories, ~getYearInfo(., years)) %>%
  do.call("rbind.data.frame", .)

local <- dirname(rstudioapi::getSourceEditorContext()$path)
write.csv(finalDf, file.path(local, "nominees.csv"), row.names = F)

finalDf <- read.csv(file.path(local, "nominees.csv"))
