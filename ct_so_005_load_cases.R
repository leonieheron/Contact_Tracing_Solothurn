#' ---
#' title: "Load and clean Solothurn case data"
#' author: "Leonie Heron"
#' date: "10/02/2022"
#' ---
#' 

ct_so_005_load_cases = function() {
  
  ############################################
  # Import data from Solothurn csv file ####
  ############################################
  
  #Data downloaded on 10/02/2022 from https://www.covid19.admin.ch/en/epidemiologic/case
  
  
  ct_so_cases <- read.csv("COVID19Cases_CH.csv") #needs updated with latest dates
 #only keep solothurn cases
  ct_so_cases <- ct_so_cases %>%
    filter(geoRegion == "SO") %>%
    select(datum, entries) %>%
    #7-day rolling average
    mutate(cases_7day = round(rollmean(entries, k = 7, fill = NA), 2)) 
  #convert to date format
  ct_so_cases$datum <- as.Date(ct_so_cases$datum, "%d/%m/%Y") 
  #calculate cases per 100,000
  #population of Solothurn taken from 
  #https://www.bfs.admin.ch/bfs/en/home/statistics/regional-statistics/regional-portraits-key-figures/cantons/solothurn.html
  solothurn_pop <- 275247
  ct_so_cases$cases_per_100k <- round(ct_so_cases$cases_7day/solothurn_pop*100000, 2)
  
  return(as.data.frame(ct_so_cases))
  
  
}
