#' ---
#' title: "Load and clean KOF stringency index data"
#' author: "Leonie Heron"
#' date: "02/02/2022"
#' ---
#' 

ct_so_004_load_kof = function() {
  
  ############################################
  # Import data from KOF stringency index ####
  ############################################
  
  kof <- read.csv("KOF_Stringency_index.csv") #needs updated with latest dates
  #keep only SO data
  kof <- kof %>%
    select("date", "ch.kof.stringency.so.stringency_plus") %>%
    mutate(SO = 1) %>%
    rename(kof_num = ch.kof.stringency.so.stringency_plus)
  kof$date <- as.Date(kof$date, "%d/%m/%Y")
  kof$MonthYear <- as.yearmon(kof$date)

  
  return(as.data.frame(kof))
  
  
}
