#' ---
#' title: "Combine KPIs, dates and cases in solothurn for plots"
#' author: "Leonie Heron"
#' date: "10/02/2022"
#' ---

ct_so_007_combine_KPIs_cases = function(ct_so_index, ct_so_contacts_index, so_cases) {
  

  # #rearrange KPI data to per date
  # #first KPIs from index
  # KPIs_index <- ct_so_index %>%
  #   distinct(IP_CaseID, .keep_all = TRUE) %>%
  #   select(Case_Date, KPI1, KPI2, KPI4, KPI6, KPI7, KPI9, KPI10, KPI11, 
  #          KPI12, KPI13, KPI14, KPI15) %>%
  #   group_by(Case_Date) %>%
  #   summarise(meanKPI1 = mean(KPI1, na.rm = T),
  #             meanKPI2 = mean(KPI2, na.rm = T),
  #             meanKPI4 = mean(KPI4, na.rm = T),
  #             meanKPI6 = mean(KPI6, na.rm = T),
  #             meanKPI7 = mean(KPI7, na.rm = T),
  #             meanKPI9 = mean(KPI9, na.rm = T),
  #             meanKPI10 = mean(KPI10, na.rm = T),
  #             meanKPI11 = mean(KPI11, na.rm = T),
  #             meanKPI12 = mean(KPI12, na.rm = T),
  #             meanKPI13 = mean(KPI13, na.rm = T),
  #             meanKPI14 = mean(KPI14, na.rm = T),
  #             meanKPI15 = mean(KPI15, na.rm = T))
  # 
  # #combine KPIS from index with cases
  # ct_so_KPIs_cases <- left_join(KPIs_index, ct_so_cases, by = c("Case_Date" = "datum"))
  # names(ct_so_KPIs_cases)[names(ct_so_KPIs_cases) == "entries"] <- "cases"
  # rm(KPIs_index)
  # #KPIs from contacts and index cases
  # KPIs_contacts <- ct_so_contacts_index %>%
  #   select(KP_Case_Date, KPI3, KPI5, KPI8) %>%
  #   group_by(KP_Case_Date) %>%
  #   summarise(meanKPI3 = mean(KPI3, na.rm = T),
  #             meanKPI5 = mean(KPI5, na.rm = T),
  #             meanKPI8 = mean(KPI8, na.rm = T))
  # #combine KPIS from contacts with KPIs from index and cases
  # ct_so_KPIs_cases <- left_join(ct_so_KPIs_cases, KPIs_contacts, by = c("Case_Date" = "KP_Case_Date"))
  # rm(KPIs_contacts)
  # ct_so_KPIs_cases$datum <- ct_so_KPIs_cases$Case_Date
  # ct_so_KPIs_cases <- ct_so_KPIs_cases[c("datum", "meanKPI1", "meanKPI2", "meanKPI3", "meanKPI4", "meanKPI5", 
  #                                        "meanKPI6", "meanKPI7", "meanKPI8", "meanKPI9", "meanKPI10", "meanKPI11", 
  #                                        "meanKPI12", "meanKPI13", "meanKPI14", "meanKPI15", "cases", 
  #                                        "cases_7day", "cases_per_100k")]
  # 
  # 
  # 
  # ct_so_KPIs_cases <- ct_so_KPIs_cases %>%
  #  mutate(meanKPI1_7day = round(rollapply(meanKPI1, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI2_7day = round(rollapply(meanKPI2, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI3_7day = round(rollapply(meanKPI3, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI4_7day = round(rollapply(meanKPI4, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI5_7day = round(rollapply(meanKPI5, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI6_7day = round(rollapply(meanKPI6, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI7_7day = round(rollapply(meanKPI7, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI8_7day = round(rollapply(meanKPI8, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI9_7day = round(rollapply(meanKPI9, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI10_7day = round(rollapply(meanKPI10, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI11_7day = round(rollapply(meanKPI11, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         meanKPI12_7day = round(rollapply(meanKPI12, width = 7, FUN=function(x) mean(x, na.rm=TRUE), fill = NA), 2),
  #         cases_per_1k = cases_per_100k/100,
  #         #create var to indicate when cases are above 15/100k
  #         cases_15_100k = as.numeric((cases_per_100k >= 15)),
  #         #create var to indicate when cases are above 25/100k
  #         cases_25_100k = (cases_per_100k >= 25),
  #         #create var to indicate when cases are above 50/100k
  #         cases_50_100k = (cases_per_100k >= 50),
  #         #create var to indicate when cases are above 100/100k
  #         cases_100_100k = (cases_per_100k >= 100))
  # 
  # #create mean kpi per week
  # #generate week variable
  # ct_so_KPIs_cases$weekyear <- format(ct_so_KPIs_cases$datum, "%Y-%U")
  # head(ct_so_KPIs_cases$weekyear)
  # 
  # #generate mean KPIs per week
  # KPIs_week <- ct_so_KPIs_cases %>%
  #   group_by(weekyear) %>%
  #   summarise(meanKPI14week = mean(meanKPI14),
  #             meanKPI15week = mean(meanKPI15))
  # #add mean KPIs per week to dataset
  # ct_so_KPIs_cases <- left_join(ct_so_KPIs_cases, KPIs_week, by = "weekyear")
  # 
  # #add workforce data
  # workforce <- ct_so_index %>%
  #   select(Case_Date, efte)
  # ct_so_KPIs_cases <- left_join(ct_so_KPIs_cases, workforce, by = c("datum" = "Case_Date"))
  # 
  # #cases per workforce availability
  # ct_so_KPIs_cases$caseperwf <- ct_so_KPIs_cases$cases_per_100k/ct_so_KPIs_cases$efte
  # 
  
  
  
  #rearrange KPI data to per date
  #first KPIs from index
  KPIs_index <- ct_so_index %>%
    distinct(IP_CaseID, .keep_all = TRUE) %>%
    select(Case_Date, #KPI1, KPI2, KPI4, KPI6, KPI9, KPI10, KPI11, KPI12, KPI13, 
           KPI14, KPI6,
           KPI7, KPI15, KPI16) 
  
  #combine KPIS from index with cases
  ct_so_KPIs_cases <- left_join(KPIs_index, ct_so_cases, by = c("Case_Date" = "datum"))
  names(ct_so_KPIs_cases)[names(ct_so_KPIs_cases) == "entries"] <- "cases"
  rm(KPIs_index)
  #KPIs from contacts and index cases
  KPIs_contacts <- ct_so_contacts_index %>%
    select(KP_Case_Date, #KPI3, KPI5, 
           KPI8) 
  #combine KPIS from contacts with KPIs from index and cases
  ct_so_KPIs_cases <- left_join(ct_so_KPIs_cases, KPIs_contacts, by = c("Case_Date" = "KP_Case_Date"))
  rm(KPIs_contacts)
  ct_so_KPIs_cases$datum <- ct_so_KPIs_cases$Case_Date
  ct_so_KPIs_cases$Case_Date <- NULL
  
  
  ct_so_KPIs_cases <- ct_so_KPIs_cases %>%
    mutate(cases_per_1k = cases_per_100k/100,
           #create var to indicate when cases are above 15/100k
           cases_15_100k = as.numeric((cases_per_100k >= 15)),
           #create var to indicate when cases are above 25/100k
           cases_25_100k = (cases_per_100k >= 25),
           #create var to indicate when cases are above 50/100k
           cases_50_100k = (cases_per_100k >= 50),
           #create var to indicate when cases are above 100/100k
           cases_100_100k = (cases_per_100k >= 100))
  
  #add workforce data
  workforce <- ct_so_index %>%
    select(Case_Date, efte) %>%
    distinct(.keep_all = TRUE) %>%
    rename(datum = Case_Date)
  ct_so_KPIs_cases <- left_join(ct_so_KPIs_cases, workforce, by = "datum")
  
  #cases per workforce availability
  ct_so_KPIs_cases$caseperwf <- ct_so_KPIs_cases$cases_per_100k/ct_so_KPIs_cases$efte
  
  #calculate KPIs per week
  #first make a year-week variable
  ct_so_KPIs_cases$yearweek <- yearweek(ct_so_KPIs_cases$datum)
  KPIs_weekly <- ct_so_KPIs_cases %>%
    select(yearweek, KPI6, KPI7, KPI8, KPI15, KPI14, KPI16)%>% 
    group_by(yearweek) %>%
    summarise(KPI7week = mean(KPI7, na.rm = TRUE),
              KPI8week = mean(KPI8, na.rm = TRUE),
              KPI6week = mean(KPI6, na.rm = TRUE),
              KPI15week = mean(KPI15, na.rm = TRUE),
              KPI16week = mean(KPI16, na.rm = TRUE),
              KPI14week = mean(KPI14, na.rm = TRUE))
  
  #combine with ct_so_KPIs_cases df
  ct_so_KPIs_cases <- left_join(ct_so_KPIs_cases, KPIs_weekly, by = "yearweek")
  
  ct_so_KPIs_cases <- ct_so_KPIs_cases %>%
    distinct(.keep_all = TRUE)
  ##############################################
  #Add when VOCs became dominant in Solothurn area
  ct_so_KPIs_cases <- ct_so_KPIs_cases %>%
    mutate(VOC_period_KPIs = ifelse(datum < "2021-02-08", "Pre-VOC",
                                     ifelse(datum < "2021-06-28", "Alpha",
                                            ifelse(datum < "2021-12-27", "Delta",
                                                   "Omicron"))))
  ct_so_KPIs_cases$VOC_period_KPIs <- fct_relevel(ct_so_KPIs_cases$VOC_period_KPIs, 
                                              c("Pre-VOC", "Alpha", "Delta", "Omicron"))  

return(as.data.frame(ct_so_KPIs_cases))
}

