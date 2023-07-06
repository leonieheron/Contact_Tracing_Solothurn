#' ---
#' title: "Load and clean contact data"
#' author: "Leonie Heron"
#' date: "02/02/2022"
#' ---
#' 

ct_so_002_load_contact = function() {
  
  ############################################################
  #Import data from raw dataset - Nov '20 to Apr'21       ####
  ############################################################
  
  #Data import KP full report 15th Nov onwards
  ct_so_contact_1 <-  read.csv("KP full report 15th Nov onwards.csv", 
                               sep = ",", encoding = "UTF-8")
  
  ############################################################
  #Clean data - Nov '20 to Apr'21                         ####
  ############################################################
  
  #convert NULL response to NA
  ct_so_contact_1[ct_so_contact_1 == "NULL"] <- NA

  
  # Convert character vars to date 
  ct_so_contact_1$KP_Case_Date <- as.Date(ct_so_contact_1$KP_Case_Date) #TODO check strange dates
  ct_so_contact_1$DOB <- as.Date(ct_so_contact_1$DOB) #TODO check strange dates
  ct_so_contact_1$Quar_StartDate <- as.Date(ct_so_contact_1$Quar_StartDate) #TODO check strange dates
  ct_so_contact_1$Quar_EndDate <- as.Date(ct_so_contact_1$Quar_EndDate) #TODO check strange dates
  ct_so_contact_1$KP_Date_contactIP <- as.Date(ct_so_contact_1$KP_Date_contactIP) #TODO check strange dates
  ct_so_contact_1$KP_ReportDate <- as.Date(ct_so_contact_1$KP_ReportDate) #TODO check strange dates
  
  #Recode variables
  ct_so_contact_1$withdrawnquarReason[ct_so_contact_1$withdrawnquarReason == "Ausserkantonal"] <- "Outside of canton"
  ct_so_contact_1$withdrawnquarReason[ct_so_contact_1$withdrawnquarReason == "Falsch positiv"] <- "False positiv"
  ct_so_contact_1$withdrawnquarReason[ct_so_contact_1$withdrawnquarReason == "Kein enger Kontakt"] <- "No close contact"
  ct_so_contact_1$withdrawnquarReason[ct_so_contact_1$withdrawnquarReason == "Letzten 90 Tagen an COIVD-19 erkrankt"] <- "COVID positive in last 90 days"
  
  #keep important vars
  ct_so_contact_1 <- ct_so_contact_1 %>%
    select(c(KP_CaseID, KP_Case_Date, DOB, Gender, Quar_StartDate, Quar_EndDate, 
             withdrawnquarReason, IP_DocID, KP_Date_contactIP, KP_ReportDate))
  
  ############################################################
  #Import data from raw dataset - Apr'21 to Feb '22       ####
  ############################################################
  
  ct_so_contact_2 <- read.csv("contacts_ct_22_LH.csv", 
                              sep = ",", encoding = "UTF-8")
  
  ############################################################
  #Clean data - Apr'21 to Feb '22                         ####
  ############################################################
  
  #convert NULL response to NA
  ct_so_contact_2[ct_so_contact_2 == "NULL"] <- NA
  #rename
  colnames(ct_so_contact_2)[1] <- "KP_Case_Date"
  
  #keep important vars
  ct_so_contact_2 <- ct_so_contact_2 %>%
    select(c(KP_Case_Date, KP_DocID, age, Gender, Quar_StartDate, 
             Quar_EndDate, withdrawnquarReason, IP_DocID, KP_Date_contactIP, 
             Kontact_age, AgeGroup))
  #rename
  names(ct_so_contact_2)[names(ct_so_contact_2) == "ï..KP_Case_Date"] <- "KP_Case_Date"
  dfSummary(ct_so_contact_2)
  names(ct_so_contact_2)
  
  
  #Recode variables
  ct_so_contact_2$withdrawnquarReason[ct_so_contact_2$withdrawnquarReason == "Ausserkantonal"] <- "Outside of canton"
  ct_so_contact_2$withdrawnquarReason[ct_so_contact_2$withdrawnquarReason == "Falsch positiv"] <- "False positiv"
  ct_so_contact_2$withdrawnquarReason[ct_so_contact_2$withdrawnquarReason == "Kein enger Kontakt"] <- "No close contact"
  ct_so_contact_2$withdrawnquarReason[ct_so_contact_2$withdrawnquarReason == "Letzten 90 Tagen an COIVD-19 erkrankt"] <- "COVID positive in last 90 days"
  
  # Convert character vars to date 
  ct_so_contact_2$KP_Case_Date <- as.Date(ct_so_contact_2$KP_Case_Date, format = "%d/%m/%Y")
  ct_so_contact_2$Quar_StartDate <- as.Date(ct_so_contact_2$Quar_StartDate, origin = "1899-12-30") 
  ct_so_contact_2$Quar_EndDate <- as.Date(ct_so_contact_2$Quar_EndDate, origin = "1899-12-30") 
  ct_so_contact_2$KP_Date_contactIP <- as.Date(ct_so_contact_2$KP_Date_contactIP, format = "%d/%m/%Y")
  
  ############################################################
  #Combine data from both periods                         ####
  ############################################################
  ct_so_contact <- full_join(ct_so_contact_1,
                             ct_so_contact_2,
                             suffix = c(".1", ".2")) #combine both
  #remove other datasets
  ct_so_contact_1 <- NULL
  ct_so_contact_2 <- NULL
  ############################################################
  # Clean data                                            ####
  ############################################################
  
  #Recode variables
  ct_so_contact$Gender[ct_so_contact$Gender == "Männlich"] <- "Male"
  ct_so_contact$Gender[ct_so_contact$Gender == "Weiblich"] <- "Female"
  ct_so_contact$Gender[ct_so_contact$Gender == "andere"] <- "Other"
  
  #replace dob before 1912 or after 2023 (not possible) with NA
  ct_so_contact$DOB[ct_so_contact$DOB < as.Date("1910-01-01") | 
                      ct_so_contact$DOB > as.Date("2022-03-01")] <- NA
  #convert  dob to age 
  ct_so_contact$age_from_dob <- floor(as.numeric(difftime(as.Date("2022-02-02"),
                                                 ct_so_contact$DOB, 
                                                 units = "weeks"))/52.25)
  
  #delete dob
  ct_so_contact$DOB <- NULL
  #merge age and age_from_dob
  ct_so_contact <- ct_so_contact %>%
    mutate(age = ifelse(!is.na(age_from_dob), age_from_dob, age))
  #delete age_from_dob
  ct_so_contact$age_from_dob <- NULL
  #remove other age vars
  ct_so_contact$Kontact_age <- NULL
  ct_so_contact$AgeGroup <- NULL
  
  ############################
  # clean date variables

  #replace dates before 2020-01-01 (not possible) with NA for certain date vars
  ct_so_contact$Quar_StartDate[ct_so_contact$Quar_StartDate < as.Date("2020-01-01") | 
                                 ct_so_contact$Quar_StartDate > as.Date("2022-03-01")] <- NA
  ct_so_contact$Quar_EndDate[ct_so_contact$Quar_EndDate < as.Date("2020-01-01") | 
                                 ct_so_contact$Quar_EndDate > as.Date("2022-03-01")] <- NA
  ct_so_contact$KP_ReportDate[ct_so_contact$KP_ReportDate < as.Date("2020-01-01") | 
                               ct_so_contact$KP_ReportDate > as.Date("2022-03-01")] <- NA
  ct_so_contact$KP_Date_contactIP[ct_so_contact$KP_Date_contactIP < as.Date("2020-01-01") | 
                                ct_so_contact$KP_Date_contactIP > as.Date("2022-03-01")] <- NA
  #some dates should be reasonably within 30 days of being registered in the system
  ct_so_contact$Quar_StartDate[ct_so_contact$Quar_StartDate > as.Date(ct_so_contact$KP_Case_Date) + 25 |
                                 ct_so_contact$Quar_StartDate < as.Date(ct_so_contact$KP_Case_Date) - 25] <- NA
  ct_so_contact$Quar_EndDate[ct_so_contact$Quar_EndDate > as.Date(ct_so_contact$KP_Case_Date) + 25 |
                                 ct_so_contact$Quar_EndDate < as.Date(ct_so_contact$KP_Case_Date) - 25] <- NA
  ct_so_contact$KP_ReportDate[ct_so_contact$KP_ReportDate > as.Date(ct_so_contact$KP_Case_Date) + 25 |
                               ct_so_contact$KP_ReportDate < as.Date(ct_so_contact$KP_Case_Date) - 25] <- NA
  ct_so_contact$KP_Date_contactIP[ct_so_contact$KP_Date_contactIP > as.Date(ct_so_contact$KP_Case_Date) + 30 |
                                ct_so_contact$KP_Date_contactIP < as.Date(ct_so_contact$KP_Case_Date) - 30] <- NA
  
  #create var to indicate withdrawal
  ct_so_contact <- ct_so_contact %>%
    mutate(withdrawn = ifelse(!is.na(ct_so_contact$withdrawnquarReason), 
                              TRUE, 
                              FALSE))
  #clean IP doc ID
  AusserK <- grepl('us', ct_so_contact$IP_DocID) | grepl('AK', ct_so_contact$IP_DocID) | grepl('KT', ct_so_contact$IP_DocID) | grepl('Kt', ct_so_contact$IP_DocID)
  ct_so_contact$IP_DocID[AusserK] <- "Ausserkantonal"
  #merge 2 vars for KP ID
  ct_so_contact <- ct_so_contact %>%
    mutate(KP_CaseID = ifelse(!is.na(KP_CaseID), KP_CaseID, KP_DocID))
  #delete KP_DocID
  ct_so_contact$KP_DocID <- NULL
  

  ############################################################
  #Clean dates                                            ####
  ############################################################
  #quarantine start date for contacts should  be within 25 days of the contact date - likely to be typo otherwise
  ct_so_contact$Quar_StartDate[ct_so_contact$Quar_StartDate > as.Date(ct_so_contact$KP_Case_Date) + 25 |
                                      ct_so_contact$Quar_StartDate < as.Date(ct_so_contact$KP_Case_Date) - 25] <- NA
  
  ############################################################
  #remove duplicate rows                                  ####
  ############################################################
  #remove duplicated contacts
  ct_so_contact <- ct_so_contact[!duplicated(ct_so_contact$KP_CaseID), ]
  
  ##############################################
  #Add when VOCs became dominant in Solothurn area
  ct_so_contact <- ct_so_contact %>%
    mutate(VOC_period_contact = ifelse(KP_Case_Date < "2021-02-08", "Pre-VOC",
                                     ifelse(KP_Case_Date < "2021-06-28", "Alpha",
                                            ifelse(KP_Case_Date < "2021-12-27", "Delta",
                                                   "Omicron"))))
  
  
  #remove typos
  ct_so_contact$IP_DocID <- str_replace_all(ct_so_contact$IP_DocID, "[\\\\n]", "")
  #remove contacts before 15 Nov
  ct_so_contact <- ct_so_contact %>%
    filter(KP_Case_Date > as.Date("2020-11-14"))
  
  #add var for week of year
  ct_so_contact$weekyear <- yearweek(ct_so_contact$KP_Case_Date)
  ct_so_contact$weekyear <- as.character(ct_so_contact$weekyear)

  return(as.data.frame(ct_so_contact))
  
}

