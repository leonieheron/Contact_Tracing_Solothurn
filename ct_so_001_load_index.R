#' ---
#' title: "Load and clean index data"
#' author: "Leonie Heron"
#' date: "02/02/2022"
#' ---
#' 

ct_so_001_load_index = function() {
  
  #########################################
  #Import data from raw datasets       ####
  #########################################
  
  #Data import Full IP report 15-11-2020 onwards
  ct_so_index <- read.csv("ip_export_feb_22_LH.csv")
  #convert NULL response to NA
  ct_so_index[ct_so_index == "NULL"] <- NA
  #remove variables, not helpful
  ct_so_index <- ct_so_index %>%
    select(-c(TestLocation, TestLocationDetail, TestLaboratory, 
              TestLaboratoryDetail, TestDetectionMethod, pathogenTestResult, 
              Pathogentype, VaccineOther, Action, Disease, ismcaseid, 
              First.Symptoms,Vaccine2, VaccineCompany, VaccineDate1, 
              VaccineDate2))
  ct_so_index$VaccineCompany <- NULL
  #remove variables to be cleaned
  ct_so_index <- ct_so_index %>%
    select(-c(PreECAdipositas, PreECBloodPressure, PreECcancer, PreECDiabetes, 
              PreECHearth, PreECImmunsupression, PreECkidneys, PreECliver, 
              preEClungs, PreExistingCondition)) #preexising conditions
  ct_so_index <- ct_so_index %>%
    select(-c(Employer, Type, Employ_Address, Employ_AddressNo, Employ_PLZ, 
              Employ_Place)) #employer
  ct_so_index <- ct_so_index %>%
    select(-c(SchoolInfo, Class, SchoolAddress, SchoolAddressNo, School_Place, 
              School_PLZ)) #school
  # ct_so_index <- ct_so_index %>%
  #   select(-c(LocationActivity, locationActivityOthers, locationDescription,
  #             locationAddress, LocationAddressNo, locationPLZ, LocationPlace,
  #             LocationCanton, TravelCountry)) #location
  ct_so_index <- ct_so_index %>%
    select(-c(Residence, Residence.Category, Residence.Type, Residence.Name,
              IsolationPlace)) #residence
  # Convert character data to date and time.
  ct_so_index$Case_Date <- as.POSIXct(ct_so_index$Case_Date) 
  # Convert character vars to date 
  ct_so_index$Date.Receipt.Lab <- as.Date(ct_so_index$Date.Receipt.Lab) 
  ct_so_index$Test.Date <- as.Date(ct_so_index$Test.Date) 
  ct_so_index$Isolation_Start <- as.Date(ct_so_index$Isolation_Start)
  ct_so_index$Isolation_End <- as.Date(ct_so_index$Isolation_End) 
  ct_so_index$Date.Start.Symptoms <- as.Date(ct_so_index$Date.Start.Symptoms) 
  #check missing values
  check_Date.Start.Symptoms <- ct_so_index %>% filter(is.na(Date.Start.Symptoms))
  #how many missing symptom values due to lack of symptoms?
  table(is.na(check_Date.Start.Symptoms$NoSymptom))
  #3,813/13,778 (28%) reported that they had no symptoms. 
  ct_so_index$LocationStartDate <- as.Date(ct_so_index$LocationStartDate, origin = "1899-12-30") 
  ct_so_index$LocationEndDate <- as.Date(ct_so_index$LocationEndDate, origin = "1899-12-30") 
  
  #Recode variables
  ct_so_index$Gender <- as.factor(ct_so_index$Gender)
  levels(ct_so_index$Gender) <- c("Other", "Male", "Female")
  ct_so_index$Gender <- as.character(ct_so_index$Gender)
  ct_so_index$Res.Canton[ct_so_index$Res.Canton == "Solothurn"] <- "SO"
  ct_so_index$AnySymptom[ct_so_index$NoSymptom == FALSE] <- "TRUE"
  ct_so_index$AnySymptom[ct_so_index$NoSymptom == TRUE] <- "FALSE"
  ct_so_index$NoSymptom <- NULL
#code variable to indicate that person gave some symptom
  ct_so_index$SpecificSymptom <- "TRUE"
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Sweat)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Cough)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Diarrhea)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Breath)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Temp)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$RapidNausea)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$RapidBreath)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Runnynose)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Sorethroat)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$ARDS)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Pneumonia)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Taste)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Smell)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$General)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$HeartRate)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$OtherClinical)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Oxygen)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$Symptom_Others)] <- NA
  ct_so_index$SpecificSymptom[is.na(ct_so_index$RespiratoryFailure)] <- NA
    #change to numeric
  ct_so_index$locationNumPeople <- as.numeric(ct_so_index$locationNumPeople)
  ct_so_index$LocationNumTotalPeople <- as.numeric(ct_so_index$LocationNumTotalPeople)
  ct_so_index$locationNumSickPeople <- as.numeric(ct_so_index$locationNumSickPeople)
  #####################################################
  # clean vaccine 
  #####################################################
  ct_so_index$vaccine[is.na(ct_so_index$vaccine)] <- FALSE
  #Missing response converted to F
  #####################################################
  # clean symptom variables
  #####################################################
  #group other symptoms and other clinical symptoms
  ct_so_index$Other_symp_group <- FALSE
  ct_so_index$Other_symp_group[ct_so_index$OtherClinical == TRUE | 
                                 !is.na(ct_so_index$Symptom_Others)] <- TRUE
  #group loss of taste and smell
  ct_so_index$loss_taste_smell <- FALSE
  ct_so_index$loss_taste_smell[ct_so_index$Taste == TRUE | 
                                 ct_so_index$Smell == TRUE] <- TRUE
  #group breathlessness and rapid breathing
  ct_so_index$diff_breathing <- FALSE
  ct_so_index$diff_breathing[ct_so_index$Breath == TRUE | 
                                   ct_so_index$RapidBreath == TRUE] <- TRUE
  #####################################################
  # clean activity variables
  #####################################################
  #recode these activities as false - 
  #if they are in work, school or parties, then NoActivity == FALSE
  ct_so_index$NoActivity[ct_so_index$NoActivity == "ARBEIT" |
                           ct_so_index$NoActivity == "PRIVATES FEST" |
                           ct_so_index$NoActivity == "SCHULEN"] <- FALSE
  ct_so_index$AnyActivity[ct_so_index$NoActivity == FALSE] <- "TRUE"
  ct_so_index$AnyActivity[ct_so_index$NoActivity == TRUE] <- "FALSE"
  ct_so_index$NoActivity <- NULL
  #####################################################
  # clean date variables
  #####################################################
  
  #remove observaion with missing case date, no information
  
  ct_so_index <- ct_so_index %>% drop_na(Case_Date)
  
  #replace dates before 2020-01-01 (not possible) with NA for certain date vars

  table(is.na(ct_so_index$Date.Receipt.Lab))
   ct_so_index$Date.Receipt.Lab[ct_so_index$Date.Receipt.Lab < as.Date("2020-01-01") | 
                               ct_so_index$Date.Receipt.Lab > as.Date("2022-03-01")] <- NA
  table(is.na(ct_so_index$Date.Receipt.Lab))
  table(is.na(ct_so_index$Date.Start.Symptoms))
  ct_so_index$Date.Start.Symptoms[ct_so_index$Date.Start.Symptoms < as.Date("2020-01-01") | 
                                  ct_so_index$Date.Start.Symptoms > as.Date("2022-03-01")] <- NA
  table(is.na(ct_so_index$Date.Start.Symptoms))
  ct_so_index$LocationStartDate[ct_so_index$LocationStartDate < as.Date("2020-01-01") | 
                                  ct_so_index$LocationStartDate > as.Date("2022-03-01")] <- NA
  ct_so_index$LocationEndDate[ct_so_index$LocationEndDate < as.Date("2020-01-01") | 
                                ct_so_index$LocationEndDate > as.Date("2022-03-01")] <- NA
  
  #some dates should be reasonably within 25 days of being registered in the system
  ct_so_index$Date.Start.Symptoms[ct_so_index$Date.Start.Symptoms > as.Date(ct_so_index$Case_Date) + 25 |
                                        ct_so_index$Date.Start.Symptoms < as.Date(ct_so_index$Case_Date) - 25] <- NA
  table(is.na(ct_so_index$Date.Start.Symptoms))
  #removed Date.Receipt.Lab if it was 25 days before or after interview date
   ct_so_index$Date.Receipt.Lab[ct_so_index$Date.Receipt.Lab > as.Date(ct_so_index$Case_Date) + 25 |
                                   ct_so_index$Date.Receipt.Lab < as.Date(ct_so_index$Case_Date) - 25] <- NA
   table(is.na(ct_so_index$Test.Date))
  ct_so_index$Test.Date[ct_so_index$Test.Date > as.Date(ct_so_index$Case_Date) + 25 |
                               ct_so_index$Test.Date < as.Date(ct_so_index$Case_Date) - 25] <- NA
  table(is.na(ct_so_index$Test.Date))
  ct_so_index$LocationStartDate[ct_so_index$LocationStartDate > as.Date(ct_so_index$Case_Date) + 25 |
                              ct_so_index$LocationStartDate < as.Date(ct_so_index$Case_Date) - 25] <- NA
  ct_so_index$LocationEndDate[ct_so_index$LocationEndDate > as.Date(ct_so_index$Case_Date) + 25 |
                              ct_so_index$LocationEndDate < as.Date(ct_so_index$Case_Date) - 25] <- NA
  table(is.na(ct_so_index$Isolation_Start))
  ct_so_index$Isolation_Start[ct_so_index$Isolation_Start > as.Date(ct_so_index$Case_Date) + 25 |
                                ct_so_index$Isolation_Start < as.Date(ct_so_index$Case_Date) - 25] <- NA
  table(is.na(ct_so_index$Isolation_Start))
  
  
 
  #create age categories variable
  ct_so_index$age_cat[ct_so_index$age < 18] <- "0-17"                  
  ct_so_index$age_cat[ct_so_index$age >= 18 & ct_so_index$age < 30] <- "18-29"
  ct_so_index$age_cat[ct_so_index$age >= 30 & ct_so_index$age < 40] <- "30-39"
  ct_so_index$age_cat[ct_so_index$age >= 40 & ct_so_index$age < 50] <- "40-49"
  ct_so_index$age_cat[ct_so_index$age >= 50 & ct_so_index$age < 60] <- "50-59"
  ct_so_index$age_cat[ct_so_index$age >= 60 & ct_so_index$age < 70] <- "60-69"
  ct_so_index$age_cat[ct_so_index$age >= 70 & ct_so_index$age < 80] <- "70-79"
  ct_so_index$age_cat[ct_so_index$age >= 80] <- ">80"
  ct_so_index$age_cat <- factor(ct_so_index$age_cat, levels = c("0-17",
                                                                "18-29",
                                                                "30-39",
                                                                "40-49",
                                                                "50-59",
                                                                "60-69",
                                                                "70-79",
                                                                ">80"))
  
  ##############################################
  # social activities 
  
  #create new var of number of social activities reported
  ct_so_index <- ct_so_index %>% add_count(IP.DocID, name = "nActivity")
  ct_so_index$nActivity[is.na(ct_so_index$LocationStartDate) &
                          is.na(ct_so_index$LocationEndDate) &
                          is.na(ct_so_index$LocationActivity) &
                          is.na(ct_so_index$locationActivityOthers) &
                          is.na(ct_so_index$locationDescription) &
                          is.na(ct_so_index$locationAddress) &
                          is.na(ct_so_index$locationDescription) &
                          is.na(ct_so_index$locationDescription) &
                          is.na(ct_so_index$locationDescription)] <- 0
  #create categories of number of social activities reported
  ct_so_index$nActivity_cat[ct_so_index$nActivity == 0] <- "0"    
  ct_so_index$nActivity_cat[ct_so_index$nActivity == 1] <- "1"                  
  ct_so_index$nActivity_cat[ct_so_index$nActivity == 2] <- "2"                  
  ct_so_index$nActivity_cat[ct_so_index$nActivity >= 3] <- "3+"                  
  
  ct_so_index$locationReceivedNotification[is.na(ct_so_index$locationReceivedNotification)] <- FALSE
  
  
  ##############################################
  #Source of infection
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Andere" | 
                                ct_so_index$Locale_infected == "Andere" ] <- "Other"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "gesundheitseinrichtung" | 
                                ct_so_index$Locale_infected == "medizinal" | 
                                ct_so_index$Locale_infected == "Gesundheitseinrichtung"] <- "Healthcare setting"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Keine Vermutung" | 
                                ct_so_index$Locale_infected == "keine" | 
                                ct_so_index$Locale_infected == "nein" |
                                ct_so_index$Locale_infected == "unbekannt" |
                                ct_so_index$Locale_infected == "Unbekannt"] <- "Unknown"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "privates Fest" | 
                                ct_so_index$Locale_infected == "Privates Fest" ] <- "Private party"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "bar" | 
                                ct_so_index$Locale_infected == "disco" | 
                                ct_so_index$Locale_infected == "Bar / Disco" |
                                ct_so_index$Locale_infected == "In Disco / Club" |
                                ct_so_index$Locale_infected == "In Bar / Restaurant"] <- "Bar/Disco/Restaurant"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "demonstration" | 
                                ct_so_index$Locale_infected == "Bei Demonstration / Veranstaltung"] <- "Demonstration"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Als Medizinal- oder Pflegepersonal"] <- "As healthcare worker"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "arbeit" | 
                                ct_so_index$Locale_infected == "Bei der Arbeit"] <- "Work"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Armee / Zivilschutz"] <- "Army/Civil Service"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Asylzentrum"] <- "Asylum centre"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Sportveranstaltung (Zuschauer)"] <- "Sport (spectator)"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Religiöse Versammlung / Beerdigung"] <- "Religious gathering/Funeral"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "arbeit"] <- "Work"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Chor / Gesangsverein / Orchester"] <- "Choir/Orchestra"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Läden / Markt / ÖV"] <- "Shopping/Public transport"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "arbeit"] <- "Work"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Fitnesscenter / Sportclub / Training"] <- "Gym/sport"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "Kino / Theater / Konzert"] <- "Cinema/Theatre/Concert"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "familie" | 
                                ct_so_index$Locale_infected ==  "fam" | 
                                ct_so_index$Locale_infected ==  "fami" |
                                ct_so_index$Locale_infected ==  "Eigener Haushalt" |
                                ct_so_index$Locale_infected == "Treffen mit Familie / Freunden"] <- "Family or friends"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "flugzeug" | 
                                ct_so_index$Locale_infected == "reise" | 
                                ct_so_index$Locale_infected == "Flugzeug / Reise" |
                                ct_so_index$Locale_infected == "Reise in ein Risikoland / nach Hause aus einem Risikoland (gemäss Liste BAG)"] <- "Travel"
  ct_so_index$Locale_infected[grepl("Reise", ct_so_index$Locale_infected)] <- "Travel"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "markt"] <- "Shopping"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "menschenansammlung"] <- "Crowds of people"
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "schulen" | 
                                ct_so_index$Locale_infected == "schule" | 
                                ct_so_index$Locale_infected == "In Schulen / Kindergarten / Krippe"] <- "School"  
  ct_so_index$Locale_infected[grepl("Hotel", ct_so_index$Locale_infected)] <- "Group accommodation"
  ct_so_index$Locale_infected[grepl("Lager", ct_so_index$Locale_infected)] <- "Group accommodation"
  ct_so_index$Locale_infected[grepl("Beerdigung", ct_so_index$Locale_infected)] <- "Religious gathering/Funeral"
  ct_so_index$Locale_infected[grepl("Markt", ct_so_index$Locale_infected)] <- "Shopping/Public transport"
  
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "anlass" | 
                                ct_so_index$Locale_infected == "k" |
                                ct_so_index$Locale_infected == "ke" |
                                ct_so_index$Locale_infected == "eig" |
                                ct_so_index$Locale_infected == "ges"] <- NA
  ct_so_index$Locale_infected[ct_so_index$Locale_infected == "In spontaner Menschenansammlung"] <- "Spontaneous gathering"
  ct_so_index$Locale_infected[grepl("Friseure", ct_so_index$Locale_infected)] <- "Personal care with close contact (e.g., hairdresser)"
  #Recode Locale_infected
  ct_so_index <- ct_so_index %>%
    mutate(Locale_infected = ifelse(Locale_infected == "Family or friends", "Family or friends",
                                    ifelse(Locale_infected == "Unknown", "Unknown",
                                           ifelse(Locale_infected == "School", "School",
                                                  ifelse(Locale_infected == "Work", "Work",
                                                         ifelse(Locale_infected == "Shopping/Public transport", "Shopping/Public transport",
                                                                ifelse(is.na(Locale_infected), "Missing",
                                                                       "Other")))))))
  #rename veriables
  colnames(ct_so_index)[colnames(ct_so_index) == "IP.DocID"] <- "IP_CaseID"
  colnames(ct_so_index)[colnames(ct_so_index) == "Date.Start.Symptoms"] <- "DateStartSymptoms"
  
  ##############################################
  #Add when VOCs became dominant in Solothurn area
  ct_so_index <- ct_so_index %>%
    mutate(VOC_period_index = ifelse(Case_Date < "2021-02-08", "Pre-VOC",
                                     ifelse(Case_Date < "2021-06-28", "Alpha",
                                            ifelse(Case_Date < "2021-12-27", "Delta",
                                                   "Omicron"))))
  ct_so_index$VOC_period_index <- fct_relevel(ct_so_index$VOC_period_index, 
                                              c("Pre-VOC", "Alpha", "Delta", "Omicron"))
  
  #add var for week of year
  ct_so_index$weekyear <- yearweek(ct_so_index$Case_Date)
  ct_so_index$weekyear <- as.character(ct_so_index$weekyear)
  
  
           
  ##############################################
  #Generate KPIs
  #########################
  # KPI 1
  #########################
  #1. Time between case symptom onset and diagnosis of the index case ("diagnostic time") 
  
  ct_so_index$KPI1 <- as.numeric(difftime(ct_so_index$Date.Receipt.Lab, 
                                          ct_so_index$DateStartSymptoms, #put variable expected to happen first in 2nd place
                                          unit = "days"))
  #########################
  # KPI 2
  #########################
  #2. Time between testing and receipt of test
  
  ct_so_index$KPI2 <- as.numeric(difftime(ct_so_index$Date.Receipt.Lab, 
                                          ct_so_index$Test.Date, unit = "days"))
  
  ct_so_index$Date.Receipt.Lab[ct_so_index$KPI2 < 0] <- NA #remove strange dates
  ct_so_index$Test.Date[ct_so_index$KPI2 < 0] <- NA
  ct_so_index$KPI2[is.na(ct_so_index$Test.Date)] <- NA
  
  #########################
  # KPI 4
  #########################
  #4. Time between test laboratory results and isolation of index case
  
  ct_so_index$KPI4 <- as.numeric(difftime(ct_so_index$Date.Receipt.Lab,
                                          ct_so_index$Isolation_Start, unit = "days"))
  #########################
  # KPI 6
  #########################
  #6. test results returned within 24 hours
  ct_so_index <- ct_so_index %>%
    mutate(KPI6 = ifelse(as.Date(ct_so_index$Date.Receipt.Lab) <= as.Date(ct_so_index$Test.Date) + 1, 
                         1, 0))
  
  
  #########################
  # KPI 7
  #########################
  #7. index cases contacted within 24 hours of the test result
  ct_so_index <- ct_so_index %>%
    mutate(KPI7 = ifelse(as.Date(ct_so_index$Case_Date) <= as.Date(ct_so_index$Date.Receipt.Lab) + 1, 
                         1, 0))

  #########################
  # KPI 10
  #########################
  #10.	Proportion tested and interviewed within 3 days of onset of symptoms
  
  ct_so_index$KPI10 <- NA
  ct_so_index$kpi10_tmp <- as.Date(ct_so_index$Case_Date) - as.Date(ct_so_index$DateStartSymptoms) + 1
  ct_so_index$KPI10[ct_so_index$AnySymptom == TRUE & ct_so_index$kpi10_tmp <= 3] <- 1
  ct_so_index$KPI10[ct_so_index$AnySymptom == TRUE & ct_so_index$kpi10_tmp > 3] <- 0
  ct_so_index$kpi10_tmp <- NULL
 

  #########################
  # KPI 12
  #########################
  #12. Index cases who use the app
  ct_so_index <- ct_so_index %>%
    mutate(KPI12 = as.numeric(Appavailable))
  
  #########################
  # KPI 13
  #########################
  #13. Number of people working in the contact tracing team
  #upload workforce dara
  workforce <- read.csv("workforce.csv", header = TRUE, 
                        col.names = c("month", "tracers", "efte"))
  workforce$month <- as.yearmon(as.Date(paste0("01-", workforce$month), 
                                        format = "%d-%b-%y"), "%Y %m")
  #merge into index df
  ct_so_index$Case_Date_month <- as.yearmon(ct_so_index$Case_Date, "%Y %m") #create month var
  ct_so_index <- left_join(ct_so_index, workforce, by = c("Case_Date_month" = "month"))
  ct_so_index$KPI13 <- ct_so_index$efte
  
  #########################
  # KPI 14
  #########################
  #14. Percentage of new cases who had been previously identified as contacts
  
  #read csv file which links index cases with their contact ID if available
  conversion_ct <- read.csv("conversion_ct_feb_22.csv", header = TRUE,
                            col.names = c("IP.DocID","Isolation.Starts",
                                          "IP.Test.Date","IP_Vaccinated",
                                          "KP.DocID", "KP_Vaccinated",
                                          "Quarantine.Starts", "Quarantine.Ends",
                                          "Test.Date.Dur.Quar","SYS.Test.Taken.Dur.Quar",
                                          "IP_Vaccine_Company","IP_Vaccine1_Date",
                                          "IP_Vaccine2_date","KP_Vaccine_Company",
                                          "KP_Vaccine1_Date","KP_Vaccine2_date"))
  conversion_ct2 <- read.csv("KP_to_IP.csv", header = TRUE,
                             col.names = c("IP.DocID","Isolation.Starts",
                                           "KP.DocID","IP.Test.Date",
                                           "Quarantine.Starts","Quarantine.Ends",
                                           "Test.Date.Dur.Quar",
                                           "SYS.Test.Taken.Dur.Quar"))
  
  conversion_ct3 <- readxl::read_xlsx("ip_from_kp_maytoaugust.xlsx")
  conversion_ct3 <- conversion_ct3[ ,1]
  names(conversion_ct3) <- c("IP.DocID")
  suppressWarnings(conversion_ct$IP.DocID <- as.integer(conversion_ct$IP.DocID))
  suppressWarnings(conversion_ct3$IP.DocID <- as.integer(conversion_ct3$IP.DocID))
  conversion_ct1_2 <- bind_rows(conversion_ct,conversion_ct2)
  conversion_ct <- bind_rows(conversion_ct1_2,conversion_ct3)
  conversion_ct <- conversion_ct[-is.na(conversion_ct$IP.DocID),]
  ct_so_index$KPI14 <- ifelse(ct_so_index$IP_CaseID %in% conversion_ct$IP.DocID, TRUE, FALSE)

  #########################
  # KPI 15
  #########################
  #15. Percentage of new cases who had been previously identified as contacts and
  #under quarantine at the time of onset of symptoms or if asymptomatic, at first
  #positive test with immediate initiation of isolation
  #merge conversion ct with index df
  conversion_ct$Quarantine.Starts <- as.Date(conversion_ct$Quarantine.Starts)
  ct_so_index <- left_join(ct_so_index, conversion_ct, by = c("IP_CaseID" = "IP.DocID"), suffix = c("", ".prevcontact"))
  #remove unnecessary variables
  ct_so_index <- ct_so_index %>%
    select(-c("IP_Vaccinated","KP_Vaccinated","Quarantine.Ends","Test.Date.Dur.Quar",
              "SYS.Test.Taken.Dur.Quar","IP_Vaccine_Company","IP_Vaccine1_Date",
              "IP_Vaccine2_date","KP_Vaccine_Company","KP_Vaccine1_Date","KP_Vaccine2_date"))
  #variable to indicate if under quarantine at time of onset of symptoms, if any
  ct_so_index$KPI15a <- ifelse(ct_so_index$Quarantine.Starts <= ct_so_index$DateStartSymptoms, TRUE, FALSE)
  #variable to indicate if in quarantine before test taken if asymptomatic
  ct_so_index$KPI15b <- ifelse((ct_so_index$Quarantine.Starts <= ct_so_index$Test.Date) & is.na(ct_so_index$DateStartSymptoms), TRUE, FALSE)
  #variable to indicate if under quarantine at the time of onset of symptoms
  #OR if asymptomatic,
  #started quarantine before or on same day as taking a test
  ct_so_index$KPI15c <- ifelse(ct_so_index$KPI15a == TRUE | ct_so_index$KPI15b == TRUE, TRUE, FALSE)
  #proportion of new cases who had been previously identified as contacts AND
  #under quarantine at the time of onset of symptoms
  #OR if asymptomatic,
  #started quarantine before or on same day as taking a test
  ct_so_index$KPI15[ct_so_index$KPI14 == FALSE] <- NA
  ct_so_index$KPI15[ct_so_index$KPI14 == TRUE] <- 0
  ct_so_index$KPI15[ct_so_index$KPI15c == TRUE] <- 1
  ct_so_index$KPI15a <- NULL
  ct_so_index$KPI15b <- NULL
  ct_so_index$KPI15c <- NULL


  #########################
  # KPI 16
  #########################
  #14. Percentage of index cases who were in quarantine by day of test
  
  #calculate days between test and isolation start
  ct_so_index$days_test_iso <- ct_so_index$Test.Date - ct_so_index$Isolation_Start
  
  #create indicator for starting isolation before, after or on same day
  ct_so_index$days_test_iso_text[ct_so_index$days_test_iso > 0] <- "Started isolation before test"
  ct_so_index$days_test_iso_text[ct_so_index$days_test_iso == 0] <- "Started isolation on day of test"
  ct_so_index$days_test_iso_text[ct_so_index$days_test_iso < 0] <- "Started isolation after test"
  
  #create kpi 16
  ct_so_index$KPI16 <- NA
  ct_so_index$KPI16[ct_so_index$days_test_iso_text == "Started isolation before test" |
                      ct_so_index$days_test_iso_text == "Started isolation on day of test"] <- 1
  ct_so_index$KPI16[ct_so_index$days_test_iso_text == "Started isolation after test"] <- 0
  
  ct_so_index$days_test_iso <- NULL
  ct_so_index$days_test_iso_text <- NULL

  return(as.data.frame(ct_so_index))}
