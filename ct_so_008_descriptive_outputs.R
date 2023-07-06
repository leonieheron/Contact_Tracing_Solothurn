#' ---
#' title: "Descriptive tables of index cases and contacts"
#' author: "Leonie Heron"
#' date: "04/02/2022"
#' ---

ct_so_008_descriptive_outputs = function(ct_so_index, ct_so_contact, ct_so_contacts_index) {
  dir_create <- "./output/tables"
  if (!dir.exists(dir_create)){
    dir.create(dir_create)
  } 
  #########################
  # Descriptive overview of index cases and contacts
  # Table overview
  #########################
  #df of unique index cases
  ct_so_index_unique <- distinct(ct_so_index, IP_CaseID, .keep_all = TRUE)
  #age and gender
  age_table <- ct_so_index_unique %>% tabyl(age_cat) %>% 
    rename(Level = age_cat) %>% 
    arrange(desc(n)) %>% 
    mutate(Variable = NA)
  age_table$Level <- as.character(age_table$Level)
  age_table[1, "Variable"] <- "Age (years)"
  age_table$Level[is.na(age_table$Level)] <- "Missing"
  gender_table <- ct_so_index_unique %>% tabyl(Gender) %>% 
    rename(Level = Gender) %>%  
    arrange(desc(n)) %>% 
    mutate(Variable = NA)
  gender_table[1, "Variable"] <- "Gender"
  gender_table$Level[is.na(gender_table$Level)] <- "Missing"
  
  #symptoms
  
  #sweat
  sweat_table <- ct_so_index_unique %>% tabyl(Sweat) %>% 
    rename(Level = Sweat) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  sweat_table["Variable"] <- "Sweating"
  sweat_table$Level[is.na(sweat_table$Level)] <- "Missing"
  sweat_table <- sweat_table[sweat_table$Level == TRUE,]
  #cough
  cough_table <- ct_so_index_unique %>% tabyl(Cough) %>% 
    rename(Level = Cough) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  cough_table["Variable"] <- "Cough"
  cough_table$Level[is.na(cough_table$Level)] <- "Missing"
  cough_table <- cough_table[cough_table$Level == TRUE,]
  #diarrhea
  diar_table <- ct_so_index_unique %>% tabyl(Diarrhea) %>% 
    rename(Level = Diarrhea) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  diar_table["Variable"] <- "Diarrhea"
  diar_table$Level[is.na(diar_table$Level)] <- "Missing"
  diar_table <- diar_table[diar_table$Level == TRUE,]
  #breath
  breath_table <- ct_so_index_unique %>% tabyl(diff_breathing) %>% 
    rename(Level = diff_breathing) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  breath_table["Variable"] <- "Difficulty breathing or shortness of breath"
  breath_table$Level[is.na(breath_table$Level)] <- "Missing"
  breath_table <- breath_table[breath_table$Level == TRUE,]
  #temp
  temp_table <- ct_so_index_unique %>% tabyl(Temp) %>% 
    rename(Level = Temp) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  temp_table["Variable"] <- "Fever"
  temp_table$Level[is.na(temp_table$Level)] <- "Missing"
  temp_table <- temp_table[temp_table$Level == TRUE,]
  #rapidnausea
  nausea_table <- ct_so_index_unique %>% tabyl(RapidNausea) %>% 
    rename(Level = RapidNausea) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  nausea_table["Variable"] <- "Sudden nausea"
  nausea_table$Level[is.na(nausea_table$Level)] <- "Missing"
  nausea_table <- nausea_table[nausea_table$Level == TRUE,]
  #runnynose
  runny_table <- ct_so_index_unique %>% tabyl(Runnynose) %>% 
    rename(Level = Runnynose) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  runny_table["Variable"] <- "Runny nose"
  runny_table$Level[is.na(runny_table$Level)] <- "Missing"
  runny_table <- runny_table[runny_table$Level == TRUE,]
  #sorethroat
  throat_table <- ct_so_index_unique %>% tabyl(Sorethroat) %>% 
    rename(Level = Sorethroat) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  throat_table["Variable"] <- "Sore throat"
  throat_table$Level[is.na(throat_table$Level)] <- "Missing"
  throat_table <- throat_table[throat_table$Level == TRUE,]
  #ARDS
  ards_table <- ct_so_index_unique %>% tabyl(ARDS) %>% 
    rename(Level = ARDS) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  ards_table["Variable"] <- "Acute respiratory distress"
  ards_table$Level[is.na(ards_table$Level)] <- "Missing"
  ards_table <- ards_table[ards_table$Level == TRUE,]
  #pneumonia
  pneu_table <- ct_so_index_unique %>% tabyl(Pneumonia) %>% 
    rename(Level = Pneumonia) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  pneu_table["Variable"] <- "Pneumonia"
  pneu_table$Level[is.na(pneu_table$Level)] <- "Missing"
  pneu_table <- pneu_table[pneu_table$Level == TRUE,]
  #taste/smell
  taste_table <- ct_so_index_unique %>% tabyl(loss_taste_smell) %>% 
    rename(Level = loss_taste_smell) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  taste_table["Variable"] <- "Loss of taste or smell"
  taste_table$Level[is.na(taste_table$Level)] <- "Missing"
  taste_table <- taste_table[taste_table$Level == TRUE,]
  #general
  gen_table <- ct_so_index_unique %>% tabyl(General) %>% 
    rename(Level = General) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  gen_table["Variable"] <- "General malaise"
  gen_table$Level[is.na(gen_table$Level)] <- "Missing"
  gen_table <- gen_table[gen_table$Level == TRUE,]
  #heartrate
  HR_table <- ct_so_index_unique %>% tabyl(HeartRate) %>% 
    rename(Level = HeartRate) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  HR_table["Variable"] <- "Heart rate"
  HR_table$Level[is.na(HR_table$Level)] <- "Missing"
  HR_table <- HR_table[HR_table$Level == TRUE,]
  #oxygen
  oxy_table <- ct_so_index_unique %>% tabyl(Oxygen) %>% 
    rename(Level = Oxygen) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  oxy_table["Variable"] <- "Oxygen required"
  oxy_table$Level[is.na(oxy_table$Level)] <- "Missing"
  oxy_table <- oxy_table[oxy_table$Level == TRUE,]
  #respfailure
  respf_table <- ct_so_index_unique %>% tabyl(RespiratoryFailure) %>% 
    rename(Level = RespiratoryFailure) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  respf_table["Variable"] <- "Respiratory failure"
  respf_table$Level[is.na(respf_table$Level)] <- "Missing"
  respf_table <- respf_table[respf_table$Level == TRUE,]
  #other
  other_table <- ct_so_index_unique %>% tabyl(Other_symp_group) %>% 
    rename(Level = Other_symp_group) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  other_table["Variable"] <- "Other symptoms"
  other_table$Level[is.na(other_table$Level)] <- "Missing"
  other_table <- other_table[other_table$Level == TRUE,]
  #no symptoms
  none_table <- ct_so_index_unique %>% tabyl(AnySymptom) %>% 
    rename(Level = AnySymptom) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  none_table$Level[is.na(none_table$Level)] <- "Missing"
  none_table <- none_table[none_table$Level != TRUE,]
  none_table[1, "Variable"] <- "No symptoms reported"
  none_table[2, "Variable"] <- "Missing"
  
  
  
  #bind symp tables together
  symp_table <- bind_rows(sweat_table, cough_table, diar_table, breath_table, 
                          temp_table, nausea_table, 
                          runny_table, throat_table, ards_table,  pneu_table,  
                          taste_table, gen_table,  HR_table, 
                          oxy_table,  respf_table,  
                          other_table, none_table) 
  symp_table$Level <- symp_table$Variable
  symp_table$Variable <- NA
  symp_table <- symp_table %>% arrange(desc(n))
  symp_table <- symp_table %>% 
    arrange(factor(Level, levels = c("No symptoms reported","Cough","Runny nose",
                                     "Sore throat","Fever","Sweating",
                                     "General malaise","Other symptoms",
                                     "Loss of taste or smell","Sudden nausea",
                                     "Diarrhea",
                                     "Difficulty breathing or shortness of breath",
                                     "Heart rate","Pneumonia",
                                     "Acute respiratory distress",
                                     "Oxygen required","Respiratory failure",
                                     "Missing")))
  symp_table$Variable[1] <- "Symptoms"
  
  
  symp_order <- symp_table$Level #for second table of characteristics
  #add missing data
  #remove temp tables
  rm(sweat_table, cough_table, diar_table, breath_table, 
     temp_table, nausea_table, 
     runny_table, throat_table, ards_table,  pneu_table,  
     taste_table, gen_table,  HR_table, 
     oxy_table,  respf_table,  
     other_table, none_table)
  
  #vaccine status
  vax_table <- ct_so_index_unique %>% tabyl(vaccine) %>% 
    rename(Level = vaccine) %>% mutate(Variable = NA)
  vax_table <- vax_table[match(c(TRUE, FALSE, NA), vax_table$Level), ] 
  vax_table[1, "Variable"] <- "Vaccinated (2 doses)"
  vax_table <- vax_table[1:2,] 
  vax_table$Level <- as.character(vax_table$Level)
  
  #bind tables together
  index_table <- bind_rows(age_table, gender_table, symp_table, vax_table) 
  rm(age_table, gender_table, symp_table, vax_table) #remove
  #add row for total
  total <- length(unique(ct_so_index_unique$IP_CaseID)) #total unique index
  index_table <- rbind(c(NA, total, 1, 1, NA), index_table) 
  
  
  index_table <- index_table[, c(5, 1, 2, 3)]
  index_table[1,1] <- "Total"
  index_table$percent <- paste0(round((index_table$percent*100), 1), "%")
  index_table$percent[index_table$Variable == "Total"] <- ""
  index_table$n <- prettyNum(index_table$n, big.mark = ",")
  index_table[index_table == "TRUE"] <- "True"
  index_table[index_table == "FALSE"] <- "False"
  names(index_table)[names(index_table) == "percent"] <- "%"
  index_table$Level[index_table$Level == "Personal care with close contact (e.g., hairdresser)"] <- "Personal care"
  #prepare column that can be added to index_voc_table
  all_index <- index_table[,2:4]
  all_index$`Entire period` <- paste0(all_index$`%`, " (", all_index$n, ")")
  all_index$n <- NULL
  all_index$`%` <- NULL
  
  #########################
  # Descriptive overview of index cases and contacts by VOC period
  # Table of index cases by voc period
  #########################
  
  #age 
  age_table <- ct_so_index_unique %>% tabyl(age_cat, VOC_period_index) %>%
    rename(Level = age_cat) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  age_table$variable <- NA
  age_table$variable[1] <- "Age (years)"
  #gender
  gender_table <- ct_so_index_unique %>% tabyl(Gender, VOC_period_index) %>% 
    rename(Level = Gender) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  gender_table$variable <- NA
  gender_table$variable[1] <- "Gender"
  gender_table$Level <- as.character(gender_table$Level)
  
  #symptoms
  #sweat
  sweat_table <- ct_so_index_unique %>% tabyl(Sweat, VOC_period_index) %>% 
    rename(Level = Sweat) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  sweat_table$variable <- "Sweating"
  sweat_table <- sweat_table[sweat_table$Level == TRUE & 
                               complete.cases(sweat_table),]
  sweat_table$Level <- NULL
  #cough
  cough_table <- ct_so_index_unique %>% tabyl(Cough, VOC_period_index) %>% 
    rename(Level = Cough) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  cough_table$variable <- "Cough"
  cough_table <- cough_table[cough_table$Level == TRUE & 
                               complete.cases(cough_table),]
  cough_table$Level <- NULL
  #diarrhea
  diar_table <- ct_so_index_unique %>% tabyl(Diarrhea, VOC_period_index) %>% 
    rename(Level = Diarrhea) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  diar_table$variable <- "Diarrhea"
  diar_table <- diar_table[diar_table$Level == TRUE  & 
                             complete.cases(diar_table),]
  diar_table$Level <- NULL
  #breath
  breath_table <- ct_so_index_unique %>% tabyl(diff_breathing, VOC_period_index) %>% 
    rename(Level = diff_breathing) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  breath_table$variable <- "Difficulty breathing or shortness of breath"
  breath_table <- breath_table[breath_table$Level == TRUE & 
                                 complete.cases(breath_table),]
  breath_table$Level <- NULL
  #temp
  temp_table <- ct_so_index_unique %>% tabyl(Temp, VOC_period_index) %>% 
    rename(Level = Temp) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  temp_table$variable <- "Fever"
  temp_table <- temp_table[temp_table$Level == TRUE & 
                             complete.cases(temp_table),]
  temp_table$Level <- NULL
  #rapidnausea
  nausea_table <- ct_so_index_unique %>% tabyl(RapidNausea, VOC_period_index) %>% 
    rename(Level = RapidNausea) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  nausea_table$variable <- "Sudden nausea"
  nausea_table <- nausea_table[nausea_table$Level == TRUE & 
                                 complete.cases(nausea_table),]
  nausea_table$Level <- NULL
  #runnynose
  runny_table <- ct_so_index_unique %>% tabyl(Runnynose, VOC_period_index) %>% 
    rename(Level = Runnynose) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  runny_table$variable <- "Runny nose"
  runny_table <- runny_table[runny_table$Level == TRUE & 
                               complete.cases(runny_table),]
  runny_table$Level <- NULL
  #sorethroat
  throat_table <- ct_so_index_unique %>% tabyl(Sorethroat, VOC_period_index) %>% 
    rename(Level = Sorethroat) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  throat_table$variable <- "Sore throat"
  throat_table <- throat_table[throat_table$Level == TRUE & 
                                 complete.cases(throat_table),]
  throat_table$Level <- NULL
  #ARDS
  ards_table <- ct_so_index_unique %>% tabyl(ARDS, VOC_period_index) %>% 
    rename(Level = ARDS) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  ards_table$variable <- "Acute respiratory distress"
  ards_table <- ards_table[ards_table$Level == TRUE & 
                             complete.cases(ards_table),]
  ards_table$Level <- NULL
  #pneumonia
  pneu_table <- ct_so_index_unique %>% tabyl(Pneumonia, VOC_period_index) %>% 
    rename(Level = Pneumonia) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  pneu_table$variable <- "Pneumonia"
  pneu_table <- pneu_table[pneu_table$Level == TRUE & 
                             complete.cases(pneu_table),]
  pneu_table$Level <- NULL
  #taste
  taste_table <- ct_so_index_unique %>% tabyl(loss_taste_smell, VOC_period_index) %>% 
    rename(Level = loss_taste_smell) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  taste_table$variable <- "Loss of taste or smell"
  taste_table <- taste_table[taste_table$Level == TRUE & 
                               complete.cases(taste_table),]
  taste_table$Level <- NULL
  #general
  gen_table <- ct_so_index_unique %>% tabyl(General, VOC_period_index) %>% 
    rename(Level = General) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  gen_table$variable <- "General malaise"
  gen_table <- gen_table[gen_table$Level == TRUE & 
                           complete.cases(gen_table),]
  gen_table$Level <- NULL
  #heartrate
  HR_table <- ct_so_index_unique %>% tabyl(HeartRate, VOC_period_index) %>% 
    rename(Level = HeartRate) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  HR_table$variable <- "Heart rate"
  HR_table <- HR_table[HR_table$Level == TRUE & 
                         complete.cases(HR_table),]
  HR_table$Level <- NULL
  #oxygen
  oxy_table <- ct_so_index_unique %>% tabyl(Oxygen, VOC_period_index) %>% 
    rename(Level = Oxygen) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  oxy_table$variable <- "Oxygen required"
  oxy_table <- oxy_table[oxy_table$Level == TRUE & 
                           complete.cases(oxy_table),]
  oxy_table$Level <- NULL
  #respfailure
  respf_table <- ct_so_index_unique %>% tabyl(RespiratoryFailure, VOC_period_index) %>% 
    rename(Level = RespiratoryFailure) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  respf_table$variable <- "Respiratory failure"
  respf_table <- respf_table[respf_table$Level == TRUE & 
                               complete.cases(respf_table),]
  respf_table$Level <- NULL
  #other
  other_table <- ct_so_index_unique %>% tabyl(Other_symp_group, VOC_period_index) %>% 
    rename(Level = Other_symp_group) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  other_table$variable <- "Other symptoms"
  other_table <- other_table[other_table$Level == TRUE & 
                               complete.cases(other_table),]
  other_table$Level <- NULL
  #no symptoms
  none_table <- ct_so_index_unique %>% tabyl(AnySymptom, VOC_period_index) %>% 
    rename(Level = AnySymptom) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  none_table <- subset(none_table, none_table$Level == FALSE | is.na(none_table$Level))
  none_table[1, "variable"] <- "No symptoms reported"
  none_table[2, "variable"] <- "Missing"
  none_table$Level <- NULL
  
  #bind symp tables together
  symp_table <- bind_rows(sweat_table, cough_table, diar_table, breath_table, 
                          temp_table, nausea_table, 
                          runny_table, throat_table, ards_table,  pneu_table,  
                          taste_table, gen_table,  HR_table, 
                          oxy_table,  respf_table,  
                          other_table, none_table) 
  symp_table <- symp_table[match(symp_order, symp_table$variable), ] 
  symp_table <- symp_table[!is.na(symp_table$Alpha),]
  rm(sweat_table, cough_table, diar_table, breath_table, temp_table, #remove
     nausea_table, runny_table, throat_table, ards_table,  
     pneu_table,  taste_table, gen_table,  HR_table, oxy_table,  
     respf_table,  other_table, none_table) 
  symp_table$Level <- symp_table$variable
  symp_table$variable <- NA
  symp_table$variable[1] <-  "Symptoms"
  
  #vaccine status
  vax_table <- ct_so_index_unique %>% tabyl(vaccine, VOC_period_index) %>% 
    rename(Level = vaccine) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  vax_table <- vax_table[match(c(TRUE, FALSE, NA), vax_table$Level), ] 
  vax_table$variable <- NA
  vax_table$variable[1] <- "Vaccinated (2 doses)"
  vax_table$Level <- as.character(vax_table$Level)
  vax_table <- vax_table[1:2,]
  
  #combine tables
  index_voc_table <- bind_rows(age_table, gender_table, symp_table, vax_table) 
  rm(age_table, gender_table, symp_table, vax_table) #remove
  
  #reorder cols
  index_voc_table <- index_voc_table %>% select("variable", "Level", "Pre-VOC", 
                                                "Alpha", "Delta", "Omicron")
  #add row for totals
  VOC_table <- ct_so_index_unique %>% tabyl(VOC_period_index) 
  Pre_VOC_total <-  prettyNum(VOC_table[1, 2], big.mark = ",")
  Alpha_total <- prettyNum(VOC_table[2, 2], big.mark = ",")
  Delta_total <- prettyNum(VOC_table[3, 2], big.mark = ",")
  Omicron_total <- prettyNum(VOC_table[4, 2], big.mark = ",")
  index_voc_table <- rbind(c("Total", "", Pre_VOC_total, Alpha_total, Delta_total, 
                             Omicron_total), index_voc_table) 
  #add missing
  index_voc_table$Level[is.na(index_voc_table$Level)] <- "Missing"
  rownames(index_voc_table) <- seq(length = nrow(index_voc_table))
  index_voc_table[index_voc_table == "TRUE"] <- "True"
  index_voc_table[index_voc_table == "FALSE"] <- "False"
  
  #########################
  # Combined VOC and total table
  #########################
  VOC_total_table <- cbind(index_voc_table, all_index)
  VOC_total_table[, 7] <- NULL
  VOC_total_table[1, 7] <- gsub(" ", "", gsub("[()]", "", VOC_total_table[1, 7]))

  #########################
  # Potential source of infection table
  #########################
  
  
  
  source_table <- ct_so_index_unique %>% tabyl(Locale_infected) %>%
    rename(Level = Locale_infected) %>% select(-valid_percent)
  source_table$variable <- NA
  source_table$variable[1] <- "Source of infection (self-reported)"
  source_order <- c("Family or friends", "School", "Work", "Shopping/Public transport", "Other", 
                    "Unknown", NA)  
  source_table <- source_table[match(source_order, source_table$Level), ] 
  source_table$Level[is.na(source_table$Level)] <- "Missing"
  
  #number of close contacts reported table
  ncontacts_table <- ct_so_index_unique %>% tabyl(nContactscat) %>%
    rename(Level = nContactscat)
  ncontacts_table$variable <- NA
  ncontacts_table$variable[1] <- "Number of close contacts reported"
  #types of activities reported table
  #to do
  
  #combine tables
  source_inf_table <- bind_rows(source_table, ncontacts_table) 
  rm(source_table, ncontacts_table) #remove
  source_inf_table <- source_inf_table[, c(4, 1, 2, 3)]
  source_inf_table$percent <- paste0(round((source_inf_table$percent*100), 1), "%")
  source_inf_table$n <- prettyNum(source_inf_table$n, big.mark = ",")
  names(source_inf_table)[names(source_inf_table) == "percent"] <- "%"
  
  
  
  #LH 5.6.23 check table below
  
  #########################
  # Summary table of KPIs
  #########################
  #summary tables
  KPI_summary_index <- ct_so_index_unique %>%
    select(starts_with("KPI")) %>%
    sumtable(out = "return")  
  KPI_summary_combined <- ct_so_contacts_index %>%
    select(KPI3, KPI5, KPI8) %>%
    sumtable(out = "return")  
  #combine summary tables
  KPI_summary_table <- bind_rows(KPI_summary_index, KPI_summary_combined)
  rm(KPI_summary_index)
  rm(KPI_summary_combined)
  order <- c("KPI1", "KPI2", "KPI3", "KPI4", "KPI5", "KPI6", 
             "KPI7", "KPI8", "KPI9", "KPI10", "KPI12", 
             "KPI13", "KPI14", "KPI15")
  KPI_summary_table <- KPI_summary_table %>% slice(match(order, Variable)) #reorder
  rm(order)
  names(KPI_summary_table)[1] <- "KPI"  #rename columns
  ##add labels to KPIs
  KPI_labels = c("1. Time between case symptom onset and diagnosis of the index case",
          "2. Time between index case test and results",
          "3.	Time between case symptom onset and quarantine start of contacts",
          "4.	Time between test results and isolation of index case",
          "5.	Time between isolation of index case and quarantine of contact",
          "6.	Percentage of test results returned within 24 hours",
          "7.	Proportion of index cases contacted within 24 hours of the test result",
          "8.	Percentage of contacts notified within 24 hours of interview with index",
          "9.	Proportion of cases with no contacts elicited",
          "10.	Out of new symptomatic cases, number tested and interviewed within 3 days of onset of symptoms",
          "11.	Median contacts per case", 
          "1.	Proportion of index cases who use the app",
          "2.	Number of people working in the contact tracing team (FTEs)",
          "1.	Percentage of new cases arising among contacts identified by program",
          "2.	Percentage of new cases arising among contacts identified by program and under quarantine at the time of onset of their symptoms or, if asymptomatic, at first positive test with immediate initiation of isolation")

  KPI_summary_table$Description <- KPI_labels
  rm(KPI_labels)
  #reorder columns
  KPI_summary_table <- KPI_summary_table[, c(1, 9, 2:8)]
  #structure table
  Structure_table <- KPI_summary_table[12:13, ]
  #process table
  Process_table <- KPI_summary_table[1:11, ]
  #outcome table
  Outcome_table <- KPI_summary_table[14:15, ]
  
  summary()
  
  
  
  ############################################################################
  # table of KPIs listed
  KPIs_table <- data.frame(Category = c("Process", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                    "Structure", NA,
                                    "Outcome", NA),
                       KPI = c("1. Time between case symptom onset and diagnosis of the index case",
                               "2. Time between index case test and results",
                               "3.	Time between case symptom onset and quarantine start of contacts",
                               "4.	Time between test results and isolation of index case",
                               "5.	Time between isolation of index case and quarantine of contact",
                               "6.	Percentage of test results returned within 24 hours",
                               "7.	Proportion of index cases contacted within 24 hours of the test result",
                               "8.	Percentage of contacts notified within 24 hours of interview with index",
                               "9.	Proportion of cases with no contacts elicited",
                               "10.	Out of new symptomatic cases, number tested and interviewed within 3 days of onset of symptoms",
                               "11.	Median contacts per case",
                               "1.	Proportion of index cases who use the app",
                               "2.	Number of people working in the contact tracing team (FTEs)",
                               "1.	Percentage of new cases arising among contacts identified by program",
                               "2.	Percentage of new cases arising among contacts identified by program and under quarantine at the time of onset of their symptoms or, if asymptomatic, at first positive test with immediate initiation of isolation"))
                               
  ############################################################################
  #####################################
  # Table of reported activities by VOC period
  
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivityCat == "None reported"] <- NA
  
  table_activities <- ct_so_index %>% 
    tabyl(LocationActivityCat, VOC_period_index) %>%
    adorn_totals("row") %>%
    adorn_percentages(c("col")) %>%
    adorn_pct_formatting(digits = 0) %>%
    adorn_ns() %>%
    rename(Activity = LocationActivityCat) 
  
  activities_total <- ct_so_index %>%
    tabyl(LocationActivityCat) %>%
    adorn_totals("row") %>%
    adorn_pct_formatting(digits = 0) %>%
    rename(Activity = LocationActivityCat) 
  
  table_activities <- left_join(table_activities, activities_total, 
                                by = "Activity")
  
  table_activities <- table_activities %>%
    arrange(desc(n)) %>%
    mutate(Total = paste0(percent, " (", n, ")"))
  
  table_activities$Activity[is.na(table_activities$Activity)] <- "Missing"
  
  
  table_activities <- table_activities[c(4:14,3,2,1),c(1:5,9)]
  rownames(table_activities) <- NULL

  ############################################################################
  #% locations that received notification
  
  notified_table_all <- ct_so_index %>% 
    tabyl(LocationActivityCat, locationReceivedNotification) %>%
    adorn_totals("col") %>%
    mutate(fraction = paste0(`TRUE`, "/", Total),
           percent = scales::percent(`TRUE`/Total, accuracy = 1)) %>%
    mutate(all = paste0(percent, " (", fraction, ")"))

  
  notified_table_byVOC <- ct_so_index %>% 
    tabyl(LocationActivityCat, locationReceivedNotification, VOC_period_index) %>%
    adorn_totals("col") 
  
  alpha <- as.data.frame(notified_table_byVOC$Alpha) %>%
    mutate(fraction = paste0(`TRUE`, "/", Total),
           percent = scales::percent(`TRUE`/Total, accuracy = 1)) %>%
    mutate(alpha = paste0(percent, " (", fraction, ")"))
  delta <- as.data.frame(notified_table_byVOC$Delta) %>%
    mutate(fraction = paste0(`TRUE`, "/", Total),
           percent = scales::percent(`TRUE`/Total, accuracy = 1)) %>%
    mutate(delta = paste0(percent, " (", fraction, ")"))
  omicron <- as.data.frame(notified_table_byVOC$Omicron) %>%
    mutate(fraction = paste0(`TRUE`, "/", Total),
           percent = scales::percent(`TRUE`/Total, accuracy = 1)) %>%
    mutate(omicron = paste0(percent, " (", fraction, ")"))
  prevoc <- as.data.frame(notified_table_byVOC$`Pre-VOC`) %>%
    mutate(fraction = paste0(`TRUE`, "/", Total),
           percent = scales::percent(`TRUE`/Total, accuracy = 1)) %>%
    mutate(prevoc = paste0(percent, " (", fraction, ")"))
  
  
  str(alpha)
  notified_table <- data.frame(Activity = alpha$LocationActivityCat,
                                  `Pre-VOC` = prevoc$prevoc,
                                  Alpha = alpha$alpha,
                                  Delta = delta$delta,
                                  Omicron = omicron$omicron,
                                  Total = notified_table_all$all)
  notified_table <- na.omit(notified_table)
  
  ############################################################################
  
  #########################
  # Descriptive overview of index cases and contacts
  # Table overview
  # Missing values removed
  #########################
  #df of unique index cases
  ct_so_index_unique <- distinct(ct_so_index, IP_CaseID, .keep_all = TRUE)
  #age and gender
  age_table_NM <- ct_so_index_unique %>% tabyl(age_cat, show_na = FALSE) %>% 
    rename(Level = age_cat) %>% 
    arrange(desc(n)) %>% 
    mutate(Variable = NA)
  age_table_NM$Level <- as.character(age_table_NM$Level)
  age_table_NM[1, "Variable"] <- "Age (years)"
  gender_table_NM <- ct_so_index_unique %>% tabyl(Gender, show_na = FALSE) %>% 
    rename(Level = Gender) %>%  
    arrange(desc(n)) %>% 
    mutate(Variable = NA)
  gender_table_NM[1, "Variable"] <- "Gender"
  gender_table_NM <- gender_table_NM[c(2,1,3), 1:4]
  #symptoms
  
  #sweat
  sweat_table_NM <- ct_so_index_unique %>% tabyl(Sweat, show_na = FALSE) %>% 
    rename(Level = Sweat) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  sweat_table_NM["Variable"] <- "Sweating"
  sweat_table_NM <- as.data.frame(sweat_table_NM[sweat_table_NM$Level == TRUE,])
  #cough
  cough_table_NM <- ct_so_index_unique %>% tabyl(Cough, show_na = FALSE) %>% 
    rename(Level = Cough) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  cough_table_NM["Variable"] <- "Cough"
  cough_table_NM <- as.data.frame(cough_table_NM[cough_table_NM$Level == TRUE,])
  #diarrhea
  diar_table_NM <- ct_so_index_unique %>% tabyl(Diarrhea, show_na = FALSE) %>% 
    rename(Level = Diarrhea) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  diar_table_NM["Variable"] <- "Diarrhea"
  diar_table_NM <- as.data.frame(diar_table_NM[diar_table_NM$Level == TRUE,])
  #breath
  breath_table_NM <- ct_so_index_unique %>% tabyl(diff_breathing, show_na = FALSE) %>% 
    rename(Level = diff_breathing) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  breath_table_NM["Variable"] <- "Difficulty breathing or shortness of breath"
  breath_table_NM <- as.data.frame(breath_table_NM[breath_table_NM$Level == TRUE,])
  #temp
  temp_table_NM <- ct_so_index_unique %>% tabyl(Temp, show_na = FALSE) %>% 
    rename(Level = Temp) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  temp_table_NM["Variable"] <- "Fever"
  temp_table_NM <- as.data.frame(temp_table_NM[temp_table_NM$Level == TRUE,])
  #rapidnausea
  nausea_table_NM <- ct_so_index_unique %>% tabyl(RapidNausea, show_na = FALSE) %>% 
    rename(Level = RapidNausea) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  nausea_table_NM["Variable"] <- "Sudden nausea"
  nausea_table_NM <- as.data.frame(nausea_table_NM[nausea_table_NM$Level == TRUE,])
  #runnynose
  runny_table_NM <- ct_so_index_unique %>% tabyl(Runnynose, show_na = FALSE) %>% 
    rename(Level = Runnynose) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  runny_table_NM["Variable"] <- "Runny nose"
  runny_table_NM <- runny_table_NM[2,]
  #oxygen
  oxy_table_NM <- ct_so_index_unique %>% tabyl(Oxygen, show_na = FALSE) 
  oxy_table_NM["Variable"] <- "Oxygen required"
  oxy_table_NM <- rename(oxy_table_NM, Level = Oxygen)
  oxy_table_NM <- oxy_table_NM[2,]
  #respfailure
  respf_table_NM <- ct_so_index_unique %>% tabyl(RespiratoryFailure, show_na = FALSE) %>% 
    rename(Level = RespiratoryFailure) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  respf_table_NM["Variable"] <- "Respiratory failure"
  respf_table_NM <- respf_table_NM[2,]
  #other
  other_table_NM <- ct_so_index_unique %>% tabyl(Other_symp_group, show_na = FALSE) %>% 
    rename(Level = Other_symp_group) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  other_table_NM["Variable"] <- "Other symptoms"
  other_table_NM <- other_table_NM[2,]
  #no symptoms
  none_table_NM <- ct_so_index_unique %>% tabyl(AnySymptom, show_na = FALSE) %>% 
    rename(Level = AnySymptom) %>% arrange(desc(n)) %>% mutate(Variable = NA)
  none_table_NM <- as.data.frame(none_table_NM[none_table_NM$Level != TRUE,])
  none_table_NM[1, "Variable"] <- "No symptoms reported"
  #pneumonia
  pneu_table_NM <- ct_so_index_unique %>% tabyl(Pneumonia, show_na = FALSE)
  pneu_table_NM["Variable"] <- "Pneumonia"
  pneu_table_NM <- rename(pneu_table_NM, Level = Pneumonia)
  pneu_table_NM <- pneu_table_NM[2,]
  #sorethroat
  throat_table_NM <- ct_so_index_unique %>% tabyl(Sorethroat, show_na = FALSE) 
  throat_table_NM["Variable"] <- "Sore throat"
  throat_table_NM <- rename(throat_table_NM, Level = Sorethroat)
  throat_table_NM <- throat_table_NM[2,]
  #ARDS
  ards_table_NM <- ct_so_index_unique %>% tabyl(ARDS, show_na = FALSE) 
  ards_table_NM["Variable"] <- "Acute respiratory distress"
  ards_table_NM <- rename(ards_table_NM, Level = ARDS)
  ards_table_NM <- ards_table_NM[2,]
  #taste/smell
  taste_table_NM <- ct_so_index_unique %>% tabyl(loss_taste_smell, show_na = FALSE) 
  taste_table_NM["Variable"] <- "Loss of taste or smell"
  taste_table_NM <- rename(taste_table_NM, Level = loss_taste_smell)
  taste_table_NM <- taste_table_NM[2,]
  #general
  gen_table_NM <- ct_so_index_unique %>% tabyl(General, show_na = FALSE) 
  gen_table_NM["Variable"] <- "General malaise"
  gen_table_NM <- rename(gen_table_NM, Level = General)
  gen_table_NM <- gen_table_NM[2,]
  #HR
  HR_table_NM <- ct_so_index_unique %>% tabyl(HeartRate, show_na = FALSE) 
  HR_table_NM["Variable"] <- "Heart rate"
  HR_table_NM <- rename(HR_table_NM, Level = HeartRate)
  HR_table_NM <- HR_table_NM[2,]
  
  
  sweat_table_NM$Level <- as.character(sweat_table_NM$Level)
  cough_table_NM$Level <- as.character(cough_table_NM$Level)
  diar_table_NM$Level <- as.character(diar_table_NM$Level)
  temp_table_NM$Level <- as.character(temp_table_NM$Level)
  breath_table_NM$Level <- as.character(breath_table_NM$Level)
  nausea_table_NM$Level <- as.character(nausea_table_NM$Level)
  runny_table_NM$Level <- as.character(runny_table_NM$Level)
  throat_table_NM$Level <- as.character(throat_table_NM$Level)
  ards_table_NM$Level <- as.character(ards_table_NM$Level)
  taste_table_NM$Level <- as.character(taste_table_NM$Level)
  gen_table_NM$Level <- as.character(gen_table_NM$Level)
  HR_table_NM$Level <- as.character(HR_table_NM$Level)
  oxy_table_NM$Level <- as.character(oxy_table_NM$Level)
  pneu_table_NM$Level <- as.character(pneu_table_NM$Level)
  respf_table_NM$Level <- as.character(respf_table_NM$Level)
  other_table_NM$Level <- as.character(other_table_NM$Level)
  none_table_NM$Level <- as.character(none_table_NM$Level)
  
  
  #bind symp tables together
  symp_table_NM <- bind_rows(sweat_table_NM, cough_table_NM, diar_table_NM, breath_table_NM, 
                             temp_table_NM, nausea_table_NM, 
                             runny_table_NM, throat_table_NM, ards_table_NM,  pneu_table_NM,  
                             taste_table_NM, gen_table_NM,  HR_table_NM, 
                             oxy_table_NM,  respf_table_NM,  
                             other_table_NM, none_table_NM)
  symp_table_NM$Level <- symp_table_NM$Variable
  symp_table_NM$Variable <- NULL
  symp_table_NM <- symp_table_NM %>% 
    arrange(factor(Level, levels = c("No symptoms reported","Cough","Runny nose",
                                     "Sore throat","Fever","Sweating",
                                     "General malaise","Other symptoms",
                                     "Loss of taste or smell","Sudden nausea",
                                     "Diarrhea",
                                     "Difficulty breathing or shortness of breath",
                                     "Heart rate","Pneumonia",
                                     "Acute respiratory distress",
                                     "Oxygen required","Respiratory failure")))
  symp_table_NM$Variable <- NA
  symp_table_NM[1,4] <- "Symptoms"
  
  
  symp_order <- symp_table_NM$Level #for second table of characteristics
  #add missing data
  #remove temp tables
  rm(sweat_table_NM, cough_table_NM, diar_table_NM, breath_table_NM, 
     temp_table_NM, nausea_table_NM, 
     runny_table_NM, throat_table_NM, ards_table_NM,  pneu_table_NM,  
     taste_table_NM, gen_table_NM,  HR_table_NM, 
     oxy_table_NM,  respf_table_NM,  
     other_table_NM, none_table_NM)
  
  #vaccine status
  vax_table_NM <- ct_so_index_unique %>% tabyl(vaccine) %>% 
    rename(Level = vaccine) %>% mutate(Variable = NA)
  vax_table_NM <- vax_table_NM[match(c(TRUE, FALSE, NA), vax_table_NM$Level), ] 
  vax_table_NM[1, "Variable"] <- "Vaccination status" #Vaccinated (2 doses)"
  vax_table_NM[1, "Level"] <- "Vaccinated (2 doses)"
  vax_table_NM <- vax_table_NM[1,] 
  vax_table_NM$Level <- as.character(vax_table_NM$Level)
  
  #bind tables together
  index_table_NM <- bind_rows(age_table_NM, gender_table_NM, symp_table_NM, vax_table_NM) 
  rm(age_table_NM, gender_table_NM, symp_table_NM, vax_table_NM) #remove
  #add row for total
  total <- length(unique(ct_so_index_unique$IP_CaseID)) #total unique index
  index_table_NM <- rbind(c(NA, total, 1, 1, NA), index_table_NM) 
  
  
  index_table_NM <- index_table_NM[, c(4, 1, 2, 3)]
  index_table_NM[1,1] <- "Total"
  index_table_NM$percent <- paste0(round((index_table_NM$percent*100), 1), "%")
  index_table_NM$percent[index_table_NM$Variable == "Total"] <- ""
  index_table_NM$n <- prettyNum(index_table_NM$n, big.mark = ",")
  index_table_NM[index_table_NM == "TRUE"] <- "True"
  index_table_NM[index_table_NM == "FALSE"] <- "False"
  names(index_table_NM)[names(index_table_NM) == "percent"] <- "%"
  index_table_NM$Level[index_table_NM$Level == "Personal care with close contact (e.g., hairdresser)"] <- "Personal care"
  #prepare column that can be added to index_voc_table_NM
  all_index_NM <- index_table_NM[,2:4]
  all_index_NM$`Entire period` <- paste0(all_index_NM$`%`, " (", all_index_NM$n, ")")
  all_index_NM$n <- NULL
  all_index_NM$`%` <- NULL
  
  #########################
  # Descriptive overview of index cases and contacts by VOC period
  # Table of index cases by voc period
  #########################
  
  #age 
  age_table_NM <- ct_so_index_unique %>% tabyl(age_cat, VOC_period_index, show_na = F) %>%
    rename(Level = age_cat) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  age_table_NM$variable <- NA
  age_table_NM$variable[1] <- "Age (years)"
  #gender
  gender_table_NM <- ct_so_index_unique %>% tabyl(Gender, VOC_period_index, show_na = F) %>% 
    rename(Level = Gender) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  gender_table_NM$variable <- NA
  gender_table_NM$variable[1] <- "Gender"
  gender_table_NM$Level <- as.character(gender_table_NM$Level)
  
  #symptoms
  #sweat
  sweat_table_NM <- ct_so_index_unique %>% tabyl(Sweat, VOC_period_index, show_na = F) %>% 
    rename(Level = Sweat) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  sweat_table_NM$variable <- "Sweating"
  sweat_table_NM <- sweat_table_NM[sweat_table_NM$Level == TRUE & 
                                     complete.cases(sweat_table_NM),]
  sweat_table_NM$Level <- NULL
  #cough
  cough_table_NM <- ct_so_index_unique %>% tabyl(Cough, VOC_period_index, show_na = F) %>% 
    rename(Level = Cough) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  cough_table_NM$variable <- "Cough"
  cough_table_NM <- cough_table_NM[cough_table_NM$Level == TRUE & 
                                     complete.cases(cough_table_NM),]
  cough_table_NM$Level <- NULL
  #diarrhea
  diar_table_NM <- ct_so_index_unique %>% tabyl(Diarrhea, VOC_period_index, show_na = F) %>% 
    rename(Level = Diarrhea) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  diar_table_NM$variable <- "Diarrhea"
  diar_table_NM <- diar_table_NM[diar_table_NM$Level == TRUE  & 
                                   complete.cases(diar_table_NM),]
  diar_table_NM$Level <- NULL
  #breath
  breath_table_NM <- ct_so_index_unique %>% tabyl(diff_breathing, VOC_period_index, show_na = F) %>% 
    rename(Level = diff_breathing) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  breath_table_NM$variable <- "Difficulty breathing or shortness of breath"
  breath_table_NM <- breath_table_NM[breath_table_NM$Level == TRUE & 
                                       complete.cases(breath_table_NM),]
  breath_table_NM$Level <- NULL
  #temp
  temp_table_NM <- ct_so_index_unique %>% tabyl(Temp, VOC_period_index, show_na = F) %>% 
    rename(Level = Temp) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  temp_table_NM$variable <- "Fever"
  temp_table_NM <- temp_table_NM[temp_table_NM$Level == TRUE & 
                                   complete.cases(temp_table_NM),]
  temp_table_NM$Level <- NULL
  #rapidnausea
  nausea_table_NM <- ct_so_index_unique %>% tabyl(RapidNausea, VOC_period_index, show_na = F) %>% 
    rename(Level = RapidNausea) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  nausea_table_NM$variable <- "Sudden nausea"
  nausea_table_NM <- nausea_table_NM[nausea_table_NM$Level == TRUE & 
                                       complete.cases(nausea_table_NM),]
  nausea_table_NM$Level <- NULL
  #runnynose
  runny_table_NM <- ct_so_index_unique %>% tabyl(Runnynose, VOC_period_index, show_na = F) %>% 
    rename(Level = Runnynose) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  runny_table_NM$variable <- "Runny nose"
  runny_table_NM <- runny_table_NM[runny_table_NM$Level == TRUE & 
                                     complete.cases(runny_table_NM),]
  runny_table_NM$Level <- NULL
  #sorethroat
  throat_table_NM <- ct_so_index_unique %>% tabyl(Sorethroat, VOC_period_index, show_na = F) %>% 
    rename(Level = Sorethroat) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  throat_table_NM$variable <- "Sore throat"
  throat_table_NM <- throat_table_NM[throat_table_NM$Level == TRUE & 
                                       complete.cases(throat_table_NM),]
  throat_table_NM$Level <- NULL
  #ARDS
  ards_table_NM <- ct_so_index_unique %>% tabyl(ARDS, VOC_period_index, show_na = F) %>% 
    rename(Level = ARDS) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  ards_table_NM$variable <- "Acute respiratory distress"
  ards_table_NM <- ards_table_NM[ards_table_NM$Level == TRUE & 
                                   complete.cases(ards_table_NM),]
  ards_table_NM$Level <- NULL
  #pneumonia
  pneu_table_NM <- ct_so_index_unique %>% tabyl(Pneumonia, VOC_period_index, show_na = F) %>% 
    rename(Level = Pneumonia) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  pneu_table_NM$variable <- "Pneumonia"
  pneu_table_NM <- pneu_table_NM[pneu_table_NM$Level == TRUE & 
                                   complete.cases(pneu_table_NM),]
  pneu_table_NM$Level <- NULL
  #taste
  taste_table_NM <- ct_so_index_unique %>% tabyl(loss_taste_smell, VOC_period_index, show_na = F) %>% 
    rename(Level = loss_taste_smell) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  taste_table_NM$variable <- "Loss of taste or smell"
  taste_table_NM <- taste_table_NM[taste_table_NM$Level == TRUE & 
                                     complete.cases(taste_table_NM),]
  taste_table_NM$Level <- NULL
  #general
  gen_table_NM <- ct_so_index_unique %>% tabyl(General, VOC_period_index, show_na = F) %>% 
    rename(Level = General) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  gen_table_NM$variable <- "General malaise"
  gen_table_NM <- gen_table_NM[gen_table_NM$Level == TRUE & 
                                 complete.cases(gen_table_NM),]
  gen_table_NM$Level <- NULL
  #heartrate
  HR_table_NM <- ct_so_index_unique %>% tabyl(HeartRate, VOC_period_index, show_na = F) %>% 
    rename(Level = HeartRate) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  HR_table_NM$variable <- "Heart rate"
  HR_table_NM <- HR_table_NM[HR_table_NM$Level == TRUE & 
                               complete.cases(HR_table_NM),]
  HR_table_NM$Level <- NULL
  #oxygen
  oxy_table_NM <- ct_so_index_unique %>% tabyl(Oxygen, VOC_period_index, show_na = F) %>% 
    rename(Level = Oxygen) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  oxy_table_NM$variable <- "Oxygen required"
  oxy_table_NM <- oxy_table_NM[oxy_table_NM$Level == TRUE & 
                                 complete.cases(oxy_table_NM),]
  oxy_table_NM$Level <- NULL
  #respfailure
  respf_table_NM <- ct_so_index_unique %>% tabyl(RespiratoryFailure, VOC_period_index, show_na = F) %>% 
    rename(Level = RespiratoryFailure) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  respf_table_NM$variable <- "Respiratory failure"
  respf_table_NM <- respf_table_NM[respf_table_NM$Level == TRUE & 
                                     complete.cases(respf_table_NM),]
  respf_table_NM$Level <- NULL
  #other
  other_table_NM <- ct_so_index_unique %>% tabyl(Other_symp_group, VOC_period_index, show_na = F) %>% 
    rename(Level = Other_symp_group) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  other_table_NM$variable <- "Other symptoms"
  other_table_NM <- other_table_NM[other_table_NM$Level == TRUE & 
                                     complete.cases(other_table_NM),]
  other_table_NM$Level <- NULL
  #no symptoms
  none_table_NM <- ct_so_index_unique %>% tabyl(AnySymptom, VOC_period_index, show_na = F) %>% 
    rename(Level = AnySymptom) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  none_table_NM <- subset(none_table_NM, none_table_NM$Level == FALSE | is.na(none_table_NM$Level))
  none_table_NM[1, "variable"] <- "No symptoms reported"
  none_table_NM$Level <- NULL
  
  #bind symp tables together
  symp_table_NM <- bind_rows(sweat_table_NM, cough_table_NM, diar_table_NM, breath_table_NM, 
                             temp_table_NM, nausea_table_NM, 
                             runny_table_NM, throat_table_NM, ards_table_NM,  pneu_table_NM,  
                             taste_table_NM, gen_table_NM,  HR_table_NM, 
                             oxy_table_NM,  respf_table_NM,  
                             other_table_NM, none_table_NM) 
  symp_table_NM <- symp_table_NM[match(symp_order, symp_table_NM$variable), ] 
  symp_table_NM <- symp_table_NM[!is.na(symp_table_NM$Alpha),]
  rm(sweat_table_NM, cough_table_NM, diar_table_NM, breath_table_NM, temp_table_NM, #remove
     nausea_table_NM, runny_table_NM, throat_table_NM, ards_table_NM,  
     pneu_table_NM,  taste_table_NM, gen_table_NM,  HR_table_NM, oxy_table_NM,  
     respf_table_NM,  other_table_NM, none_table_NM) 
  symp_table_NM$Level <- symp_table_NM$variable
  symp_table_NM$variable <- NA
  symp_table_NM$variable[1] <-  "Symptoms"
  
  #vaccine status
  vax_table_NM <- ct_so_index_unique %>% tabyl(vaccine, VOC_period_index, show_na = F) %>% 
    rename(Level = vaccine) %>% adorn_percentages("col") %>%
    adorn_pct_formatting(rounding = "half up", digits = 0) %>% adorn_ns()
  vax_table_NM <- vax_table_NM[match(c(TRUE, FALSE, NA), vax_table_NM$Level), ] 
  vax_table_NM$variable <- NA
  vax_table_NM$Level[1] <- "Vaccinated (2 doses)"
  vax_table_NM$variable[1] <- "Vaccination status"
  vax_table_NM$Level <- as.character(vax_table_NM$Level)
  vax_table_NM <- vax_table_NM[1,]
  
  #combine tables
  index_voc_table_NM <- bind_rows(age_table_NM, gender_table_NM, symp_table_NM, vax_table_NM) 
  rm(age_table_NM, gender_table_NM, symp_table_NM, vax_table_NM) #remove
  
  #reorder cols
  index_voc_table_NM <- index_voc_table_NM %>% select("variable", "Level", "Pre-VOC", 
                                                      "Alpha", "Delta", "Omicron")
  #add row for totals
  VOC_table_NM <- ct_so_index_unique %>% tabyl(VOC_period_index) 
  Pre_VOC_total <-  prettyNum(VOC_table_NM[1, 2], big.mark = ",")
  Alpha_total <- prettyNum(VOC_table_NM[2, 2], big.mark = ",")
  Delta_total <- prettyNum(VOC_table_NM[3, 2], big.mark = ",")
  Omicron_total <- prettyNum(VOC_table_NM[4, 2], big.mark = ",")
  index_voc_table_NM <- rbind(c("Total", "", Pre_VOC_total, Alpha_total, Delta_total, 
                                Omicron_total), index_voc_table_NM) 
  rownames(index_voc_table_NM) <- seq(length = nrow(index_voc_table_NM))
  
  
  #########################
  # Combined VOC and total table
  #########################
  VOC_total_table_NM <- cbind(index_voc_table_NM, all_index_NM)
  VOC_total_table_NM[, 7] <- NULL
  VOC_total_table_NM[1, 7] <- gsub(" ", "", gsub("[()]", "", VOC_total_table_NM[1, 7]))
  
  #Compare source of infection and activities
  
  source_activity_table <- table(ct_so_index_unique$Locale_infected, ct_so_index_unique$LocationActivityCat)

  ############################################################################
  #create list of tables
  tables_list <- list(index_table, index_voc_table, source_inf_table, #1, 2, 3
                      VOC_total_table, table_activities, notified_table,
                      VOC_total_table_NM, source_activity_table) #7,8
  
}

