#' ---
#' title: "Combine index cases and contact cases for Solothurn contact tracing data"
#' author: "Leonie Heron"
#' date: "04/02/2022"
#' ---

ct_so_006_combine_data = function(ct_so_index, ct_so_contact, ct_so_cases, 
                                  ct_so_contact_conv) {

  #create dataset of unique index cases
  
  ct_so_index_unique <- distinct(ct_so_index, IP_CaseID, .keep_all = TRUE)
  
  #merge 2 datasets
  ct_so_contacts_index <- merge(ct_so_contact, ct_so_index_unique, 
                                by.x = "IP_DocID", by.y = "IP_CaseID",
                                suffixes = c(".contact", ".index"))

  
  #####################################################
  #create new var of number of contacts per index case
  ct_so_contacts_index <- ct_so_contacts_index %>% 
    add_count(IP_DocID, name = "nContacts")
  #create categories of number of of contacts per index case
  #NB these are only index cases with >0 contacts
  ct_so_contacts_index$nContacts_cat[ct_so_contacts_index$nContacts == 0] <- "0"    
  ct_so_contacts_index$nContacts_cat[ct_so_contacts_index$nContacts == 1 | 
                                       ct_so_contacts_index$nContacts == 2] <- "1-2"                  
  ct_so_contacts_index$nContacts_cat[ct_so_contacts_index$nContacts == 3 | 
                                       ct_so_contacts_index$nContacts == 4] <- "3-4"   
  ct_so_contacts_index$nContacts_cat[ct_so_contacts_index$nContacts == 5 | 
                                       ct_so_contacts_index$nContacts == 6] <- "5-6"                  
  ct_so_contacts_index$nContacts_cat[ct_so_contacts_index$nContacts >= 7] <- "7+"       
  
  #create indicator variable for contacts who could be linked to their index cases
  ct_so_contact$linked_index <- ifelse(ct_so_contact$IP_DocID %in% ct_so_index$IP_CaseID, 1, 0)
  

#####################################################
  # Generate KPIs
  
  #########################
  # KPI 3
  #########################
  #3. Time between start of index symptoms and contact interview
  
  ct_so_contacts_index$KPI3 <- as.numeric(difftime(ct_so_contacts_index$KP_Case_Date, 
                                                   ct_so_contacts_index$DateStartSymptoms, unit = "days"))
  

  
  #########################
  # KPI 5
  #########################
  #5. Time between case symptom onset and quarantine debut of contacts
  
  ct_so_contacts_index$KPI5 <- as.numeric(difftime(ct_so_contacts_index$Quar_StartDate, 
                                        ct_so_contacts_index$Isolation_Start, unit = "days"))
  
  #########################
  # KPI 8
  #########################
  #8.  % of contacts notified within 24 hours of interview with CT team
  ct_so_contacts_index <- ct_so_contacts_index %>%
    mutate(KPI8 = ifelse(as.Date(KP_Case_Date) <= as.Date(Case_Date) + 1, 
                         1, 0))
  #########################
  # KPI 9
  #########################
  #9.  % of cases with no contacts 
  ct_so_index$KPI9 <- ifelse(ct_so_index$IP_CaseID %in% ct_so_contacts_index$IP_DocID, 1, 0)
  
  index_cases_linked_contacts <- unique(ct_so_contact$IP_DocID)
  
  ct_so_index$hasatleastonecontact <- ifelse(ct_so_index$IP_CaseID %in% index_cases_linked_contacts, 1, 0)
  
  atleastonecontact <- ct_so_index %>% select(IP_CaseID, hasatleastonecontact)
  atleastonecontact$IP_CaseID <- as.character (atleastonecontact$IP_CaseID)

  #########################
  # KPI 11
  #########################
  #11. Median contacts per case
  #add ncontacts to index df
  ncontacts <- ct_so_contacts_index %>%
    select(IP_DocID, nContacts)
  ncontacts$IP_DocID = as.integer(ncontacts$IP_DocID)
  ct_so_index <- left_join(ct_so_index, ncontacts, by = c("IP_CaseID" = "IP_DocID"))
  #ct_so_index$KPI11 <- ifelse(ct_so_index$KPI9 == "Has no contacts", 0, ct_so_index$nContacts)
  #create categorical variable
  ct_so_index$nContacts <- as.integer(ct_so_index$nContacts)
  ct_so_index$nContacts[is.na(ct_so_index$nContacts)] <- "0"
  ct_so_index <- ct_so_index %>%
    mutate(nContactscat = ifelse(nContacts > 6, "7+",
                              ifelse(nContacts > 4, "5-6",
                                     ifelse(nContacts > 2, "3-4",
                                            ifelse(nContacts > 0, "1-2", "0")))))
  ct_so_index$nContacts <- as.integer(ct_so_index$nContacts)
  #########################
  # KPI 15
  #########################
  #add quarantine var to index df
  colnames(ct_so_contact_conv) <- paste(colnames(ct_so_contact_conv),".prev_con",sep="")
  ct_so_index <- left_join(ct_so_index, ct_so_contact_conv, 
                           by = c("IP_CaseID" = "IP DocID.prev_con"))
   ct_so_index$prev_contact <- ct_so_index$IP_CaseID %in% 
    ct_so_contact_conv$`IP DocID.prev_con` #indicates index case was previously a contact
   ct_so_index$KPI14[ct_so_index_unique$prev_contact == TRUE] <- 1
   ct_so_index$KPI14[ct_so_index_unique$prev_contact == FALSE] <- 0
     #symptomatic indicator
  ct_so_index$symptomatic <- !is.na(ct_so_index$DateStartSymptoms)
  #indicator for in quarantine at time of symptom start
  ct_so_index$quar_symp_onset <- NA
  ct_so_index$quar_symp_onset[as.Date(ct_so_index$DateStartSymptoms) >= 
                                as.Date(ct_so_index$`Quarantine Starts.prev_con`)] <- 1
  ct_so_index$quar_symp_onset[as.Date(ct_so_index$DateStartSymptoms) < 
                                as.Date(ct_so_index$`Quarantine Starts.prev_con`)] <- 0
  #indicator for in quarantine at time of positive test
  ct_so_index$quar_pos_test <- NA
  ct_so_index$quar_pos_test[as.Date(ct_so_index$Test.Date) >= 
                                as.Date(ct_so_index$`Quarantine Starts.prev_con`)] <- 1
  ct_so_index$quar_pos_test[as.Date(ct_so_index$Test.Date) < 
                                as.Date(ct_so_index$`Quarantine Starts.prev_con`)] <- 0
  #create KPI var
  ct_so_index <- ct_so_index %>% #indicate individuals who used to be contacts
    mutate(KPI15 = ifelse(prev_contact == 1, 0, NA))
  ct_so_index$KPI15[ct_so_index$quar_symp_onset == 1] <- 1
  ct_so_index$KPI15[is.na(ct_so_index$quar_symp_onset) & 
                      ct_so_index$quar_pos_test == 1] <- 1

  ############################################################################
  
  #add case data to index df
  ct_so_index <- left_join(ct_so_index, ct_so_cases, by = c("Case_Date" = "datum"))


 
  dataset_list <- list(as.data.frame(ct_so_contacts_index), as.data.frame(ct_so_index), as.data.frame(ct_so_contact))
}

