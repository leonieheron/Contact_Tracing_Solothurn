#' ---
#' title: "Load and clean contact conversion data"
#' author: "Leonie Heron"
#' date: "01/06/2022"
#' ---
#' 

ct_so_003_load_contact_conv = function() {
############################################################
#Import data from conversion dataset 2 Nov20 - Apr21    ####
############################################################

#Data import contacts become index cases from Nov20 - Feb21  
ct_so_contact_conv_nov20_apr21 <- readxl::read_xlsx("KP_to_IP_nov20_apr21.xlsx")
ct_so_contact_conv_nov20_apr21 <- ct_so_contact_conv_nov20_apr21 %>%
  mutate(IP_Vaccinated = NA, KP_Vaccinated = NA, IP_Vaccine_Company = NA, 
         IP_Vaccine1_Date = NA, IP_Vaccine2_date = NA, KP_Vaccine_Company = NA,
         KP_Vaccine1_Date = NA, KP_Vaccine2_date = NA)

############################################################
#Import data from conversion dataset 2 may21 - aug21    ####
############################################################

#Data import contacts become index cases from may21 to aug22
ct_so_contact_conv_may21_aug21 <- readxl::read_xlsx("ip_from_kp_maytoaugust.xlsx")

#############################################################
#Import data from conversion dataset 1 - Aug21 to Feb22  ####
#############################################################

#Data import contacts become index cases from Aug21 to Feb22
ct_so_contact_conv_aug21_feb22 <- readxl::read_xlsx("conversion_ct_feb_22.xlsx")

#############################################################
#Combine all conversion data                            ####
#############################################################
ct_so_contact_conv <- rbind(ct_so_contact_conv_nov20_apr21,ct_so_contact_conv_may21_aug21)
ct_so_contact_conv <- rbind(ct_so_contact_conv,ct_so_contact_conv_aug21_feb22)
#remove duplicates
ct_so_contact_conv <- ct_so_contact_conv[!duplicated(ct_so_contact_conv$`IP DocID`),]

#convert IP ID to interger
ct_so_contact_conv$`IP DocID` <- as.integer(ct_so_contact_conv$`IP DocID`)

#remove other datasets
rm(ct_so_contact_conv_nov20_apr21, ct_so_contact_conv_may21_aug21, ct_so_contact_conv_aug21_feb22)

return(as.data.frame(ct_so_contact_conv))
}
