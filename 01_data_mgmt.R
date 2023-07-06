#' ---
#' title: "Data management"
#' author: "Leonie Heron"
#' date: "02/02/2022"
#' ---
#' 
#' ## Function loading of cleaned variables
# check initialize file - are all packages installed? (open R/comix_000_initialize.R)
path_name <- rstudioapi::getSourceEditorContext()$path
path_name <- gsub("01_data_mgmt.R","",path_name)
setwd(path_name)
source(paste0(path_name,"R/ct_so_000_initialise.R"))

## Load data
#load and clean raw index case data:
ct_so_index = ct_so_001_load_index()
#clean activity data:
ct_so_index = ct_so_002_clean_activities(ct_so_index)
#load and clean raw contacts data:
ct_so_contact = ct_so_002_load_contact() 
#Load and clean contact conversion data
ct_so_contact_conv = ct_so_003_load_contact_conv()
#load KOF stringency index data
ct_so_kof = ct_so_004_load_kof() 
#load and clean SO cases
ct_so_cases <- ct_so_005_load_cases()
#combine index and contact data
dataset_list <- ct_so_006_combine_data(ct_so_index, ct_so_contact, ct_so_cases,
                                       ct_so_contact_conv)
ct_so_contacts_index <- as.data.frame(dataset_list[1])
ct_so_index <- as.data.frame(dataset_list[2])
ct_so_contact <- as.data.frame(dataset_list[3])
#Combine KPIs, dates and cases in solothurn for plots
ct_so_KPIs_cases <- ct_so_007_combine_KPIs_cases(ct_so_index, 
                                                 ct_so_contacts_index, 
                                                 ct_so_cases)
#Create unique dataset of index cases
ct_so_index_unique <- ct_so_index %>% distinct(IP_CaseID, .keep_all = TRUE)
## Descriptive Data Presentation
# get overview tables (will be saved in output/table folder):
tables_list <- ct_so_008_descriptive_outputs(ct_so_index, 
                                             ct_so_contact, 
                                             ct_so_contacts_index)
# get figures:
figures_list <- ct_so_009_descriptive_figures(ct_so_index, ct_so_contact, 
                                              ct_so_contacts_index, ct_so_cases)



# save data generated (is needed to generate rmd output file):
save.image(paste0(path_name,"ct_so_project_",Sys.Date(),".RData"))

# reports
dir_create <- "./output/reports"
if (!dir.exists(dir_create)){
  dir.create(dir_create)
} 
# produce tables and figures for manuscript:
rmarkdown::render(paste0("./output/ct_so_manuscript.Rmd"))
# produce tables and figures for supplementary information:
#rmarkdown::render(paste0("./output/ct_so_supplement.Rmd"))

#remove functions
rm(ct_so_001_load_index)
rm(ct_so_002_load_contact)
rm(ct_so_003_load_kof)
rm(ct_so_004_load_cases)
rm(ct_so_005_combine_data)
rm(ct_so_006_combine_KPIs_cases)
rm(ct_so_007_descriptive_outputs)
rm(ct_so_008_descriptive_figures)

