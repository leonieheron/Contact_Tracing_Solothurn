#' ---
#' title: "Descriptive figures"
#' author: "Leonie Heron"
#' date: "10/02/2022"
#' ---

ct_so_009_descriptive_figures = function(ct_so_index, ct_so_contact, 
                                         ct_so_contacts_index, ct_so_cases) {
  dir_create <- "./output/figures"
  if (!dir.exists(dir_create)){
    dir.create(dir_create)
  } 
  
#line plot of KPIs 1-6

KPIs1_6_cases_plot <- ct_so_KPIs_cases %>%
    mutate(datum = as.Date(datum)) %>%
    ggplot(aes(x = datum)) +
      xlab("Date") + #axis labels
      ylab("KPIs, seven-day rolling average in days") + #axis labels
  geom_smooth(aes(y = meanKPI1_7day, color = "Time between case symptom onset and diagnosis of the index case"), alpha = 0.2, lwd = 1.5) +
  geom_smooth(aes(y = meanKPI2_7day, color = "Time between index case test and results"), alpha = 0.2, lwd = 1.5) + 
  geom_smooth(aes(y = meanKPI3_7day, color = "Time between case symptom onset and quarantine start of contacts"), alpha = 0.2, lwd = 1.5) +
  geom_smooth(aes(y = meanKPI4_7day, color = "Time between test results and isolation of index case"), alpha = 0.2, lwd = 1.5) +
  geom_smooth(aes(y = meanKPI5_7day, color = "Time between isolation of index case and quarantine of contact"), alpha = 0.2, lwd = 1.5) +
      #geom_tile(aes(y = 1.25, 
      #              fill = cut(cases_per_100k, 
      #                         b = c(0, 15, 50, 100, 500)), 
      #              height = 7.5), alpha = 0.5) +
    #scale_fill_manual("COVID-19 cases in Solothurn \nper 100,000 people", 
    #                  values = c("white", "pink", "red", "darkred")) +
      labs(color = "Key performance \nindicators (days between)") +
    scale_color_brewer(palette = "Dark2",
                        labels = c("Case symptom onset and \ndiagnosis of the index case\n", 
                                       "Time between case symptom\nonset and quarantine start of contacts", 
                                       "Time between index case test\nand results", 
                                       "Time between isolation of index\ncase and quarantine of contact", 
                                       "Time between test results and isolation of index case")) +
    scale_x_date(expand = c(0, 0)) + 
    theme_bw()  #+ theme(text = element_text(size = 16)) + scale_y_continuous(expand = c(0, 0), limits = c(-2.5, 5))
      
  

  #line plot of KPIs 7-12
  
  KPIs7_12_cases_plot <- ct_so_KPIs_cases %>%
  mutate(datum = as.Date(datum)) %>%
    ggplot(aes(x = datum)) +
    xlab("Date") + #axis labels
    ylab("KPIs (%)") + #axis labels
    geom_smooth(aes(y = meanKPI6, color = "Percentage of test results returned within 24 hours"), alpha = 0.2, lwd = 1.5) +
  geom_smooth(aes(y = meanKPI7, color = "Proportion of index cases contacted within 24 hours of the test result"), alpha = 0.2, lwd = 1.5) +
  geom_smooth(aes(y = meanKPI8, color = "Percentage of contacts notified within 24 hours of interview with index"), alpha = 0.2, lwd = 1.5) + 
  geom_smooth(aes(y = meanKPI9, color = "Proportion of cases with no contacts elicited"), alpha = 0.2, lwd = 1.5) +
  geom_smooth(aes(y = meanKPI10, color = "Out of new symptomatic cases, number tested and interviewed within 3 days of onset of symptoms"), alpha = 0.2, lwd = 1.5) +
  geom_smooth(aes(y = meanKPI12, color = "Proportion of index cases who use the app"), alpha = 0.2, lwd = 1.5) +
    geom_smooth(aes(y = meanKPI14, color = "Percentage of new cases arising among contacts identified by program"), alpha = 0.2, lwd = 1.5) +
    geom_smooth(aes(y = meanKPI15, color = "Percentage of new cases arising among contacts identified by program and under quarantine at the time of onset of their symptoms or, if asymptomatic, at first positive test with immediate initiation of isolation"), alpha = 0.2, lwd = 1.5) +
  geom_tile(aes(y = 0.55, 
                  fill = cut(cases_per_100k, 
                             b = c(0, 15, 50, 100, 500)), 
                  height = 1.1), alpha = 0.5) +
    scale_fill_manual("COVID-19 cases in Solothurn \nper 100,000 people", 
                      values = c("white", "pink", "red", "darkred")) +
    labs(color = "Key performance \nindicators (%)") +
    scale_color_brewer(palette = "Dark2",
                       labels = c("Index cases who report\nusing the app", #KPI7
                                  "Index cases contacted\nwithin 24 hours of the test result", #KPI9
                                  "Test results returned\nwithin 24 hours", #KPI8
                                  "Cases with no contacts elicited", #KPI12
                                  "Contacts notified within 24 hours\nof interview with index", #kPI11
                                  "Cases interviewed and isolated\nwithin 24 hours of case report") #KPI10
                       ) +
    theme_bw()  + theme(text = element_text(size = 16),
                        axis.text = element_text(size = 16)) + 
  scale_x_date(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1))
  
  #structure KPIs plot 1
  structure_1_plot <- ct_so_KPIs_cases %>%
    mutate(datum = as.Date(datum)) %>%
    ggplot(aes(x = datum)) +
    xlab("Date") + #axis labels
    ylab("KPIs (%)") + #axis labels
    geom_smooth(aes(y = meanKPI12), alpha = 0.2, lwd = 1.5) +
    theme_bw() +
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 16)) + 
    scale_x_date(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0.1, 0.4),
                       labels = scales::percent_format(accuracy = 1)) +
    ggtitle("Proportion of index cases who reported using the SwissCovid app")
  #structure KPIs plot 2
  ##line plot of workforce plus cases
  workforce_plot <- ct_so_index %>%
    mutate(cases_per_1m = cases_per_100k/10) %>%
    ggplot(aes(x = Case_Date)) +
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 16)) + 
    geom_step(aes(y = cases_per_1m), color = "red", size = 1.2) +
    geom_step(aes(y = efte), color = "blue", size = 1.2) +
    xlab("Date") + #axis labels
    ylab("Contact tracers (FTE hours)(blue)\nCases per million(red)") + #axis labels
    ggtitle("Contact tracing workforce during SARS-CoV-2 pandemic") 
  #process KPIs plot 1
  KPIs_process_plot_1 <- ct_so_KPIs_cases %>%
    mutate(datum = as.Date(datum)) %>%
    ggplot(aes(x = datum)) +
    xlab("Date") + #axis labels
    ylab("KPIs (days)") + #axis labels
    geom_smooth(aes(y = meanKPI1, color = "Time between case symptom onset and diagnosis of the index case"), alpha = 0.2, lwd = 1.5) +
    geom_smooth(aes(y = meanKPI2, color = "Time between index case test and results"), alpha = 0.2, lwd = 1.5) + 
    geom_smooth(aes(y = meanKPI3, color = "Time between case symptom onset and quarantine start of contacts"), alpha = 0.2, lwd = 1.5) +
    geom_smooth(aes(y = meanKPI4, color = "Time between test results and isolation of index case"), alpha = 0.2, lwd = 1.5) +
    geom_smooth(aes(y = meanKPI5, color = "Time between isolation of index case and quarantine of contact"), alpha = 0.2, lwd = 1.5) +
    labs(color = "Key performance \nindicators (days between)") +
    scale_color_brewer(palette = "Dark2",
                       labels = c("Case symptom onset and \ndiagnosis of the index case\n", 
                                  "Time between case symptom\nonset and quarantine start of contacts", 
                                  "Time between index case test\nand results", 
                                  "Time between isolation of index\ncase and quarantine of contact", 
                                  "Time between test results and isolation of index case")) +
    scale_x_date(expand = c(0, 0)) + 
    theme_bw() + #+ theme(text = element_text(size = 16)) + scale_y_continuous(expand = c(0, 0), limits = c(-2.5, 5))
    ggtitle("Key performance indicators of the Solothurn contact tracing system relating to process")
  #process KPIs plot 2
  
  #KPIs_process_plot_2 <- 
  ct_so_KPIs_cases <- ct_so_KPIs_cases %>%
    mutate(datum = as.Date(datum))
  
ggplot(ct_so_KPIs_cases, aes(x = datum)) +
  xlab("Date") + #axis labels
  ylab("KPIs (%)") + #axis labels
  # geom_tile(aes(y = 0.5, 
  #               fill = cut(cases_per_100k, b = c(0, 15, 50, 100, 500)), 
  #               height = 1), alpha = 0.9) +
  # scale_fill_manual("COVID-19 cases in Solothurn \nper 100,000 people", 
  #                   values = c("white", "pink", "red", "darkred")) +
  geom_smooth(aes(y = meanKPI6, 
                  color = "Proportion of test results returned within 24 hours"), 
              alpha = 0.2, lwd = 1.5) +
  geom_smooth(aes(y = meanKPI7, 
                  color = "Proportion of index cases contacted within 24 hours of the test result"), 
              alpha = 0.2, lwd = 1.5) +
  geom_smooth(aes(y = meanKPI8, 
                  color = "Proportion of contacts notified within 24 hours of interview with index"), 
              alpha = 0.2, lwd = 1.5) + 
  geom_smooth(aes(y = meanKPI9, 
                  color = "Proportion of cases with no contacts elicited"), 
              alpha = 0.2, lwd = 1.5) +
  geom_smooth(aes(y = meanKPI10, 
                  color = "Out of new symptomatic cases, proportion tested and interviewed within 3 days of onset of symptoms"), 
              alpha = 0.2, lwd = 1.5) +
  labs(color = "Key performance \nindicators (%)") +
  scale_color_brewer(palette = "Dark2",
                      labels = c("Out of new symptomatic cases,\nproportion tested and interviewed\nwithin 3 days of onset of symptoms", #KPI9
                                 "Proportion of cases with no\ncontacts elicited", #KPI9
                                 "Percentage of contacts notified\nwithin 24 hours of interview with index", #KPI8
                                 "Proportion of index cases\ncontacted within 24 hours of the test result", #KPI7
                                 "Percentage of test results\nreturned within 24 hours")#KPI6
  ) +
  theme_bw()  + #theme(text = element_text(size = 16),
                  #     axis.text = element_text(size = 16)) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1)) +
  ggtitle("Key performance indicators of the Solothurn contact tracing system relating to process")
    
    
  #process KPIs plot 3
  #median contacts per case
  KPIs_process_plot_3 <- ct_so_KPIs_cases %>%
    mutate(datum = as.Date(datum)) %>%
    ggplot(aes(x = datum)) +
    xlab("Date") + #axis labels
    ylab("Median contacts (n)") + #axis labels
    geom_smooth(aes(y = meanKPI11), alpha = 0.2, lwd = 1.5) +
    scale_x_date(expand = c(0, 0)) + 
    theme_bw() + #+ theme(text = element_text(size = 16)) + scale_y_continuous(expand = c(0, 0), limits = c(-2.5, 5))
    ggtitle("Median contacts per case in the Solothurn contact tracing system")
  

  #outcome KPIs plots
  outcome_1_plot <- ct_so_KPIs_cases %>%
    mutate(datum = as.Date(datum)) %>%
    ggplot(aes(x = datum)) +
    xlab("Date") + #axis labels
    ylab("KPIs (%)") + #axis labels
    geom_line(aes(y = meanKPI14week)) +
    theme_bw() +
    #theme(text = element_text(size = 16),
    #      axis.text = element_text(size = 16)) + 
    scale_x_date(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0),#, limits = c(0.1, 0.4),
                       labels = scales::percent_format(accuracy = 1)) +
    ggtitle("Proportion of new cases arising among contacts identified by program")
  outcome_2_plot <- ct_so_KPIs_cases %>%
    mutate(datum = as.Date(datum)) %>%
    ggplot(aes(x = datum)) +
    xlab("Date") + #axis labels
    ylab("KPIs (%)") + #axis labels
    geom_smooth(aes(y = meanKPI15)) +
    theme_bw() +
    #theme(text = element_text(size = 16),
    #      axis.text = element_text(size = 16)) + 
    scale_x_date(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0),#, limits = c(0.1, 0.4),
                       labels = scales::percent_format(accuracy = 1)) +
    ggtitle("Percentage of new cases arising among contacts identified by program who were either 
under quarantine at the time of onset of their symptoms or, if asymptomatic, began 
to quarantine before or on day of testing")
  
  #plot relevant missing data for index cases
  ct_so_index_unique <- ct_so_index %>%
    distinct(IP_CaseID, .keep_all = TRUE)
  ct_so_index_unique_miss <- ct_so_index_unique %>%
    select(Case_Date, IP_CaseID, Gender, Date.Receipt.Lab, Test.Date, Res.Canton, 
           Isolation.Possible, DateStartSymptoms, Appavailable, Isolation_Start, 
           Isolation_End, Locale_infected, vaccine, AnySymptom, AnyActivity, nActivity)  
  names(ct_so_index_unique_miss) <- c("Case date", "Case ID", "Gender", 
                                      "Date of lab receipt", "Test date",
                                      "Canton", "Isolation possible", 
                                      "Symptoms start", "App available",
                                      "Isolation start", "Isolation end",
                                      "Source of infection", "Vaccine", 
                                      "Any symptom", "Any activity",
                                      "nActivity")
  missing_plot <- missmap(ct_so_index_unique_miss)
  
  ##plot workforce against cases and variants and important kpis
  ct_so_KPIs_cases$datum <- as.Date(ct_so_KPIs_cases$datum)
  wfcases_KPIs_plot <- ct_so_KPIs_cases %>% 
    ggplot(aes(x = datum)) +
    scale_y_continuous(name = "Cases per workforce (full-time equivalent) (Black line)", 
                       sec.axis = sec_axis(trans=~.*10, name="Percentage (Coloured lines)")) +
    xlab("Date") +
    geom_line(aes(y = caseperwf)) +
    geom_bracket(xmin = as.Date("2020-11-14"), xmax = as.Date("2021-02-07"), #keep to as.date for rmd
                 y.position = 10, label = "Wild-type") +
    geom_bracket(xmin = as.Date("2021-02-08"), xmax = as.Date("2021-06-27"),
                  y.position = 10, label = "Alpha") +
    geom_bracket(xmin = as.Date("2021-06-28"), xmax = as.Date("2021-12-26"),
                  y.position = 10, label = "Delta") +
    geom_bracket(xmin = as.Date("2021-12-27"), xmax = as.Date("2022-02-01"),
                  y.position = 10, label = "Omicron") +
    geom_smooth(aes(y = meanKPI15*10, color = "O2")) +
    geom_smooth(aes(y = meanKPI8*10, color = "P8")) +
    geom_smooth(aes(y = meanKPI7*10, color = "P7")) +
    scale_colour_discrete(labels = c("% of index cases who were\npreviously identified as contacts\nthat were in quarantine by the time\nof symptom onset or, if asymptomatic,\nby time of positive test",
                                     "\n% of index cases contacted\nwithin 24 hours of the PCR\ntest result",
                                     "\n% of contact cases contacted\nwithin 24 hours of the\ncontact tracing interview with the\nindex case"))
    tiff(file="output/figures/wfcases_KPIs_plot.tiff") #save image
      #width=600, height=350)
  wfcases_KPIs_plot
  dev.off()
  
  #Calculate numbers
  #Confirmed positive SARS-CoV-2 cases in Solothurn
  ct_period_index <- ct_so_cases %>%
    filter(datum > as.Date("2020-11-14") & datum < as.Date("2022-02-03"))
  total_cases <- sum(ct_period_index$entries)
  #Index cases contacted by text message or phone call
  contacted <- sum(length(unique(ct_so_index$IP_CaseID)))
  #Index cases who completed online form
  missingallinfo_index <- ct_so_index %>% filter(is.na(Gender) & is.na(age)) 
  completedform <- contacted - length(unique(missingallinfo_index$IP_CaseID))
  #Reported contacts of index cases and contacted
  ct_period_contacts <- ct_so_contact %>%
    filter(KP_Case_Date > as.Date("2020-11-14") & KP_Case_Date < as.Date("2022-02-03"))
  total_contacts <- sum(length(unique(ct_period_contacts$KP_CaseID)))
  #Contacts completed online form
  missingallinfo_contact <- ct_period_contacts %>% filter(is.na(Gender) & is.na(age)) 
  contact_success <- total_contacts - length(unique(missingallinfo_contact$KP_CaseID))
  #number of converted contacts in this period
  ct_period_conv_contact <- ct_so_contact_conv %>%
    filter(`IP DocID` %in% ct_so_index$IP_CaseID)
  conv_contacts <- length(unique(ct_period_conv_contact$`IP DocID`))
  
  #cascade figure
  tab1 <<- prettyNum(total_cases, big.mark = ",") #Confirmed positive SARS-CoV-2 cases in Solothurn
  tab2 <<- prettyNum(contacted, big.mark = ",") #Index cases contacted by text message or phone call
  tab3 <<- prettyNum(completedform, big.mark = ",") #Index cases who completed online form
  tab4 <<- prettyNum(total_contacts, big.mark = ",") #Reported contacts of index cases
  tab5 <<- prettyNum(total_contacts, big.mark = ",") #Contacts successfully contacted
  tab6 <<- prettyNum(contact_success, big.mark = ",") #Contacts completed online form
  tab7 <<- prettyNum(conv_contacts, big.mark = ",") #number of converted contacts in this period
  
 
  
  cascade_plot <- grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, 
      style = filled, fillcolor = lightskyblue2, fixedsize = FALSE]        
      tab1 [label = '@@1', width = 12, fontsize = 20]
      tab2 [label = '@@2', width = 11.4, fontsize = 20]
      tab3 [label = '@@3', width = 11.4, fontsize = 20]
      tab4 [label = '@@4', fillcolor = lightgoldenrod1, width = 14.4, fontsize = 20]
      tab5 [label = '@@5', fillcolor = lightgoldenrod1, width = 14.4, fontsize = 20]
      tab6 [label = '@@6', fillcolor = lightgoldenrod1, width = 13.4, fontsize = 20]
      tab7 [label = '@@7', fillcolor = darkseagreen1, width = 1.2, fontsize = 20]

      # edge definitions with the node IDs
      tab1 -> tab2 -> tab3 -> tab4 -> tab5 -> tab6 -> tab7;
      }

      [1]: paste0('Confirmed positive SARS-CoV-2 cases in Solothurn n=', tab1)
      [2]: paste0('Index cases contacted by text message or phone call n=', tab2)
      [3]: paste0('Index cases who completed online form n=', tab3)
      [4]: paste0('Reported contacts of index cases n=', tab4)
      [5]: paste0('Contacts successfully contacted n=', tab5)
      [6]: paste0('Contacts completed online form n=', tab6)
      [7]: paste0('n=', tab7)
      ")
  cascade_plot
  #Contacts tested positive for SARS-CoV-2 and becoming index cases 7
  
  ######################################################
  # KPI plot by VOC period                          ####
  ######################################################
  
  kpi7_plot_data <- ct_so_index_unique %>% 
    tabyl(KPI7, VOC_period_index, show_na = FALSE) %>% 
    rename(Level = KPI7) %>% adorn_percentages("col") %>%
    filter(Level == 1) %>% select(-Level) %>% t() %>% data.frame() %>%
    mutate(KPI = "7",
           voc = c("Alpha", "Delta", "Omicron", "Pre-VOC")) %>%
    mutate(voc = fct_relevel(voc, c("Pre-VOC", "Alpha", "Delta", "Omicron")))
  names(kpi7_plot_data)[1] <- "mean"
  kpi7_plot_data <- kpi7_plot_data[c(4, 1, 2, 3),]
  
  kpi8_plot_data <- ct_so_contacts_index %>% 
    tabyl(KPI8, VOC_period_contact, show_na = FALSE) %>% 
    rename(Level = KPI8) %>% adorn_percentages("col") %>%
    filter(Level == 1) %>% select(-Level) %>% t() %>% data.frame() %>%
    mutate(KPI = "8", 
           voc = c("Alpha", "Delta", "Omicron", "Pre-VOC")) %>%
    mutate(voc = fct_relevel(voc, c("Pre-VOC", "Alpha", "Delta", "Omicron")))
  names(kpi8_plot_data)[1] <- "mean"
  kpi8_plot_data <- kpi8_plot_data[c(4, 1, 2, 3),]
  #kpi15 data
  kpi15_plot_data <- ct_so_index_unique %>% 
    tabyl(KPI15, VOC_period_index, show_na = FALSE) %>% 
    rename(Level = KPI15) %>% adorn_percentages("col") %>%
    filter(Level == 1) %>% select(-Level) %>% t() %>% data.frame() %>%
    mutate(KPI = "15", 
           voc = c("Alpha", "Delta", "Omicron", "Pre-VOC")) %>%
    mutate(voc = fct_relevel(voc, c("Pre-VOC", "Alpha", "Delta", "Omicron")))
  names(kpi15_plot_data)[1] <- "mean"
  kpi15_plot_data <- kpi15_plot_data[c(4, 1, 2, 3),]
  
  
  #combine rows
  kpi7_8_15_plot_data <- rbind(kpi7_plot_data, kpi8_plot_data, kpi15_plot_data)
  
  #create plot
  kpi7_8_15_bar_plot <- ggplot(kpi7_8_15_plot_data, aes(voc, mean, fill=KPI))+
    geom_bar(stat="identity", position="dodge", width=0.5,size=5) +
    labs(x = "Dominant variant of concern", 
         y = "Percentage",
         fill = "Legend") +
    theme_classic() +
    scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0, 1.01)) +
    scale_fill_hue(labels = c("% of index cases who were\npreviously identified as contacts\nthat were in quarantine by the time\nof symptom onset or, if asymptomatic,\nby time of positive test",
                              "\n% of index cases contacted\nwithin 24 hours of the PCR\ntest result",
                              "\n% of contact cases contacted\nwithin 24 hours of the\ncontact tracing interview with the\nindex case"))

  ######################################################
  # n contacts per index case - boxplot             ####
  ######################################################
  ncontacts_boxplot <- boxplot(ct_so_index_unique$nContacts ~ 
                                 ct_so_index_unique$VOC_period_index,
                               #notch = TRUE,
                               varwidth = TRUE,
                               labels = fivenum(median(ct_so_index_unique$nContacts)),
                               xlab = "Dominant variant of concern",
                               ylab = "Reported contacts (n)",
                               outline = FALSE,
                               col = c("#0B3C4D", "#167697", "#1b92bc", "#21aedf"))
  

  boxplot_values <- tapply(ct_so_index_unique$nContacts, 
         ct_so_index_unique$VOC_period_index, 
         summary)    # Summary by group using tapply
  
  prevoc_IQR <- "1.00 (0.00-3.00)"
  alpha_IQR <- "1.00 (0.00-3.00)"
  delta_IQR <- "1.00 (0.00-2.00)"
  omicron_IQR <- "0.10 (0.00-1.00)"
  
  tiff(file = "Figure4.tif", 
       width = 3000,
       height = 3000,
       res = 300)
  
  ncontacts_boxplot <- boxplot(ct_so_index_unique$nContacts ~ 
                                 ct_so_index_unique$VOC_period_index,
                               #notch = TRUE,
                               varwidth = TRUE,
                               labels = fivenum(median(ct_so_index_unique$nContacts)),
                               xlab = "Dominant variant of concern",
                               ylab = "Reported contacts (n)",
                               outline = FALSE,
                               col = c("#0B3C4D", "#167697", "#1b92bc", "#21aedf"))
  
  
  boxplot_values <- tapply(ct_so_index_unique$nContacts, 
                           ct_so_index_unique$VOC_period_index, 
                           summary)    # Summary by group using tapply
  
  prevoc_IQR <- "1.00 (0.00-3.00)"
  alpha_IQR <- "1.00 (0.00-3.00)"
  delta_IQR <- "1.00 (0.00-2.00)"
  omicron_IQR <- "0.10 (0.00-1.00)"
  
  dev.off()
  
  ######################################################
  # cases per workforce, VOC, shaded background cases ##
  ######################################################
  
  cases_wf_plot <- ggplot(ct_so_KPIs_cases, aes(x = datum)) + 
    theme_bw() +
    geom_rect(aes(xmin = datum, xmax = dplyr::lead(datum), ymin = 0, ymax = Inf, fill = cases_per_100k)) + 
    scale_fill_gradient(low = "#FFF5F3", high = "red") +
    # ylim(0, 10.5) +
    # xlim(as.Date("2020-11-15"), as.Date("2022-02-01")) +
    expand_limits(x = as.Date(c("2020-11-20", "2022-02-02"))) +
    labs(fill="Daily SARS-CoV-2 cases per\n100,000 people in Solothurn",
         caption = "
         The graph is shaded red according to the SARS-CoV-2 case load. The dashed line indicates the contact-tracing workforce working at\nfull-time equivalent (FTE) and the solid line indicates the cases per workforce. The Greek symbols \u03B1, \u03B4, and \u03BF indicate the periods\nduring which the alpha, delta, and omicron SARS-CoV-2 variants of concern (VOC) were dominant, respectively.") +
    geom_bracket(xmin = as.Date("2020-11-16"), xmax = as.Date("2021-02-07"), 
                 y.position = 55, label = "Pre-VOC") +
    geom_bracket(xmin = as.Date("2021-02-08"), xmax = as.Date("2021-06-27"),
                 y.position = 55, label = "\u03B1") +
    geom_bracket(xmin = as.Date("2021-06-28"), xmax = as.Date("2021-12-26"),
                 y.position = 55, label = "\u03B4") +
    geom_bracket(xmin = as.Date("2021-12-27"), xmax = as.Date("2022-02-01"),
                 y.position = 55, label = "\u03BF") +
    geom_line(aes(y = caseperwf*5), size = 0.7) +
    geom_line(aes(y = efte), size = 0.7, linetype = 2) +
    annotate("text", x=as.Date("2021-08-12"), y=42, 
             label= "Contact-tracing workforce (FTE)") +
    annotate("text", x=as.Date("2021-08-12"), y=10, 
             label= "Cases per workforce (FTE)") +
    xlab("Date") + ylab("") +
    theme(plot.caption = element_text(hjust = 0)) +
    scale_x_date(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 60),
                       name = "Contact-tracing workforce (FTE)",
                       sec.axis = sec_axis( trans=~./5, name="Cases per workforce (FTE)"))
  cases_wf_plot
  
  
  tiff(file = "Figure2.tif", 
       width = 5000,
       height = 3000,
       res = 300)
  
  cases_wf_plot
  
  dev.off()
  
  

    ######################################################
  # 3 important  kpis, VOC, shaded background cases   ##
  ######################################################
  
  # kpis_caseload_plot <- ct_so_KPIs_cases %>% 
  #   ggplot(aes(x = datum)) +
  #   geom_rect(aes(xmin = datum, xmax = dplyr::lead(datum), ymin = 0, ymax = Inf, fill = cases_per_100k)) + 
  #   scale_fill_gradient(low = "#FFF5F3", high = "red") +
  #   scale_y_continuous(name = "Percentage", labels = scales::percent_format(scale = 10, accuracy = 1)) +
  #   xlab("Date") +
  #   geom_bracket(xmin = as.Date("2020-11-14"), xmax = as.Date("2021-02-07"), #keep to as.date for rmd
  #                y.position = 10.8, label = "Wild-type") +
  #   geom_bracket(xmin = as.Date("2021-02-08"), xmax = as.Date("2021-06-27"),
  #                y.position = 10.8, label = "Alpha") +
  #   geom_bracket(xmin = as.Date("2021-06-28"), xmax = as.Date("2021-12-26"),
  #                y.position = 10.8, label = "Delta") +
  #   geom_bracket(xmin = as.Date("2021-12-27"), xmax = as.Date("2022-02-01"),
  #                y.position = 10.8, label = "Omicron") +
  #   geom_smooth(aes(y = meanKPI15*10, color = "O2"), se = FALSE) +
  #   geom_smooth(aes(y = meanKPI8*10, color = "P8"), se = FALSE) +
  #   geom_smooth(aes(y = meanKPI7*10, color = "P7"), se = FALSE) +
  #   theme(legend.key = element_rect(colour = "white")) +
  #   theme_bw() +
  #   labs(fill="Daily SARS-CoV-2 cases per\n100,000 people in Solothurn", color = "Key performance indicators") +
  #   scale_colour_discrete(labels = c("% of index cases who were\npreviously identified as contacts\nthat were in quarantine by the time\nof symptom onset or, if asymptomatic,\nby time of positive test",
  #                                    "% of index cases contacted\nwithin 24 hours of the PCR\ntest result",
  #                                    "% of contact cases contacted\nwithin 24 hours of the\ncontact tracing interview with the\nindex case"))
  # 
  
  kpis_caseload_plot <- ct_so_KPIs_cases %>% 
    ggplot(x = datum) +
    geom_rect(aes(xmin = datum, xmax = dplyr::lead(datum), ymin = 0, ymax = Inf, fill = cases_per_100k)) + 
    scale_fill_gradient(low = "#FFF5F3", high = "red") +
    scale_y_continuous(name = "Key performance indicator (%)", 
                       labels = scales::percent_format(scale = 100, accuracy = 1),
                       expand = c(0, 0), limits = c(0, 1.12)) +
    xlab("Week") + 
    scale_x_yearweek(expand = c(0, 0), limits = as.Date(c("2020-11-15", 
                                                          "2022-02-02"))) +
    geom_line(aes(x = yearweek, y = KPI7week, color = "#009E73"), size = 0.9) +
    geom_line(aes(x = yearweek, y = KPI8week, color = "#0072B2"), size = 0.9) +
    geom_line(aes(x = yearweek, y = KPI14week, color = "#000000"), size = 0.9) +
    geom_line(aes(x = yearweek, y = KPI16week, color = "#f7cb07"), size = 0.9) +
    geom_bracket(xmin = as.POSIXct("2020-11-16"), xmax = as.POSIXct("2021-02-07"), 
                 y.position = 1.05, label = "Pre-VOC") +
    geom_bracket(xmin = as.POSIXct("2021-02-08"), xmax = as.POSIXct("2021-06-27"),
                 y.position = 1.05, label = "\u03B1") +
    geom_bracket(xmin = as.POSIXct("2021-06-28"), xmax = as.POSIXct("2021-12-26"),
                 y.position = 1.05, label = "\u03B4") +
    geom_bracket(xmin = as.POSIXct("2021-12-27"), xmax = as.POSIXct("2022-02-01"),
                 y.position = 1.05, label = "\u03BF") +
    labs(caption = "The graph is shaded red according to the SARS-CoV-2 case load. The solid lines indicate the percentage of the key\nperformance indicators over time. The Greek symbols \u03B1, \u03B4, and \u03BF indicate the periods during which the alpha, delta,\nand omicron SARS-CoV-2 variants of concern (VOC) were dominant, respectively. The dashed line indicates the point\nat which the system contacted individuals automatically (1 January 2022).", 
         fill="Daily SARS-CoV-2 cases per\n100,000 people in Solothurn", 
         color = "Key performance indicators") +
    theme(plot.caption = element_text(hjust = 0)) +
    #annotate("text", x = as.POSIXct("2021-12-31"), y = 0.165, label = "X", size = 5)
    geom_vline(xintercept = as.Date("2021-12-31"), 
               linetype='dashed', color='black', size=0.7) +
    scale_colour_discrete(labels = c("Outcome KPI2: % of index cases who were\npreviously identified as contacts that were in\nquarantine by the time of symptom onset or,\nif asymptomatic, by time of positive test",
                                     "\nProcess KPI8: % of contacts notified on same\nor next day after interview with index",
                                     "\nProcess KPI6: % of test results returned on\nsame or next day",
                                     "\nOutcome KPI3: % of index cases in isolation\nby day of testing"))
  
  kpis_caseload_plot
  
  ######################################################
  # changes made to kpi graph as per reviewer's request   ##
  ######################################################
  #rearrange data
  KPIs_values <- ct_so_KPIs_cases %>%
    select(datum, KPI6week, KPI8week, KPI14week, KPI16week) %>%
    gather(key = "variable", value = "value", -datum)
  #add to exisiting dataset
  ct_so_KPIs_cases <- merge(ct_so_KPIs_cases, KPIs_values, by = "datum")
  
  kpis_caseload_plot <- ct_so_KPIs_cases %>% 
    ggplot(x = datum) +
    geom_rect(aes(xmin = datum, xmax = dplyr::lead(datum), ymin = 0, ymax = Inf, fill = cases_per_100k)) + 
    scale_fill_gradient(low = "#FFF5F3", high = "red") +
    scale_y_continuous(name = "Key performance indicator (%)", 
                       labels = scales::percent_format(scale = 100, accuracy = 1),
                       expand = c(0, 0), limits = c(0, 1.12),
                       sec.axis = sec_axis(trans = ~.*200, name = "Daily SARS-CoV-2 cases per\n100,000 people in Solothurn")) +
    xlab("Week") + 
    scale_x_yearweek(expand = c(0, 0), limits = as.Date(c("2020-11-15", 
                                                          "2022-02-02"))) +
    geom_line(aes(x = yearweek, y = cases_per_100k/223.67), size = 1, color = "black", se = FALSE) +
    geom_line(aes(x = yearweek, y = value, color = variable), size = 0.9) + 
    geom_bracket(xmin = as.POSIXct("2020-11-16"), xmax = as.POSIXct("2021-02-07"), 
                 y.position = 1.05, label = "Pre-VOC") +
    geom_bracket(xmin = as.POSIXct("2021-02-08"), xmax = as.POSIXct("2021-06-27"),
                 y.position = 1.05, label = "\u03B1") +
    geom_bracket(xmin = as.POSIXct("2021-06-28"), xmax = as.POSIXct("2021-12-26"),
                 y.position = 1.05, label = "\u03B4") +
    geom_bracket(xmin = as.POSIXct("2021-12-27"), xmax = as.POSIXct("2022-02-01"),
                 y.position = 1.05, label = "\u03BF") +
    labs(caption = "The coloured solid lines indicate the percentage of the key performance indicators over time. The thicker black line outlines the change\nin SARS-CoV-2 caseload. The Greek symbols \u03B1, \u03B4, and \u03BF indicate the periods during which the alpha, delta, and omicron SARS-CoV-2\nvariants of concern (VOC) were dominant, respectively. The dashed line indicates the point at which the system contacted individuals\nautomatically (1 January 2022).", 
         fill="Daily SARS-CoV-2 cases per\n100,000 people in Solothurn", 
         color = "Key performance indicators") +
    theme(plot.caption = element_text(hjust = 0)) +
    #annotate("text", x = as.POSIXct("2021-12-31"), y = 0.165, label = "X", size = 5)
    geom_vline(xintercept = as.Date("2021-12-31"), 
               linetype='dashed', color='black', size=0.7) +
    theme(panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank()) +  # Remove minor grid lines
    scale_colour_discrete(labels = c("Outcome KPI O1: % of new cases who were\nalready in quarantine by time of positive test",
                                                           "\nOutcome KPI O2: % of index cases in isolation\nby day of testing",
                                                           "\nProcess KPI P6: % of test results returned on\nsame or next day",
                                                           "\nProcess KPI P8: % of contacts notified on same\nor next day after interview with index"))
                        
  
  kpis_caseload_plot
  #save plot
  #Percentage of new cases who were already in quarantine by time of positive test
  
  
  tiff(file = "Figure3.tif", 
       width = 5000,
       height = 3000,
       res = 300)
  
  kpis_caseload_plot
  
  dev.off()
  
    
  
  
  
  ############################################################################
  #create list of figures
  figures_list <- list(KPIs1_6_cases_plot, KPIs7_12_cases_plot, workforce_plot,#1, 2, 3
                       structure_1_plot, outcome_1_plot, outcome_2_plot,#4, 5, 6
                       KPIs_process_plot_1, #7
                       KPIs_process_plot_3, missing_plot, wfcases_KPIs_plot, #8, 9. 10
                       cascade_plot, kpi7_8_15_bar_plot, #11, 12,
                       ncontacts_boxplot, cases_wf_plot, kpis_caseload_plot) #14, 15, 
  
}
