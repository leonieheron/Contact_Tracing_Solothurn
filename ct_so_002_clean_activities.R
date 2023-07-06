#' ---
#' title: "Classify activities"
#' author: "Leonie Heron"
#' date: "08/07/2022"
#' ---
#' 

ct_so_002_clean_activities = function(ct_so_index) {
  
  #recode activities
  
  #create factor variable from LocationActivity
  ct_so_index$LocationActivityCat <- NA
  #if no locations reported, state "None reported"
  ct_so_index$LocationActivityCat[is.na(ct_so_index$LocationStartDate) 
                                  & is.na(ct_so_index$LocationEndDate) 
                                  & is.na(ct_so_index$LocationActivity) 
                                  & is.na(ct_so_index$locationDescription) 
                                  & is.na(ct_so_index$locationActivityOthers)] <- "None reported"
  
  
  #Assign activities to each category
  Activity_work <- c("Arbeit","Arbeiten", "servieren")
  Activity_PrivateParty <- c("Privates Fest", "Bei Eltern", "Besuch der Schwägerin", 
                             "2 Wohnungen (5pers/2pers)", "Fam",  
                             "Familien-Event", "Familienbesuch", "Guetzeln", 
                             "Geburt", "Geburtstagsfeier",  
                             "Frauentreffenzu Hause bei jemandem", 
                             "Half einem Freund Elias Dixon 10015801", 
                             "in der Wohnung", "Geburtstagsfest", 
                             "jeden Tag eine andere Tochter mit Mann",  "mit Fam",  
                             "mit Familie",    "mit Freunden",   
                             "mit Kollegin unterwegs", "mit Schwester",  
                             "Mit Tochter und Fam",  "privat bei sich zuhause", 
                             "Private Feier", "Weihnachten", 
                             "Weihnachtsfeier", "Weihnachtsfest", 
                             "Wohnung gestrichen Inkl. Abends Meditation",
                             "Feier", "Ferien", 
                             "Treffen",  "Sitzung mit seinen Geschwistern", 
                             "Sohn&Enkel", "Spielen",
                             "Seniorentreff", "Nachbarskinder (s. KPs)")
  Activity_eating_home <- c("Essen mit Freunde (zu Hause)", 
                            "Essen bei den Eltern mit der Familie",  
                            "Essen mit Familie", 
                            "Essen mit Freunden", 
                            "Essen mit Freunden (zu Hause)", "Essen mit Freundin", 
                            "Essen mit Freundin und befreundetem Paar",
                            "Feier (Essen mit Freunden)",
                            "Abendessen mit Eltern",
                            "Abendessen", "Apero", "Essen", 
                            "Essen\r\n(danach auf den Friedhof)", "Essen/Kochen", 
                            "Essen/Trinken", "Geimeinsames Essen", "Mittagessen",  
                            "Morgenessen (1h), Mittagssessen (1.5h)", "Kaffee trinken",
                            "Kochen und essen", "Nachtessen",  
                            "Racletteessen bei IP zuhause", "Brotkorb", 
                            "Zusammen etwas trinken",  "Trinken",  
                            "Stammtisch (Feierabend-Bier)")
  Activity_restaurant <- c("Essen im Restaurant", "Restaurant Martinshof Zuchwil")
  Activity_bar <- c("Bar / Club", "Bar", "Bar / Trinken", "Bar/Club", "Bars, Clubs", "Club")
  Activity_publicevent <- c("Öffentliche Veranstaltung", "Kleintheater", "Kultur- und Eventlokal")
  Activity_sportingevent <- "Sportveranstaltung (Zuschauer)"
  Activity_fitness <- c("Fitnesscenter / Sportclub / Training", "Ballet", 
                        "Golf", "Akrobatik Turnasium", "Eislaufen", 
                        "Schlitteln (am 17.01.21)", "Schneeschuhwandern", 
                        "Lacross Training", "Langlauf", "Skifahren", "Skiferien", 
                        "Unihockey", "Sport in Turnhalle", "Schulsport Tanzen", 
                        "Frauengrüppli")
  Activity_sing <- c("Chorproben, gemeinsames Singen", "Singen")
  Activity_camp <- c("Lager oder andere auswärtige Aufenthalte mit geteilten Schlaf- und Waschmöglichkeiten",
                     "Camping", "Camping im Wohnwagen", "Camping Paradis Plage", 
                     "Camping platz Oberiberg", "Camping Tcs", "Camping Gardenia")
  Activity_travelforeign <- c("Reise in ein Risikoland / nach Hause aus einem Risikoland (gemäss Liste BAG)", 
                              "Verlegung Kosovo-Schweiz", "Aufenthalt in Kosovo",
                              "Ferien Italien Sizilien", "Flug", "Flugreise", 
                              "Flugzeug/Reise", "Flugreise von Kosovo", 
                              "Flug Belgien - Zürich", "Flug nach Spanien", 
                              "Reise", "Flughafen", "Flughafen Zürich", "Flugzeug", "Porto",
                              "Flug Aus Indien", "Flug Bosnien-Schweiz", 
                              "Flug Gran Canaria-Zürich", "Flug Heathrow-Zürich Swiss LX333", 
                              "Flug Istanbuhl/ZRH", "Flug London-Zürich", "Flug Mykonos-Basel", 
                              "Flug nach Basel Mulhouse", "Flug Serbien- Schweiz", "Flug Türkei-Schweiz",
                              "Nordmazedonien")
  Activity_other <- c("Gitarre spielen", "Streicher-Ensemble", "Andere", "\"Ausgang\"", "19 Uhr bis 20 Uhr", 
                      "abholung in der Psychiatrie", "Garage Peier", 
                      "Kaserne Liestal", "Operationstermin", "Physio", 
                      "Physiotherapie", "RehaClinic", "Reparatur", 
                      "Thai Massage", "OBI Dreispitz", "Fusspflege", "Landi", 
                      "Ministrieren",   "Notz Group",   "Siehe Attachments", 
                      "Zügeln", "Deutsch Kurs", "Deutschkurs A1", 
                      "Deutschland Einkaufen", "Kurs A22 (VHS)",
                      "Ausflug","Urlaub", "Jugi", 
                      "Lehrer", "Tochter abgeholt")
  Activity_school <- c("Schule", "Unterricht", "Lernen mit Freunden", "Stellvertretung Schule")
  
  
  
  for (i in 1:nrow(ct_so_index)){
    if(ct_so_index$LocationActivity[[i]] %in% Activity_work) ct_so_index$LocationActivityCat[[i]] <- "Work"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_PrivateParty) ct_so_index$LocationActivityCat[[i]] <- "Private party"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_eating_home) ct_so_index$LocationActivityCat[[i]] <- "Dining with friends (home)"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_restaurant) ct_so_index$LocationActivityCat[[i]] <- "Eating in restaurant"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_bar) ct_so_index$LocationActivityCat[[i]] <- "Bar / club"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_publicevent) ct_so_index$LocationActivityCat[[i]] <- "Public event"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_sportingevent) ct_so_index$LocationActivityCat[[i]] <- "Sporting event (spectator)"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_fitness) ct_so_index$LocationActivityCat[[i]] <- "Fitness centre / sports"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_sing) ct_so_index$LocationActivityCat[[i]] <- "Choir/singing"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_camp) ct_so_index$LocationActivityCat[[i]] <- "Camping or shared accommodation"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_travelforeign) ct_so_index$LocationActivityCat[[i]] <- "Foreign travel"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_other) ct_so_index$LocationActivityCat[[i]] <- "Other"
    if(ct_so_index$LocationActivity[[i]] %in% Activity_school) ct_so_index$LocationActivityCat[[i]] <- "School"
    
  }
  
  
    # #transform to character vector
  # ct_so_index <- transform(ct_so_index,LocationActivityCat = unlist(LocationActivityCat))
  # #convert to factor
  # ct_so_index$LocationActivityCat <- factor(ct_so_index$LocationActivityCat)
  # str(ct_so_index$LocationActivityCat)
  # levels(ct_so_index$LocationActivityCat)

  #school classify
  nonwork <- c("In der Schule wurde am 22.1. seine Klassenlehrerin Frau Tabea Meier positiv auf COVID-19 getestet.",
               "Primarschülerin",
               "Schule Sekundarstufe",
               "Sekundarschule, Kreisschule Thal",
               "Kantonsschule",
               "Rückfahrt Bern Berufsschule-Biberist RBS",
               "Schule an der Kantonsschule Solothurn",
               "Schule, KSSO",
               "login Berufsbildung AG",
               "Kso",
               "Hinfahrt Biberist RBS-Bern Berufsschule",
               "Berufsschule",
               "Phoenix Schule für KomplementärTherapie GmbH",
               "Berufsschule",
               "Lernen mit Schulkollege",
               "Volkshochschule (Deutsch integrationskurs VHS)",
               "Schulhaus Oberdorf Oensingen",
               "Schule meine tochter ist positiv",
               "Schulunterricht",
               "school",
               "Primarschule",
               "Unterricht in der Volksschule",
               "Schule",
               "Schule Bannfeld",
               "Schule Eicholz", "Schullager Klasse 6b", "Schullager Klasse 6b Riedholz",
               "Schule in Olten")
  ct_so_index$LocationActivityCat[ct_so_index$locationDescription %in% nonwork] <- "School"
  ct_so_index$LocationActivityCat[ct_so_index$locationActivityOthers %in% nonwork] <- "School"
  ct_so_index$LocationActivityCat[ct_so_index$locationActivityOthers == "Schule" & ct_so_index$age < 18] <- "School"
  
  
  #more people reclassified to school based on description and age
  IDs <- c(9909504, 10012825, 9541833, 9909388)
  ct_so_index$LocationActivityCat[ct_so_index$Doc_ID %in% IDs] <- "School"
  ct_so_index$LocationActivityCat[ct_so_index$Doc_ID == 9320478] <- "Eating in restaurant"
  eatathome <- c("BESUCH MEINER MUTTER IN BERN - GEMEINSAMES ABENDESSEN - SIE LEBT ALLEINE IN EINER WOHNUNG, HAT SONST KEINE KONTAKTE - SIE IST INFORMIERT UEBER MEINEN POSITIVEN TEST",
                 "Bräteln im Wald mit Freunden",
                 "Essen bei Tante und Cousä",
                 "Essen beim vater",
                 "Essen im Familienkreis",
                 "Essen mit Eltern",
                 "Essen mit Familie",
                 "Essen mit Sohn und Tochter zu Hause/gemeinsames Wohnen/Kochen/Benutzung der Sanitärräume",
                 "Familienessen",
                 "Mittagessen mit Freundinnen",
                 "Neujahrsapero",
                 "Familienessen",
                 "Essen",
                 "Geschäftsapéro",
                 "Going out with friends",
                 "Kaffee trinken",
                 "Kaffee trinken bei den Eltern",
                 "Kaffee trinken, privat",
                 "Kafi trinken 30 min",
                 "Abendessen",
                 "Draussen trinken mit Freunden",
                 "Essen",
                 "Essen mit Arbeitern von Mann",
                 "Essen mit Freund",
                 "Essen mit Freundin",
                 "Kaffe & essen",
                 "Kaffee trinken mit 3 Personen Privat",
                 "Wiehnachtessen privat",
                 "Essen bei einer Freundin",
                 "Kaffee trinken mit 3 Personen Privat",
                 "Essen mit Freundin",
                 "Essen mit Arbeitern von Mann",
                 "Draussen trinken mit Freunden",
                 "Kaffe & essen",
                 "Essen mit Freund",
                 "Essen mit Freundin",
                 "Wiehnachtessen privat",
                 "Essen",
                 "Abendessen",
                 "1 Espresso trinken",
                 "Essen mit Sohn")
  ct_so_index$LocationActivityCat[ct_so_index$locationActivityOthers %in% eatathome] <- "Dining with friends (home)"
  ct_so_index$LocationActivityCat[ct_so_index$locationDescription %in% eatathome] <- "Dining with friends (home)"
  
  public <- "Stadtführung (Aussenbereich, alle haben Schutzmasken getragen)"
  ct_so_index$LocationActivityCat[ct_so_index$locationDescription %in% public] <- "Public event"
  sing <- "Katholischer Kinderchor"
  ct_so_index$LocationActivityCat[ct_so_index$locationActivityOthers %in% sing] <- "Choir/singing"
  
  bars <- c("Zum Grotto",
            "Eleven Shisha Bar",
            "MIR Shisha Bar",
            "Shiva Bar Lounge",
            "nineTYnine - BAR | LOUNGE | FUMOIR | SHISHA")
  ct_so_index$LocationActivityCat[ct_so_index$locationDescription %in% bars] <- "Bar / club"
  ct_so_index$LocationActivityCat[ct_so_index$locationActivityOthers %in% bars] <- "Bar / club"
  
  camping <- "Berghütte"
  ct_so_index$LocationActivityCat[ct_so_index$locationDescription %in% camping] <- "Camping or shared accommodation"
  
  working <- c("Arbeit", "Arbeit am Skilift", "Arbeit am Skilift (Nachtskifahren)", 
               "Arbeit an einem Projekt", "Arbeit im Spital", "Arbeiten", 
               "Arbeiten im Bürgerspital Solothurn", "Arbeitsort", "Büro",
               "EWG Personalbesprechung  / Schutzmassnahmen waren getroffen Desinfektion- Abstand- Schutzmaske",
               "Auf der Arbeit",
               "Gespräche bei der Arbeit",
               "Prüfung bei der Benedikt Schule in Bern",
               "Projektarbeit",
               "Schulbetrieb")
  ct_so_index$LocationActivityCat[ct_so_index$locationActivityOthers %in% working] <- "Work"
  ct_so_index$LocationActivityCat[ct_so_index$locationDescription %in% working] <- "Work"
  
  privatemeeting <- c("Aufenthalt der Grossmutter",
                      "Ausflug nach Walensee, zusammen im gleichen Auto mit gemeldeten KP",
                      "Bei meiner Freundin zuhause",
                      "Bei Mutter Essen abholen",
                      "Besuch",
                      "Besuch aus Familie",
                      "Besuch bei der Tochter in Montlingen (Christina Lehmann) 28.12.2020-30.12.2020 Ganze Familie ist dort in Quarantäne",
                      "Besuch bei Kollegin",
                      "Besuch bei meiner Mama zu Hause",
                      "Besuch bei Schwester",
                      "Besuch bei Schwiegereltern",
                      "Besuch beim Partner",
                      "Besuch der Eltern in Deutschland",
                      "Besuch Freundin",
                      "Besuch meiner Mutter in der Altersresidenz",
                      "Besuch mit abstand 45.min.",
                      "Besuch Opa",
                      "Besuch von 1 Person bei uns zuhause",
                      "Besuch von Bruder",
                      "Besuch von Bruder aus dem Kanton Zürich",
                      "Besuch von Covid-Kranker Grossmutter im Sterben.",
                      "Besuch von Tochter Vera Dreier (hat am 23.11. Positives Testresultat erhalten, bereits in Isolation), mit Maske",
                      "Besuche von Tochter Sara Dreier, 079 736 94 13, an diversen Tagen, immer mit Masken und Abstand (Sara am 23.11. negativ getestet)",
                      "Treffen in Privatwohnung",
                      "Eltern",
                      "Familenbesammensein",
                      "Familiebesuch",
                      "Familienbesuch",
                      "Familientreff",
                      "Treffen mit Partner",
                      "Freunde",
                      "Fest bei Kollege",
                      "Parteiversammlung",
                      "Weihnachtsfeier",
                      "Weihnachtsessen am 24.12.2020 mit der Familie",
                      "Neujahresfest",
                      "Weihnachten zuhause",
                      "Zusammen mit Freunden im Hotel Luin",
                      "Geburt meines Kindes",
                      "Geburtstag",
                      "Geburtstagsbesuch",
                      "Privates Treffen",
                      "Privatwohnung",
                      "Silvester mit der Familie",
                      "Spieletag",
                      "Bei Kollegen geschlafen",
                      "Bei meinen Eltern zu Hause",
                      "Besuch Eltern",
                      "Besuch Freunde",
                      "Besuch Kindern",
                      "Besuch Schwiegertochter mit Enkel",
                      "Familienfest",
                      "Weihnachtsfest",
                      "Filmabend mit einem Freund",
                      "Fondueabend mit Geschwistern",
                      "Geburtstag der IP",
                      "Geburtstag der Tochter",
                      "Geburtstagsfest der Tochter",
                      "Geburtstagsfest der Tochter",
                      "Geburtstagsfest Tante",
                      "Geburtstagsfest vom Vater",
                      "Nachbarin",
                      "Silvester",
                      "Silvester im Familienkreis",
                      "Silvester mit Familie",
                      "Silvester mit KPs",
                      "Silvesteressen mit Schwager & Grossvater",
                      "Silvesterfeier",
                      "Silvesterfeier mit Familie (KPs)",
                      "Spielnachmittag mit Schwestern",
                      "Treffen Familie anlässlich ihrem Geburtstag",
                      "Treffen Freunde",
                      "Treffen mit Coach",
                      "Treffen mit Freunden",
                      "Treffen mit Freundin Zuhause",
                      "Weihanchtsfeier mit Familie draussen",
                      "Weihnachten",
                      "Weihnachten bei Sohn in Härkingen",
                      "Weihnachten bei Sohn in Niederbuchsiten",
                      "Weihnachtsfeier (mit Sohn)",
                      "Weihnachtsfeier (mit Tochter)",
                      "Weihnachtsfeier mit Familie",
                      "Weihnachtsfeier mit Familie von Tochter",
                      "Weihnachtsfeier mit Freundin",
                      "Weihnachtsfeier mit Tochter",
                      "Besuch bei Freundin",
                      "2. Besuch bei Freundin",
                      "Beerdignung Vater",
                      "Treffen mit Freundin",
                      "Besuch bei Kollegen",
                      "Weihnachtsfeier Familie seitens IP beim Sohn",
                      "Weihnachtsfeier Familie seitens Partnerin",
                      "Beerdigung",
                      "Besuch bei Schwester und deren Familie",
                      "Mit Geschwistern")
  ct_so_index$LocationActivityCat[ct_so_index$locationActivityOthers %in% privatemeeting] <- "Private party"
  ct_so_index$LocationActivityCat[ct_so_index$locationDescription %in% privatemeeting] <- "Private party"
  foreigntravel <- c("Auslandaufenthalt in Nordmazedonien",
                     "Auslandsaufenthalt",
                     "Reise nach Albanien",
                     "Reise nach Spanien",
                     "Reise Roumänien",
                     "Rückflug von Dubai-ZRH",
                     "Reise Österreich Salzburg/ Cool Mama Hotel",
                     "Ferien Kosovo",
                     "Ferien bei der Mutter in Kosovo",
                     "Reise nach und von Kosovo mit Auto",
                     "Tansania- Zansibar")
  ct_so_index$LocationActivityCat[ct_so_index$locationActivityOthers %in% foreigntravel] <- "Foreign travel"
  ct_so_index$LocationActivityCat[ct_so_index$locationDescription %in% foreigntravel] <- "Foreign travel"
  
  sport <- c("Badminton gespielt und Abendessen", 
             "Schlitteln",
             "FC Egerkingen",
             "Stockhorn, Wanderung",
             "Wandern auf dem Weissenstein",
             "Wandern",
             "Walking im freien mit dem geforderten Abstand",
             "Walken",
             "Snow tubing",
             "Langlauf",
             "Line Dance",
             "Schlitteln & spielen mit Freund aus Schule",
             "Schlittschuhfahren Event VIS",
             "Schlittschuhlaufen",
             "Ski- Snowboardfahren",
             "Ski Ferien",
             "Skiausflug",
             "Skifahren",
             "Skifahren breuleux",
             "Skifahren in Zermatt",
             "Skifahrern", "Fitness", "Fitness / Sportclub / Training",
             "skiferien", "Fitnesscenter",
             "Skiferien in Hotel",
             "Skiferien über Silvester im Wallis, 30.12.20 - 02.01.21",
             "Skitage",
             "Spazieren",
             "Spaziergang",
             "Spaziergang mit Eltern",
             "Sport (draussen)",
             "Sport/Restaurantbesuch",
             "Freizeitaktivität - Geräteturnen leiten mit Maske",
             "Freizeitaktivität im Tanzkeller Solothurn - Tanzen mit Maske",
             "Karatetraining (strikte Einhaltung Schutzkonzept - Räumliche Distanz 2.5m, sobald Platz verlassen wird Schutzmaske)",
             "Schlitteln in Grindelwald",
             "Schlitteln mit Familie",
             "Skiferien in der eigenen Ferienwohnung",
             "Skiferien in Schwenden (Diemtigthal) in einer Ferienwohnung.",
             "Skitour", "Schwimmkurs", "Ski", "Ski fahren", "Skiferien mit Freund*innen",
             "Skiweekend",
             "Spatzieren, zusammen Wohnen",
             "Spaziergang",
             "Spaziergang mit Schwiegereltern", "Schwimmkurs", "schwimmkurs auquafitness subingen",
             "Waldspaziergang", "Schwimmbad", "Schwimmbad Arlesheim", "Schwimmen", "Hallenbad",
             "Wandertag",
             "Skiferien (siehe KPs)",
             "Grindelwald Tourismus")
  ct_so_index$LocationActivityCat[ct_so_index$locationActivityOthers %in% sport] <- "Fitness centre / sports"
  ct_so_index$LocationActivityCat[ct_so_index$locationDescription %in% sport] <- "Fitness centre / sports"
  
  
  
  #restaurants wrongly classified as eating with friends at home - reclassify
  restaurants <- c("Restaurant/Landhotel Hirschen", "Siam Food (Restaurant)",
                   "Roter Turm",
                   "Restaurant Rose",
                   "Restaurant Oberrüttenen",
                   "Restaurant Linde",
                   "Restaurant Kreuz",
                   "Restaurant",
                   "Öufi Bier Brauere",
                   "Nachtessen im Restaurant Orient",
                   "La Couronne",
                   "Kaffeehalle",
                   "Restaurant Marti", "Restaurant Sonne", 
                   "Restaurant Pintli Iventhal",
                   "Ach'i",
                   "Blüemlismatt",
                   "Essen in der Kantine Olten",
                   "Ass.la Cantine",
                   "Bäckerei Café Wälchli",
                   "Cafe Bahnhöfli",
                   "Café Ring",
                   "McDonalds",
                   "Migros Restaurant",
                   "Rest. Baseltor",
                   "Rest. National",
                   "Restaurant Sternenpinte",
                   "Restaurant Pöstli",
                   "RESTAURANTEL ATEES BANANA",
                   "Salmen (Restaurant)",
                   "Rest. Ponte del Sole")
  ct_so_index$LocationActivityCat[ct_so_index$locationDescription %in% restaurants] <- "Eating in restaurant"
  ct_so_index$LocationActivityCat[ct_so_index$locationActivityOthers %in% restaurants] <- "Eating in restaurant"
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivity %in% restaurants] <- "Eating in restaurant"
  ct_so_index$LocationActivityCat[is.na(ct_so_index$LocationActivityCat)] <- "Other"
  rests2 <- c("Rest. Ponte del Sole", "Essen im Restaurant")
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivity %in% rests2] <- "Eating in restaurant"
  homeeat <- c("Essen mit Freunden (zu Hause)", "Mittagessen", "Abendessen")
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivity %in% homeeat] <- "Dining with friends (home)"
  
  #additional cleaning after checks
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivity == "Fitnesscenter / Sportclub / Training" & 
                                    ct_so_index$LocationActivityCat == "School"] <- "Fitness centre / sports"
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivity == "Arbeit" & 
                                    ct_so_index$LocationActivityCat == "School"] <- "Work"
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivity == "Chorproben, gemeinsames Singen" & 
                                    ct_so_index$LocationActivityCat == "School"] <- "Choir/singing"
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivity == "Rest. Baseltor" & 
                                    ct_so_index$LocationActivityCat == "Dining with friends (home)"] <- "Eating in restaurant"
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivity == "Restaurant Sternenpinte" & 
                                    ct_so_index$LocationActivityCat == "Dining with friends (home)"] <- "Eating in restaurant"
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivity == "Reise in ein Risikoland / nach Hause aus einem Risikoland (gemäss Liste BAG)" & 
                                    ct_so_index$LocationActivityCat == "Private party"] <- "Foreign travel"
  ct_so_index$LocationActivityCat[ct_so_index$LocationActivity == "Ski fahren" & 
                                    ct_so_index$LocationActivityCat == "Other"] <- "Fitness centre / sports"


  return(as.data.frame(ct_so_index))}
