# Libraries ----
library(readxl)
library(data.table)



# Caricamento ----
## Elenco Comuni Istat ----
# Fonte: ISTAT - https://www.istat.it/it/archivio/6789
# Dati aggiornati al 2023
temp.file <- paste(tempfile(),".xls", sep = "")
download.file("https://www.istat.it/storage/codici-unita-amministrative/Elenco-comuni-italiani.xls", temp.file, mode = "wb")

comuni_istat <- read_excel(temp.file) |> setDT() |>
  janitor::clean_names()

# 1=Provincia; 2=Provincia autonoma; 3=Città metropolitana; 4=Libero consorzio di comuni; 5=Unità non amministrativa (ex- province del Friuli-Venezia Giulia)
comuni_istat[, tipologia_di_unita_territoriale_sovracomunale := fcase(tipologia_di_unita_territoriale_sovracomunale == 1, "Provincia",
                                                                      tipologia_di_unita_territoriale_sovracomunale == 2, "Provincia autonoma",
                                                                      tipologia_di_unita_territoriale_sovracomunale == 3, "Città metropolitana",
                                                                      tipologia_di_unita_territoriale_sovracomunale == 4, "Libero consorzio di comuni",
                                                                      tipologia_di_unita_territoriale_sovracomunale == 5, "Unità non amministrativa",
                                                                      default = NA)]
## Perifericità ----
# Fonte: https://www.istat.it/it/archivio/273176
# Dati aggiornati al 2022
temp.file <- paste(tempfile(),".xlsx", sep = "")
download.file("https://www.istat.it/it/files//2022/07/20220715_Elenco_Comuni_Classi_di_Aree_Interne.xlsx", temp.file, mode = "wb")

classificazione_periferia_raw <- read_excel(temp.file, skip = 2) |> setDT() |>
  janitor::clean_names()

classificazione_periferia <- classificazione_periferia_raw[, .(codice_comune_formato_numerico = procom_n, denominazione_in_italiano = denominazione_comune, classe_periferia = descrizione_aree_interne_2021_2027)]

rm(classificazione_periferia_raw)

## Popolazione ----
# Fonte: https://demo.istat.it - Popolazione residente
# Dati aggiornati al 2023
popolazione_raw <- fread("curl https://demo.istat.it/data/posas/POSAS_2023_it_Comuni.zip | funzip") |>
  setDT() |>
  janitor::clean_names()

popolazione_selected <- popolazione_raw[eta == 999][, popolazione := totale_maschi + totale_femmine][, .(codice_comune_formato_numerico = codice_comune,
                                                                                                        denominazione_in_italiano = comune,
                                                                                                        popolazione)]

rm(popolazione_raw)

## CAP 1 ----
# Fonte: Github repository opendatasicilia 
# Dati aggiornati 9 mesi fa
temp.file <- paste(tempfile(),".csv", sep = ",")
download.file("https://raw.githubusercontent.com/opendatasicilia/comuni-italiani/main/dati/cap.csv", temp.file, mode = "wb")
cap1 <- read.csv(temp.file) |> setDT() |>
  janitor::clean_names() |>
  setnames("pro_com_t", "codice_comune_formato_numerico")

cap1[, codice_comune_formato_alfanumerico := stringr::str_pad(as.character(codice_comune_formato_numerico), width = 6, side = "left", pad = "0")]

## CAP 2 ----
# Fonte: Opendata IPA - https://indicepa.gov.it/ipa-dati/
# Dati aggiornati al 2023
## CAP Multipli
temp.file <- paste(tempfile(),".xlsx", sep = "")
download.file("https://indicepa.gov.it/ipa-dati/dataset/5baa3eb8-266e-455a-8de8-b1f434c279b2/resource/d09adf99-dc10-4349-8c53-27b1e5aa97b6/download/enti.xlsx", temp.file, mode = "wb")

cap2_raw <- read_excel(temp.file) |> setDT() |>
  janitor::clean_names()

cap2 <- cap2_raw[, .(codice_comune_istat, codice_catastale_comune, cap)]
cap2 <- cap2[!duplicated(cap2)]
cap2[, cap := as.numeric(cap)] |>
  setnames(c("codice_comune_istat", "codice_catastale_comune"), c("codice_comune_formato_alfanumerico", "codice_catastale_del_comune"))

rm(cap2_raw)

## CAP 3 ----
#cap3_raw <- readxl::read_xlsx(file.path("input", "comuni_CAP_ich.xlsx")) |>
#  setDT() |>
#  janitor::clean_names() |>
#  setnames(c("cod_istat", "cod_istat_ch"), c("codice_comune_formato_numerico", "codice_comune_formato_alfanumerico"))

#cap3 <- cap3_raw[, .(codice_comune_formato_numerico, codice_comune_formato_alfanumerico, cap)]
#cap3[, cap := as.numeric(cap)]

#rm(cap3_raw)

## CAP 4 ----
cap_generici_raw <- pdftools::pdf_text(pdf = "https://www.sanpaolostore.it/media/faq/elenco-cap.pdf")
cap_generici_split <- strsplit(cap_generici_raw, "\n")
cap_generici_clean <- cap_generici_split[[1]][5:45]
cap_generici <- as.data.table(stringr::str_split_fixed(cap_generici_clean, " {2,}", 10)[, 1:2]) |>
  setnames(c("cap", "denominazione_in_italiano"))

cap_generici[, denominazione_in_italiano := stringr::str_to_title(denominazione_in_italiano)]
cap_generici[, denominazione_in_italiano := fifelse(denominazione_in_italiano == "Reggio Emilia", "Reggio nell'Emilia",
                                                    fifelse(denominazione_in_italiano == "Reggio Calabria", "Reggio di Calabria", denominazione_in_italiano))]

cap4 = merge(cap_generici, comuni_istat[, .(codice_comune_formato_alfanumerico, codice_comune_formato_numerico, denominazione_in_italiano)], by = "denominazione_in_italiano", all.x = T)
cap4[, cap := as.numeric(cap)]

rm(cap_generici_raw, cap_generici_split, cap_generici_clean, cap_generici)

# CAP 5 ----
cap_nuovi <- c("00011", "00072", "00073", "00075", "00076", "00077", "00078",
               "00079", "00163", "20005", "37127", "63017",
               "00074", "07051", "09050", "09067", "09068", "09069",
               "16135", "16139", "16143",
               "16155", "19137", "20003", "20004", "20006",
               "20007", "20008", "20009", "20031", "20033", "20034", "20036",
               "20038", "20039", "20041", "20043", "20044", "20045", "20046",
               "20047", "20048", "20049", "20051", "20052", "20054", "20057",
               "20058", "20059", "20071", "20072", "20074", "20076",
               "22042",
               "46021", "48034",
               "51034", "63030", "63033",
               "82014", "15047",
               "16146", "16165", "20075",
               "70057", "00050", "50030", "19030", "50130", "52040", "12057")


denominazioni_nuovi <- c("Tivoli", "Ariccia", "Castel Gandolfo", "Lanuvio", "Lariano", "Monte Compatri", "Monte Porzio Catone",
                         "Rocca Priora", "Roma", "Pogliano Milanese", "Verona", "Porto San Giorgio",
                         "Nemi", "Budoni", "Pula", "Elmas", "Uta", "Maracalagonis",
                         "Genova", "Genova", "Genova",
                         "Genova", "La Spezia", "Casorezzo", "Arluno", "Pregnana Milanese",
                         "Cornaredo", "Bareggio", "Vittuone", "Cesate", "Solaro", "San Giorgio su Legnano", "Dairago",
                         "Busto Garolfo", "Canegrate", "Bussero", "Vanzago", "Arese", "Lainate", "Cisliano",
                         "Cusago", "Pantigliate", "Settala", "Cassina de' Pecchi", "Vignate", "Segrate", "Assago",
                         "Zibido San Giacomo", "Casarile", "Vermezzo con Zelo", "Pieve Emanuele", "Carpiano", "Mediglia",
                         "San Fermo della Battaglia",
                         "Borgocarbonara", "Fusignano",
                         "Serravalle Pistoiese", "Carassai", "Monteprandone",
                         "Ceppaloni", "Alluvioni Piovera",
                         "Genova", "Genova", "Colturano",
                         "Bari", "Fiumicino", "Barberino di Mugello", "Ameglia", "Firenze", "Civitella in Val di Chiana", "Neive")

cap5_raw <- as.data.table(cbind(cap_nuovi, denominazioni_nuovi)) |>
  setnames(c("cap_nuovi", "denominazioni_nuovi"), c("cap_ch", "denominazione_in_italiano"))

cap5_raw[, cap := as.numeric(cap_ch)]

cap5 = merge(cap5_raw, comuni_istat[, .(denominazione_in_italiano, codice_comune_formato_alfanumerico, codice_comune_formato_numerico)], by = "denominazione_in_italiano", all.x = T)

rm(cap_nuovi, denominazioni_nuovi, cap5_raw)



# Analisi ----
uniqueN(comuni_istat$codice_comune_formato_numerico); uniqueN(popolazione_selected$codice_comune_formato_numerico); uniqueN(classificazione_periferia$codice_comune_formato_numerico)

## Controllo dati aggiuntivi ----
# Nota. Dal 22 gennaio 2024 sono istituiti mediante fusione di 6 comuni le nuove unità di: Sovizzo (VI), Setteville (BL) e Santa Caterina d’Este (PD). 
# Dal 1° gennaio 2024 è istituito il comune di Uggiate con Ronago mediante fusione di 2 comuni nella provincia di Como. Complessivamente il numero dei comuni risulta essere di 7.896 unità.
## Metto d'accordo elenco Istat e dati sulla popolazione
setdiff(comuni_istat$codice_comune_formato_numerico, popolazione_selected$codice_comune_formato_numerico)
# 4 comuni nuovi senza dati popolazione (13256 24128 25075 28108)
comuni_istat[codice_comune_formato_numerico %in% setdiff(comuni_istat$codice_comune_formato_numerico, popolazione_selected$codice_comune_formato_numerico), denominazione_in_italiano]
#"Uggiate con Ronago" "Sovizzo" "Setteville" "Santa Caterina d'Este"
setdiff(popolazione_selected$codice_comune_formato_numerico, comuni_istat$codice_comune_formato_numerico)
# 9 comuni vecchi che non sono nell'elenco Istat aggiornato (25002 18002 28022 24044 25070 13199 24103 13228 28098)
popolazione_selected[codice_comune_formato_numerico %in% setdiff(popolazione_selected$codice_comune_formato_numerico, comuni_istat$codice_comune_formato_numerico), denominazione_in_italiano]
#"Alano di Piave" "Albaredo Arnaboldi" "Carceri" "Gambugliano" "Quero Vas" "Ronago" "Sovizzo" "Uggiate-Trevano" "Vighizzolo d'Este" 
# Aggiornamento dei dati sulla popolazione
# Sovizzo + Gambugliano -> Sovizzo
popolazione_selected[codice_comune_formato_numerico %in% c(24103, 24044), ':=' (codice_comune_formato_numerico = 24128,
                                                                                denominazione_in_italiano = "Sovizzo",
                                                                                popolazione = sum(popolazione))]
# Quero Vas + Alano di Piave -> Setteville
popolazione_selected[codice_comune_formato_numerico %in% c(25070, 25002), ':=' (codice_comune_formato_numerico = 25075,
                                                                                denominazione_in_italiano = "Setteville",
                                                                                popolazione = sum(popolazione))]
# Carceri + Vighizzolo d'Este -> Santa Caterina d'Este
popolazione_selected[codice_comune_formato_numerico %in% c(28022, 28098), ':=' (codice_comune_formato_numerico = 28108,
                                                                                denominazione_in_italiano = "Santa Caterina d'Este",
                                                                                popolazione = sum(popolazione))]
# Ronago + Uggiate-Trevano -> Uggiate con Ronago
popolazione_selected[codice_comune_formato_numerico %in% c(13199, 13228), ':=' (codice_comune_formato_numerico = 13256,
                                                                                denominazione_in_italiano = "Uggiate con Ronago",
                                                                                popolazione = sum(popolazione))]
# Campospinoso + Albaredo Arnaboldi -> Campospinoso Albaredo
popolazione_selected[codice_comune_formato_numerico %in% c(18026, 18002), ':=' (codice_comune_formato_numerico = 18026,
                                                                                denominazione_in_italiano = "Campospinoso Albaredo",
                                                                                popolazione = sum(popolazione))]

popolazione_selected = popolazione_selected[!duplicated(popolazione_selected)]

# Metto d'accordo elenco Istat e classificazione perifericità
setdiff(comuni_istat$codice_comune_formato_numerico, classificazione_periferia$codice_comune_formato_numerico)
# 9 comuni nuovi senza dati popolazione (5122 12144 13256 24128 25075 28108 99030 99031 81025)
comuni_istat[codice_comune_formato_numerico %in% setdiff(comuni_istat$codice_comune_formato_numerico, classificazione_periferia$codice_comune_formato_numerico), denominazione_in_italiano]
# "Moransengo-Tonengo", "Bardello con Malgesso e Bregano", "Uggiate con Ronago", "Sovizzo", "Setteville", "Santa Caterina d'Este", "Montecopiolo", "Sassofeltrio", "Misiliscemi"                    
setdiff(classificazione_periferia$codice_comune_formato_numerico, comuni_istat$codice_comune_formato_numerico)
# 16 comuni vecchi che non sono nell'elenco Istat aggiornato (5079, 5110, 12009, 12018, 12095, 13199 13228 18002 24044 24103 25002 25070 28022 28098, 41033, 41060)
classificazione_periferia[codice_comune_formato_numerico %in% setdiff(classificazione_periferia$codice_comune_formato_numerico, comuni_istat$codice_comune_formato_numerico), denominazione_in_italiano]
#"Moransengo", "Tonengo", "Bardello", "Bregano", "Malgesso", "Ronago", "Uggiate-Trevano", "Albaredo Arnaboldi", "Gambugliano", "Sovizzo", "Alano di Piave", "Quero Vas", "Carceri", "Vighizzolo d'Este", "Montecopiolo", "Sassofeltrio"
# Aggiornamento dei dati sulla classificazione di perifericità
classificazione_periferia[codice_comune_formato_numerico %in% c(5079, 5110), ':=' (codice_comune_formato_numerico = 5122,
                                                                                   denominazione_in_italiano = "Moransengo-Tonengo",
                                                                                   classe_periferia = unique(classe_periferia))]
classificazione_periferia[codice_comune_formato_numerico %in% c(12009, 12018, 12095), ':=' (codice_comune_formato_numerico = 12144,
                                                                                            denominazione_in_italiano = "Bardello con Malgesso e Bregano",
                                                                                            classe_periferia = unique(classe_periferia))]
classificazione_periferia[, codice_comune_formato_numerico := fifelse(codice_comune_formato_numerico == 41033, 99030, 
                                                                      fifelse(codice_comune_formato_numerico == 41060, 99031,
                                                                              codice_comune_formato_numerico))]

classificazione_periferia[codice_comune_formato_numerico %in% c(24103, 24044), ':=' (codice_comune_formato_numerico = 24128,
                                                                                denominazione_in_italiano = "Sovizzo",
                                                                                classe_periferia = unique(classe_periferia))]
classificazione_periferia[codice_comune_formato_numerico %in% c(25070, 25002), ':=' (codice_comune_formato_numerico = 25075,
                                                                                denominazione_in_italiano = "Setteville",
                                                                                classe_periferia = unique(classe_periferia))]
classificazione_periferia[codice_comune_formato_numerico %in% c(28022, 28098), ':=' (codice_comune_formato_numerico = 28108,
                                                                                denominazione_in_italiano = "Santa Caterina d'Este",
                                                                                classe_periferia = unique(classe_periferia))]
classificazione_periferia[codice_comune_formato_numerico %in% c(13199, 13228), ':=' (codice_comune_formato_numerico = 13256,
                                                                                denominazione_in_italiano = "Uggiate con Ronago",
                                                                                classe_periferia = unique(classe_periferia))]
classificazione_periferia[codice_comune_formato_numerico %in% c(18026, 18002), ':=' (codice_comune_formato_numerico = 18026,
                                                                                denominazione_in_italiano = "Campospinoso Albaredo",
                                                                                classe_periferia = unique(classe_periferia))]

classificazione_periferia = classificazione_periferia[!duplicated(classificazione_periferia)]

classificazione_periferia = rbind(classificazione_periferia, data.table(codice_comune_formato_numerico = 81025, denominazione_in_italiano = "Misiliscemi", classe_periferia = "C - Cintura"))

comuni_isolati <- classificazione_periferia[tolower(classe_periferia) %like% "periferico", codice_comune_formato_numerico]


## CAP totali ----
tot_cap = rbind(cap1[, .(codice_comune_formato_alfanumerico, cap)], 
                cap2[, .(codice_comune_formato_alfanumerico, cap)], 
                #cap3[, .(codice_comune_formato_alfanumerico, cap)], 
                cap4[, .(codice_comune_formato_alfanumerico, cap)],
                cap5[, .(codice_comune_formato_alfanumerico, cap)])
tot_cap[, cap_ch := stringr::str_pad(as.character(cap), width = 5, side = "left", pad = "0")]
tot_cap = tot_cap[!duplicated(tot_cap)]

uniqueN(tot_cap$cap)



# Merge ----
comuni_cap <- merge(comuni_istat, tot_cap, by = "codice_comune_formato_alfanumerico", all = T)
uniqueN(comuni_cap$cap)

comuni_cap_pop <- merge(comuni_cap, popolazione_selected[, .(codice_comune_formato_numerico, popolazione)], by = "codice_comune_formato_numerico", all.x = T)

comuni_cap_pop[is.na(cap), denominazione_in_italiano]

comuni_cap_pop[is.na(cap), cap := fifelse(denominazione_in_italiano == "Moransengo-Tonengo", 14023,
                            fifelse(denominazione_in_italiano == "Valdaone", 38091, 
                                    fifelse(denominazione_in_italiano == "Borgo d'Anaunia", 38013, cap)))]

comuni_cap_pop[cap %in% c(47890, 47891, 47892, 47893, 47894, 47895, 47896, 47897, 47898, 47899), 
               ':=' (denominazione_in_italiano = fcase(cap == 47890, "Città di San Marino",
                                                  cap == 47891, "Dogana",
                                                  cap == 47892, "Acquaviva",
                                                  cap == 47893, "Borgo Maggiore",
                                                  cap == 47894, "Chiesanuova",
                                                  cap == 47895, "Domagnano",
                                                  cap == 47896, "Faetano",
                                                  cap == 47897, "Fiorentino",
                                                  cap == 47898, "Montegiardino",
                                                  cap == 47899, "Serravalle",
                                                  default = NA),
                     denominazione_regione = "Repubblica di San Marino")]

comuni_cap_pop[cap == 120, ':=' (denominazione_in_italiano = "Città del Vaticano",
                                 denominazione_regione = "Stato della Città del Vaticano")]

comuni_cap_pop = comuni_cap_pop[!is.na(denominazione_in_italiano) & cap %in% comuni_cap_pop[!is.na(denominazione_in_italiano), cap]]


comuni_cap_selected <- comuni_cap_pop[, .(cap, 
                                      codice_comune_formato_numerico, 
                                      denominazione_italiana_e_straniera, 
                                      denominazione_in_italiano, 
                                      area_geografica = ripartizione_geografica, 
                                      regione = denominazione_regione, 
                                      provincia = denominazione_dell_unita_territoriale_sovracomunale_valida_a_fini_statistici, 
                                      sigla_provincia = sigla_automobilistica,
                                      tipologia_di_unita_territoriale_sovracomunale,
                                      capoluogo = flag_comune_capoluogo_di_provincia_citta_metropolitana_libero_consorzio,
                                      popolazione)]

comuni_cap_selected[is.na(cap)]

comuni_cap_selected[is.na(popolazione)]

comuni_cap_selected[denominazione_in_italiano == "Cesena" | denominazione_in_italiano == "Urbino", capoluogo := 1]

comuni_cap_selected[, tipologia_comune_di_abitazione := fcase(tipologia_di_unita_territoriale_sovracomunale != "Città metropolitana" & capoluogo == 0, "1. Comune non capoluogo",
                                                              capoluogo == 1, "2. Capoluogo di provincia",
                                                              tipologia_di_unita_territoriale_sovracomunale == "Città metropolitana" & capoluogo == 0, "3. Città metropolitana",
                                                              default = NA)]

comuni_cap_selected[, ampiezza_comune_di_abitazione := fcase(popolazione < 30000, "1. Meno di 30 mila abitanti",
                                                             popolazione >= 30000 & popolazione < 100000, "2. 30 - 100 mila abitanti",
                                                             popolazione >= 100000 & popolazione < 500000, "3. 100 - 500 mila abitanti",
                                                             popolazione >= 500000 & popolazione < 1000000, "4. 500 mila - 1 milione abi",
                                                             popolazione >= 1000000, "5. Oltre 1 milione abitanti",
                                                             default = NA)]

comuni_cap_selected[, area_geografica_abitazione := fcase(area_geografica %like% "Nord", "1. Nord",
                                                          area_geografica %like% "Centro", "2. Centro",
                                                          area_geografica %like% "Sud" | area_geografica %like% "Isole", "3. Sud-Isole",
                                                          default = NA)]

# writexl::write_xlsx(comuni_cap_selected[, .(cap, denominazione_in_italiano, denominazione_italiana_e_straniera, area_geografica_abitazione, ampiezza_comune_di_abitazione, tipologia_comune_di_abitazione)], file.path("output", "comuni_cap_riclassificati.xlsx"))



## Come grandi centri urbani considero le città metropolitane (come classificate dall'ISTAT)
# grandi_centri_urbani = unique(comuni_cap_selected[tipologia_di_unita_territoriale_sovracomunale == "Città metropolitana" & capoluogo == 1, codice_comune_formato_numerico])
grandi_centri_urbani_alt = unique(comuni_cap_selected[popolazione >= 200000, codice_comune_formato_numerico])

## Matrice di contiguità fra comuni ----
# Fonte: https://www.istat.it/it/archivio/157423
# Dati aggiornati al 2018

url <- "https://www.istat.it/it/files//2015/04/MatriciContiguita_Comuni_2011_2018.zip"
temp <- tempfile()
temp2 <- tempfile()
download.file(url, temp)
unzip(zipfile = temp, exdir = temp2, unzip = "unzip")

matrice_contiguita <- read_xls(file.path(temp2, "MatriciContiguita_Comuni_2011_2018/MatriceContigui2018.xls")) |> setDT() |>
    janitor::clean_names()

matrice_contiguita_grandi <- matrice_contiguita[pro_com_comune %chin% grandi_centri_urbani_alt]

# province = unique(comuni_cap_selected[capoluogo == 1, codice_comune_formato_numerico])
# matrice_contiguita_province <- matrice_contiguita[pro_com_comune %chin% province]
# matrice_contiguita_isolati <- matrice_contiguita[!(pro_com_comune %chin% matrice_contiguita_province$pro_com_comune_adiacente)][!(pro_com_comune_adiacente %chin% matrice_contiguita_province$pro_com_comune_adiacente)]
# 
# comuni_isolati = unique(matrice_contiguita_isolati$pro_com_comune_adiacente)

comuni_limitrofi_grandi_centri_urbani = unique(matrice_contiguita_grandi$pro_com_comune_adiacente)

comuni_cap_selected[, limitrofo_grande_centro_urbano := fifelse(as.character(codice_comune_formato_numerico) %chin% as.character(comuni_limitrofi_grandi_centri_urbani), 1, 0)]
comuni_cap_selected[, isolato := fifelse(as.character(codice_comune_formato_numerico) %chin% as.character(comuni_isolati), 1, 0)]

comuni_cap_selected[, sede_abituale_di_lavoro := fcase(limitrofo_grande_centro_urbano == 1, "Nella periferia o in un comune limitrofo a un grande centro urbano",
                                                       limitrofo_grande_centro_urbano == 0 & popolazione < 30000 & isolato == 0, "In un piccolo comune (meno di 30 mila abitanti)",
                                                       limitrofo_grande_centro_urbano == 0 & popolazione < 30000 & isolato == 1, "In una zona isolata da insediamenti urbani",
                                                       limitrofo_grande_centro_urbano == 0 & popolazione >= 30000 & popolazione < 200000, "In un medio centro urbano (tra 30 mila e 200 mila abitanti)",
                                                       limitrofo_grande_centro_urbano == 0 & popolazione >= 200000, "In un grande centro urbano (Oltre 200 mila abitanti)",
                                                       default = NA)]

comuni_cap_selected[, costo_vita_comune_di_abitazione := fcase(regione %chin% c("Sicilia", "Sardegna", "Basilicata", "Molise", "Calabria", "Puglia", "Campania"), "1. Costo della vita basso",
                                                regione %chin% c("Marche", "Liguria", "Piemonte", "Abruzzo"), "2. Costo della vita medio - basso",
                                                regione %chin% c("Toscana", "Umbria", "Emilia-Romagna", "Friuli-Venezia Giulia", "Veneto"), "3. Costo della vita medio",
                                                regione %chin% c("Lazio", "Valle d'Aosta/Vallée d'Aoste", "Trentino-Alto Adige/Südtirol", "Lombardia"), "4. Costo della vita alto",
                                                default = NA)]

comuni_cap_selected[, cap := stringr::str_pad(as.character(cap), 5, "left", pad = "0")]

# writexl::write_xlsx(comuni_cap_selected[, .(cap, denominazione_in_italiano, denominazione_italiana_e_straniera, sede_abituale_di_lavoro)], file.path("output", "sedi_riclassificate.xlsx"))

# writexl::write_xlsx(comuni_cap_selected[, .(cap, denominazione_in_italiano, denominazione_italiana_e_straniera, codice_comune_formato_numerico)], file.path("output", "traduzione_comuni.xlsx"))

# writexl::write_xlsx(comuni_cap_selected[, .(regione, provincia, denominazione_in_italiano, denominazione_italiana_e_straniera, cap)], file.path("output", "comuni_provincia_regione.xlsx"))



# Unique CAP ----
nrow(comuni_cap_selected)
uniqueN(comuni_cap_selected$cap)

# Keep CAP con popolazione maggiore
comuni_cap_unique = copy(comuni_cap_selected)
comuni_cap_unique = comuni_cap_unique[, .SD[which.max(popolazione)], by = "cap"]

# writexl::write_xlsx(comuni_cap_unique[, .(cap, denominazione_in_italiano, area_geografica_abitazione, ampiezza_comune_di_abitazione, tipologia_comune_di_abitazione, sede_abituale_di_lavoro, costo_vita_comune_di_abitazione)], file.path("output", "dati_da_aggiungere.xlsx"))

# writexl::write_xlsx(comuni_cap_unique[, .(cap, denominazione_in_italiano, provincia, regione, popolazione, area_geografica_abitazione, ampiezza_comune_di_abitazione, tipologia_comune_di_abitazione, sede_abituale_di_lavoro, costo_vita_comune_di_abitazione)], file.path("output", "dati_da_aggiungere_new.xlsx"))
