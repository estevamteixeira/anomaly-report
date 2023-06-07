knitr::opts_chunk$set(
  echo = FALSE, 
  message = FALSE, 
  warning = FALSE,
  fig.pos = "h", 
  out.extra = "",
  out.width = '90%',
  fig.align = 'center',
  eval.after = "fig.cap",
  fig.caption= TRUE)

pckgs <- c('tidyr', 'knitr', 'scales','janitor',
           'kableExtra', 'data.table','dplyr',
           'forcats','lubridate', 'anytime',
           'htmltools', 'plotly','purrr', 'fontawesome',
           'cancensus','leaflet','leafsync','htmlwidgets',
           'htmltools','tmap','DT')
# for (pp in `pckgs`) { if (!require(pp)) install.packages(pp); library(pp, character.only = T)  }

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pckgs, character.only = TRUE)
## data input

# add data
# anomaly
cd_anom <- read.csv("data/cd_anomaly.csv", header = TRUE, stringsAsFactors = TRUE) |> 
  ## filtering only NS counties
  dplyr::filter(dplyr::between(CSDuid, 1201000, 1299999) &
                dplyr::between(BrthYear, 1987, 2021)) |>
  dplyr::filter(!substr(CSDuid, 5, 8) %in% "999") |> 
  dplyr::mutate(CSDuid = as.character(CSDuid),
                Birth_Date = as.Date(as.character(Birth_Date)),
                CDuid = substr(CSDuid, 1, 4),
                # Cluster_Number = stringr::str_pad(Cluster_Number, 6, pad = "0"),
                # NetworkID = substr(Cluster_Number, 1, 4),
                # ZoneID = substr(Cluster_Number, 1, 2),
                CSDuid = ifelse(CSDuid %in% c("1208001","1208002"),
                                "1208003",
                                CSDuid),
                CSDName = ifelse(CSDuid %in% c("1208003"),
                                 "West Hants",
                                 CSDName),
                CSDType = ifelse(CSDuid %in% c("1208003"),
                                 "Rural municipality",
                                 CSDType)) |>
  data.table::setDT()

# births
cd_birth <- read.csv("data/cd_birth.csv", header = TRUE, stringsAsFactors = TRUE) |> 
  ## filtering only NS counties
  dplyr::filter(dplyr::between(CSDuid, 1201000, 1299999) &
                  dplyr::between(BrthYear, 1987, 2021)) |>
  dplyr::filter(!substr(CSDuid, 5, 8) %in% "999") |>
  dplyr::mutate(CSDuid = as.character(CSDuid),
                CDuid = substr(CSDuid, 1, 4),
                # Cluster_Number = stringr::str_pad(Cluster_Number, 6, pad = "0"),
                # NetworkID = substr(Cluster_Number, 1, 4),
                # ZoneID = substr(Cluster_Number, 1, 2),
                CSDuid = ifelse(CSDuid %in% c("1208001","1208002"),
                                "1208003",
                                CSDuid),
                CSDName = ifelse(CSDuid %in% c("1208003"),
                                 "West Hants",
                                 CSDName),
                CSDType = ifelse(CSDuid %in% c("1208003"),
                                 "Rural municipality",
                                 CSDType)) |>
  data.table::setDT()

## Individual -------
anom_ind <- unique(cd_anom[,
                           .(CaseID, BIRTHID, MOTHERID,
                             CSDuid, CDuid, BrthYear, dlv, Diags)]) %>% 
  .[,`:=` (cat = fcase(
    grepl("^Q00", Diags), "Anencephaly",
    grepl("^Q01", Diags), "Encephalocele",
    grepl("^Q05", Diags), "Spina bifida",
    grepl("^Q02", Diags), "Microcephaly",
    grepl("^Q03", Diags), "Congenital hydrocephalus",
    grepl("^Q041|Q042", Diags), "Arhinencephaly/Holoprosencephaly",
    grepl("^Q110|Q111|Q112", Diags), "Anophtalmos/Microphtalmos",
    grepl("^Q160|Q172", Diags), "Anotia/Microtia",
    grepl("^Q300", Diags), "Choanal atresia",
    grepl("^Q200", Diags), "Commom truncus",
    grepl("^Q201|Q203|Q205", Diags), "Transposition of great vessels",
    grepl("^Q212", Diags), "Atrioventricular septal defect",
    grepl("^Q213", Diags), "Tetralogy of Fallot",
    grepl("^Q234", Diags), "Hypoplastic left heart syndrome",
    grepl("^Q251", Diags), "Coarctation of aorta",
    grepl("^Q35", Diags), "Cleft palate only",
    grepl("^Q36", Diags), "Cleft lip only",
    grepl("^Q37", Diags), "Cleft palate with cleft lip",
    grepl("^Q390|Q391|Q392|Q393|Q394", Diags), "Oesophageal atresia/stenosis, tracheoesophageal fistula",
    grepl("^Q41", Diags), "Small intestine absence/atresia/stenosis",
    grepl("^Q420|Q421|Q422|Q423", Diags), "Ano-rectal absence/atresia/stenosis",
    grepl("^Q431", Diags), "Hirschsprung disease",
    grepl("^Q442", Diags), "Atresia of bile ducts",
    grepl("^Q531|Q532|Q539", Diags), "Cryptorchidism/undescended testicles",
    grepl("Q54|Q540|Q541|Q542|Q543|
          Q545|Q546|Q547|Q548|Q549", Diags), "Hypospadias",
    grepl("^Q56", Diags), "Indeterminate sex",
    grepl("^Q640", Diags), "Epispadias",
    grepl("^Q600|Q601|Q602", Diags), "Renal agenesis",
    grepl("^Q611|Q612|Q613|Q614|
          Q615|Q618|Q619", Diags), "Cystic kidney",
    grepl("^Q641", Diags), "Bladder and cloacal exstrophy",
    grepl("^Q642|Q643", Diags), "Lower urinary tract obstruction",
    grepl("^Q65", Diags), "Hip dysplasia",
    grepl("^Q71|Q72|Q73", Diags), "Limb deficiency defects",
    grepl("^Q790", Diags), "Diaphragmatic hernia",
    grepl("^Q792", Diags), "Omphalocele/Exomphalos",
    grepl("^Q793", Diags), "Gastroschisis",
    grepl("^Q90", Diags), "Down Syndrome",
    grepl("^Q914|Q915|Q916|Q917", Diags), "Trisomy 13 - Patau",
    grepl("^Q910|Q911|Q912|Q913", Diags), "Trisomy 18 - Edwards",
    grepl("^Q96", Diags), "Turner syndrome",
    default = "Other"
  ))] %>%
  # remove q54.4, Q35.7
  .[!Diags %in% c("Q544","Q357")] %>%
  .[,`:=` (qcode = stringr::str_c(unique(Diags), collapse="|"),
           qcodecat = fcase(
             grepl("^anencephaly", tolower(cat)), "Q00",
             grepl("^encephalocele", tolower(cat)), "Q01",
             grepl("^spina bifida", tolower(cat)), "Q05",
             grepl("^microcephaly", tolower(cat)), "Q02",
             grepl("^congenital hydrocephalus", tolower(cat)), "Q03",
             grepl("^arhinencephaly", tolower(cat)), "Q04.1, Q04.2",
             grepl("^anophtalmos", tolower(cat)), "Q11.0-Q11.2",
             grepl("^anotia", tolower(cat)), "Q16.0, Q17.2",
             grepl("^choanal", tolower(cat)), "Q30.0",
             grepl("^commom", tolower(cat)), "Q20.0",
             grepl("^vessels", tolower(cat)), "Q20.1, Q20.3, Q20.5",
             grepl("^atrioventricular septal", tolower(cat)), "Q21.2",
             grepl("^tetralogy", tolower(cat)), "Q21.3",
             grepl("^hypoplastic", tolower(cat)), "Q23.4",
             grepl("^coarctation", tolower(cat)), "Q25.1",
             grepl("^cleft palate$", tolower(cat)), "Q35, excludes Q35.7",
             grepl("^cleft lip$", tolower(cat)), "Q36",
             grepl("^cleft palate with cleft lip$", tolower(cat)), "Q37",
             grepl("^oesophageal", tolower(cat)), "Q39.0-Q39.4",
             grepl("^small", tolower(cat)), "Q41",
             grepl("^ano-rectal", tolower(cat)), "Q42.0-Q42.3",
             grepl("^hirschsprung", tolower(cat)), "Q43.1",
             grepl("^atresia", tolower(cat)), "Q44.2",
             grepl("^cryptorchidism", tolower(cat)), "Q53.1, Q53.2, Q53.9",
             grepl("^hypospadias", tolower(cat)), "Q54, excludes Q54.4",
             grepl("^indeterminate", tolower(cat)), "Q56",
             grepl("^epispadias", tolower(cat)), "Q64.0",
             grepl("^renal", tolower(cat)), "Q60.0-Q60.2",
             grepl("^cystic", tolower(cat)), "Q61.1-Q61.5, Q61.8, Q61.9",
             grepl("^bladder", tolower(cat)), "Q64.1",
             grepl("^lower", tolower(cat)), "Q64.2, Q64.3",
             grepl("^hip dysplasia", tolower(cat)), "Q65",
             grepl("^limb", tolower(cat)), "Q71-Q73",
             grepl("^diaphragmatic", tolower(cat)), "Q79.0",
             grepl("^omphalocele", tolower(cat)), "Q79.2",
             grepl("^gastroschisis", tolower(cat)), "Q79.3",
             grepl("^down", tolower(cat)), "Q90",
             grepl("^trisomy 13", tolower(cat)), "Q91.4-Q91.7",
             grepl("^trisomy 18", tolower(cat)), "Q91.0-Q91.3",
             grepl("^turner", tolower(cat)), "Q96",
             default = "Other"
           )),
    by = .(cat)] %>% 
  .[,`:=` (count_anom = .N),
    by = .(BrthYear, cat)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat,count_anom)] %>% 
  unique() %>% 
  .[,`:=` (total_anom = sum(count_anom,
                            na.rm = TRUE)),
    by = .(BrthYear, cat)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat,total_anom)] %>% 
  unique() %>% 
  .[order(cat, BrthYear)] 

## Grouped --------
anom_grp <- unique(cd_anom[,
                           .(CaseID, BIRTHID, MOTHERID,
                             CSDuid, CDuid, BrthYear, dlv, Diags)]) %>% 
  .[,`:=` (cat = fcase(
    grepl("^Q00", Diags), "Neural tube defects",
    grepl("^Q01", Diags), "Neural tube defects",
    grepl("^Q05", Diags), "Neural tube defects",
    grepl("^Q02", Diags), "Selected central nervous system defects",
    grepl("^Q03", Diags), "Selected central nervous system defects",
    grepl("^Q041|Q042", Diags), "Selected central nervous system defects",
    grepl("^Q110|Q111|Q112", Diags), "Selected sense organ defects",
    grepl("^Q160|Q172", Diags), "Selected sense organ defects",
    grepl("^Q300", Diags), "Selected sense organ defects",
    grepl("^Q200", Diags), "Selected congenital heart defects",
    grepl("^Q201|Q203|Q205", Diags), "Selected congenital heart defects",
    grepl("^Q212", Diags), "Selected congenital heart defects",
    grepl("^Q213", Diags), "Selected congenital heart defects",
    grepl("^Q234", Diags), "Selected congenital heart defects",
    grepl("^Q251", Diags), "Selected congenital heart defects",
    grepl("^Q35", Diags), "Oro-facial clefts",
    grepl("^Q36", Diags), "Oro-facial clefts",
    grepl("^Q37", Diags), "Oro-facial clefts",
    grepl("^Q390|Q391|Q392|Q393|Q394", Diags), "Selected gastrointestinal defects",
    grepl("^Q41", Diags), "Selected gastrointestinal defects",
    grepl("^Q420|Q421|Q422|Q423", Diags), "Selected gastrointestinal defects",
    grepl("^Q431", Diags), "Selected gastrointestinal defects",
    grepl("^Q442", Diags), "Selected gastrointestinal defects",
    grepl("^Q531|Q532|Q539", Diags), "Selected genital anomalies",
    grepl("Q54|Q540|Q541|Q542|Q543|
          Q545|Q546|Q547|Q548|Q549", Diags), "Selected genital anomalies",
    grepl("^Q56", Diags), "Selected genital anomalies",
    grepl("^Q640", Diags), "Selected genital anomalies",
    grepl("^Q600|Q601|Q602", Diags), "Selected urinary tract defects",
    grepl("^Q611|Q612|Q613|Q614|
          Q615|Q618|Q619", Diags), "Selected urinary tract defects",
    grepl("^Q641", Diags), "Selected urinary tract defects",
    grepl("^Q642|Q643", Diags), "Selected urinary tract defects",
    grepl("^Q65", Diags), "Hip dysplasia",
    grepl("^Q71|Q72|Q73", Diags), "Limb deficiency defects",
    grepl("^Q790", Diags), "Selected abdominal wall defects",
    grepl("^Q792", Diags), "Selected abdominal wall defects",
    grepl("^Q793", Diags), "Selected abdominal wall defects",
    grepl("^Q90", Diags), "Selected chromosomal defects",
    grepl("^Q914|Q915|Q916|Q917", Diags), "Selected chromosomal defects",
    grepl("^Q910|Q911|Q912|Q913", Diags), "Selected chromosomal defects",
    grepl("^Q96", Diags), "Selected chromosomal defects",
    default = "Other"
  ))] %>%
  .[!Diags %in% c("Q544","Q357")] %>% 
  .[,`:=` (qcode = stringr::str_c(unique(Diags), collapse="|"),
           qcodecat = fcase(
             grepl("^neural tube", tolower(cat)), "Q00, Q01, Q05",
             grepl("^selected central nervous", tolower(cat)), "Q02, Q03, Q04.1, Q04.2",
             grepl("^selected sense", tolower(cat)), "Q11.0-Q11.2, Q16.0, Q17.2, Q30.0",
             grepl("^selected congenital heart", tolower(cat)), "Q20.0, Q20.1, Q20.3, Q20.5, Q21.2, Q21.3, Q23.4, Q25.1",
             grepl("^oro-facial", tolower(cat)), "Q35-Q37, excludes Q35.7",
             grepl("^selected gastrointestinal", tolower(cat)), "Q39.0-Q39.4, Q41, Q42.0-Q42.3, Q43.1, Q44.2",
             grepl("^selected genital", tolower(cat)), "Q53.1, Q53.2, Q53.9, Q54, excludes Q54.4, Q56, Q64.0",
             grepl("^selected urinary", tolower(cat)), "Q60.0-Q60.2, Q61.1-Q61.5, Q61.8, Q61.9, Q64.1-Q64.3",
             grepl("^hip", tolower(cat)), "Q65",
             grepl("^limb", tolower(cat)), "Q71-Q73",
             grepl("^congenital diaphragmatic", tolower(cat)), "Q79.0",
             grepl("^selected abdominal", tolower(cat)), "Q79.2, Q79.3",
             grepl("^selected chromosomal", tolower(cat)), "Q90, Q91.0-Q91.7, Q96",
             grepl("^other", tolower(cat)), "Other"
           )),
    by = .(cat)] %>%
  .[,.(CaseID, BrthYear, cat, qcode, qcodecat)] %>% 
  unique() %>%
  .[,`:=` (count_anom = .N),
    by = .(BrthYear, cat)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat, count_anom)] %>% 
  unique() %>% 
  .[,`:=` (total_anom = sum(count_anom,
                            na.rm = TRUE)),
    by = .(BrthYear, cat)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat, total_anom)] %>% 
  unique() %>% 
  .[order(cat, BrthYear)]

## Individual - Maternal age ---------

anom_ind_matage <- unique(cd_anom[,
                           .(CaseID, BIRTHID, MOTHERID,CSDuid, CDuid,
                             BrthYear, dlv, Diags, DMMATAGE)]) %>% 
  .[,`:=` (cat = fcase(
    grepl("^Q00", Diags), "Anencephaly",
    grepl("^Q01", Diags), "Encephalocele",
    grepl("^Q05", Diags), "Spina bifida",
    grepl("^Q02", Diags), "Microcephaly",
    grepl("^Q03", Diags), "Congenital hydrocephalus",
    grepl("^Q041|Q042", Diags), "Arhinencephaly/Holoprosencephaly",
    grepl("^Q110|Q111|Q112", Diags), "Anophtalmos/Microphtalmos",
    grepl("^Q160|Q172", Diags), "Anotia/Microtia",
    grepl("^Q300", Diags), "Choanal atresia",
    grepl("^Q200", Diags), "Commom truncus",
    grepl("^Q201|Q203|Q205", Diags), "Transposition of great vessels",
    grepl("^Q212", Diags), "Atrioventricular septal defect",
    grepl("^Q213", Diags), "Tetralogy of Fallot",
    grepl("^Q234", Diags), "Hypoplastic left heart syndrome",
    grepl("^Q251", Diags), "Coarctation of aorta",
    grepl("^Q35", Diags), "Cleft palate only",
    grepl("^Q36", Diags), "Cleft lip only",
    grepl("^Q37", Diags), "Cleft palate with cleft lip",
    grepl("^Q390|Q391|Q392|Q393|Q394", Diags), "Oesophageal atresia/stenosis, tracheoesophageal fistula",
    grepl("^Q41", Diags), "Small intestine absence/atresia/stenosis",
    grepl("^Q420|Q421|Q422|Q423", Diags), "Ano-rectal absence/atresia/stenosis",
    grepl("^Q431", Diags), "Hirschsprung disease",
    grepl("^Q442", Diags), "Atresia of bile ducts",
    grepl("^Q531|Q532|Q539", Diags), "Cryptorchidism/undescended testicles",
    grepl("Q54|Q540|Q541|Q542|Q543|
          Q545|Q546|Q547|Q548|Q549", Diags), "Hypospadias",
    grepl("^Q56", Diags), "Indeterminate sex",
    grepl("^Q640", Diags), "Epispadias",
    grepl("^Q600|Q601|Q602", Diags), "Renal agenesis",
    grepl("^Q611|Q612|Q613|Q614|
          Q615|Q618|Q619", Diags), "Cystic kidney",
    grepl("^Q641", Diags), "Bladder and cloacal exstrophy",
    grepl("^Q642|Q643", Diags), "Lower urinary tract obstruction",
    grepl("^Q65", Diags), "Hip dysplasia",
    grepl("^Q71|Q72|Q73", Diags), "Limb deficiency defects",
    grepl("^Q790", Diags), "Diaphragmatic hernia",
    grepl("^Q792", Diags), "Omphalocele/Exomphalos",
    grepl("^Q793", Diags), "Gastroschisis",
    grepl("^Q90", Diags), "Down Syndrome",
    grepl("^Q914|Q915|Q916|Q917", Diags), "Trisomy 13 - Patau",
    grepl("^Q910|Q911|Q912|Q913", Diags), "Trisomy 18 - Edwards",
    grepl("^Q96", Diags), "Turner syndrome",
    default = "Other"
  ))] %>%
  # remove q54.4, Q35.7
  .[!Diags %in% c("Q544","Q357")] %>%
  .[!is.na(DMMATAGE)] %>%
  .[,`:=` (qcode = stringr::str_c(unique(Diags), collapse="|"),
           qcodecat = fcase(
             grepl("^anencephaly", tolower(cat)), "Q00",
             grepl("^encephalocele", tolower(cat)), "Q01",
             grepl("^spina bifida", tolower(cat)), "Q05",
             grepl("^microcephaly", tolower(cat)), "Q02",
             grepl("^congenital hydrocephalus", tolower(cat)), "Q03",
             grepl("^arhinencephaly", tolower(cat)), "Q04.1, Q04.2",
             grepl("^anophtalmos", tolower(cat)), "Q11.0-Q11.2",
             grepl("^anotia", tolower(cat)), "Q16.0, Q17.2",
             grepl("^choanal", tolower(cat)), "Q30.0",
             grepl("^commom", tolower(cat)), "Q20.0",
             grepl("^vessels", tolower(cat)), "Q20.1, Q20.3, Q20.5",
             grepl("^atrioventricular septal", tolower(cat)), "Q21.2",
             grepl("^tetralogy", tolower(cat)), "Q21.3",
             grepl("^hypoplastic", tolower(cat)), "Q23.4",
             grepl("^coarctation", tolower(cat)), "Q25.1",
             grepl("^cleft palate$", tolower(cat)), "Q35, excludes Q35.7",
             grepl("^cleft lip$", tolower(cat)), "Q36",
             grepl("^cleft palate with cleft lip$", tolower(cat)), "Q37",
             grepl("^oesophageal", tolower(cat)), "Q39.0-Q39.4",
             grepl("^small", tolower(cat)), "Q41",
             grepl("^ano-rectal", tolower(cat)), "Q42.0-Q42.3",
             grepl("^hirschsprung", tolower(cat)), "Q43.1",
             grepl("^atresia", tolower(cat)), "Q44.2",
             grepl("^cryptorchidism", tolower(cat)), "Q53.1, Q53.2, Q53.9",
             grepl("^hypospadias", tolower(cat)), "Q54, excludes Q54.4",
             grepl("^indeterminate", tolower(cat)), "Q56",
             grepl("^epispadias", tolower(cat)), "Q64.0",
             grepl("^renal", tolower(cat)), "Q60.0-Q60.2",
             grepl("^cystic", tolower(cat)), "Q61.1-Q61.5, Q61.8, Q61.9",
             grepl("^bladder", tolower(cat)), "Q64.1",
             grepl("^lower", tolower(cat)), "Q64.2, Q64.3",
             grepl("^hip dysplasia", tolower(cat)), "Q65",
             grepl("^limb", tolower(cat)), "Q71-Q73",
             grepl("^diaphragmatic", tolower(cat)), "Q79.0",
             grepl("^omphalocele", tolower(cat)), "Q79.2",
             grepl("^gastroschisis", tolower(cat)), "Q79.3",
             grepl("^down", tolower(cat)), "Q90",
             grepl("^trisomy 13", tolower(cat)), "Q91.4-Q91.7",
             grepl("^trisomy 18", tolower(cat)), "Q91.0-Q91.3",
             grepl("^turner", tolower(cat)), "Q96",
             default = "Other"
           ),
           matage_format = factor(fcase(
             DMMATAGE < 25, "< 25",
             # data.table::between(DMMATAGE, 20, 24.999), "20-24",
             data.table::between(DMMATAGE, 25, 29.999), "25-34",
             # data.table::between(DMMATAGE, 30, 34.999), "30-34",
             DMMATAGE >= 35, "\u2265 35",
             # data.table::between(DMMATAGE, 40, 44.999), "40-44",
             # DMMATAGE >= 45, "\u2265 45",
             default = "-1"
           ), levels = c("< 25", #"20-24", "25-29",
                         "25-34",# "35-39", "40-44",
                         "\u2265 35", "-1"))),
    by = .(cat)] %>% 
  .[,`:=` (count_anom = .N),
    by = .(BrthYear, cat, matage_format)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat,
       matage_format, DMMATAGE, count_anom)] %>% 
  unique() %>% 
  .[,`:=` (total_anom = sum(count_anom,
                            na.rm = TRUE)),
    by = .(BrthYear, cat, matage_format)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat,
       matage_format, total_anom)] %>% 
  unique() %>% 
  .[order(cat, BrthYear)] 

# Grouped - Maternal age --------
anom_grp_matage <- unique(cd_anom[,
                           .(CaseID, BIRTHID, MOTHERID,CSDuid, CDuid,
                             BrthYear, dlv, Diags, DMMATAGE)]) %>% 
  .[,`:=` (cat = fcase(
    grepl("^Q00", Diags), "Neural tube defects",
    grepl("^Q01", Diags), "Neural tube defects",
    grepl("^Q05", Diags), "Neural tube defects",
    grepl("^Q02", Diags), "Selected central nervous system defects",
    grepl("^Q03", Diags), "Selected central nervous system defects",
    grepl("^Q041|Q042", Diags), "Selected central nervous system defects",
    grepl("^Q110|Q111|Q112", Diags), "Selected sense organ defects",
    grepl("^Q160|Q172", Diags), "Selected sense organ defects",
    grepl("^Q300", Diags), "Selected sense organ defects",
    grepl("^Q200", Diags), "Selected congenital heart defects",
    grepl("^Q201|Q203|Q205", Diags), "Selected congenital heart defects",
    grepl("^Q212", Diags), "Selected congenital heart defects",
    grepl("^Q213", Diags), "Selected congenital heart defects",
    grepl("^Q234", Diags), "Selected congenital heart defects",
    grepl("^Q251", Diags), "Selected congenital heart defects",
    grepl("^Q35", Diags), "Oro-facial clefts",
    grepl("^Q36", Diags), "Oro-facial clefts",
    grepl("^Q37", Diags), "Oro-facial clefts",
    grepl("^Q390|Q391|Q392|Q393|Q394", Diags), "Selected gastrointestinal defects",
    grepl("^Q41", Diags), "Selected gastrointestinal defects",
    grepl("^Q420|Q421|Q422|Q423", Diags), "Selected gastrointestinal defects",
    grepl("^Q431", Diags), "Selected gastrointestinal defects",
    grepl("^Q442", Diags), "Selected gastrointestinal defects",
    grepl("^Q531|Q532|Q539", Diags), "Selected genital anomalies",
    grepl("Q54|Q540|Q541|Q542|Q543|
          Q545|Q546|Q547|Q548|Q549", Diags), "Selected genital anomalies",
    grepl("^Q56", Diags), "Selected genital anomalies",
    grepl("^Q640", Diags), "Selected genital anomalies",
    grepl("^Q600|Q601|Q602", Diags), "Selected urinary tract defects",
    grepl("^Q611|Q612|Q613|Q614|
          Q615|Q618|Q619", Diags), "Selected urinary tract defects",
    grepl("^Q641", Diags), "Selected urinary tract defects",
    grepl("^Q642|Q643", Diags), "Selected urinary tract defects",
    grepl("^Q65", Diags), "Hip dysplasia",
    grepl("^Q71|Q72|Q73", Diags), "Limb deficiency defects",
    grepl("^Q790", Diags), "Selected abdominal wall defects",
    grepl("^Q792", Diags), "Selected abdominal wall defects",
    grepl("^Q793", Diags), "Selected abdominal wall defects",
    grepl("^Q90", Diags), "Selected chromosomal defects",
    grepl("^Q914|Q915|Q916|Q917", Diags), "Selected chromosomal defects",
    grepl("^Q910|Q911|Q912|Q913", Diags), "Selected chromosomal defects",
    grepl("^Q96", Diags), "Selected chromosomal defects",
    default = "Other"
  ))] %>%
  .[!Diags %in% c("Q544","Q357")] %>% 
  .[,`:=` (qcode = stringr::str_c(unique(Diags), collapse="|"),
           qcodecat = fcase(
             grepl("^neural tube", tolower(cat)), "Q00, Q01, Q05",
             grepl("^selected central nervous", tolower(cat)), "Q02, Q03, Q04.1, Q04.2",
             grepl("^selected sense", tolower(cat)), "Q11.0-Q11.2, Q16.0, Q17.2, Q30.0",
             grepl("^selected congenital heart", tolower(cat)), "Q20.0, Q20.1, Q20.3, Q20.5, Q21.2, Q21.3, Q23.4, Q25.1",
             grepl("^oro-facial", tolower(cat)), "Q35-Q37, excludes Q35.7",
             grepl("^selected gastrointestinal", tolower(cat)), "Q39.0-Q39.4, Q41, Q42.0-Q42.3, Q43.1, Q44.2",
             grepl("^selected genital", tolower(cat)), "Q53.1, Q53.2, Q53.9, Q54, excludes Q54.4, Q56, Q64.0",
             grepl("^selected urinary", tolower(cat)), "Q60.0-Q60.2, Q61.1-Q61.5, Q61.8, Q61.9, Q64.1-Q64.3",
             grepl("^hip", tolower(cat)), "Q65",
             grepl("^limb", tolower(cat)), "Q71-Q73",
             grepl("^congenital diaphragmatic", tolower(cat)), "Q79.0",
             grepl("^selected abdominal", tolower(cat)), "Q79.2, Q79.3",
             grepl("^selected chromosomal", tolower(cat)), "Q90, Q91.0-Q91.7, Q96",
             grepl("^other", tolower(cat)), "Other"
           ),
           matage_format = factor(fcase(
             DMMATAGE < 25, "< 25",
             # data.table::between(DMMATAGE, 20, 24.999), "20-24",
             data.table::between(DMMATAGE, 25, 29.999), "25-34",
             # data.table::between(DMMATAGE, 30, 34.999), "30-34",
             DMMATAGE >= 35, "\u2265 35",
             # data.table::between(DMMATAGE, 40, 44.999), "40-44",
             # DMMATAGE >= 45, "\u2265 45",
             default = "-1"
           ), levels = c("< 25", #"20-24", "25-29",
                         "25-34",# "35-39", "40-44",
                         "\u2265 35", "-1"))),
    by = .(cat)] %>%
  .[,.(CaseID, BrthYear, cat, qcode, qcodecat, matage_format)] %>% 
  unique() %>%
  .[,`:=` (count_anom = .N),
    by = .(BrthYear, cat, matage_format)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat,
       matage_format, count_anom)] %>% 
  unique() %>% 
  .[,`:=` (total_anom = sum(count_anom,
                            na.rm = TRUE)),
    by = .(BrthYear, cat, matage_format)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat,
       matage_format, total_anom)] %>% 
  unique() %>% 
  .[order(cat, BrthYear)]

## Anomaly grouped for mapping ------

anom_grp_map <- unique(cd_anom[,
                           .(CaseID, BIRTHID, MOTHERID, CSDuid, CDuid,
                             BrthYear, dlv, Diags)]) %>% 
  .[,`:=` (cat = factor(fcase(
    grepl("^Q00", Diags), "Neural tube defects",
    grepl("^Q01", Diags), "Neural tube defects",
    grepl("^Q05", Diags), "Neural tube defects",
    grepl("^Q02", Diags), "Selected central nervous system defects",
    grepl("^Q03", Diags), "Selected central nervous system defects",
    grepl("^Q041|Q042", Diags), "Selected central nervous system defects",
    grepl("^Q110|Q111|Q112", Diags), "Selected sense organ defects",
    grepl("^Q160|Q172", Diags), "Selected sense organ defects",
    grepl("^Q300", Diags), "Selected sense organ defects",
    grepl("^Q200", Diags), "Selected congenital heart defects",
    grepl("^Q201|Q203|Q205", Diags), "Selected congenital heart defects",
    grepl("^Q212", Diags), "Selected congenital heart defects",
    grepl("^Q213", Diags), "Selected congenital heart defects",
    grepl("^Q234", Diags), "Selected congenital heart defects",
    grepl("^Q251", Diags), "Selected congenital heart defects",
    grepl("^Q35", Diags), "Oro-facial clefts",
    grepl("^Q36", Diags), "Oro-facial clefts",
    grepl("^Q37", Diags), "Oro-facial clefts",
    grepl("^Q390|Q391|Q392|Q393|Q394", Diags), "Selected gastrointestinal defects",
    grepl("^Q41", Diags), "Selected gastrointestinal defects",
    grepl("^Q420|Q421|Q422|Q423", Diags), "Selected gastrointestinal defects",
    grepl("^Q431", Diags), "Selected gastrointestinal defects",
    grepl("^Q442", Diags), "Selected gastrointestinal defects",
    grepl("^Q531|Q532|Q539", Diags), "Selected genital anomalies",
    grepl("Q54|Q540|Q541|Q542|Q543|
          Q545|Q546|Q547|Q548|Q549", Diags), "Selected genital anomalies",
    grepl("^Q56", Diags), "Selected genital anomalies",
    grepl("^Q640", Diags), "Selected genital anomalies",
    grepl("^Q600|Q601|Q602", Diags), "Selected urinary tract defects",
    grepl("^Q611|Q612|Q613|Q614|
          Q615|Q618|Q619", Diags), "Selected urinary tract defects",
    grepl("^Q641", Diags), "Selected urinary tract defects",
    grepl("^Q642|Q643", Diags), "Selected urinary tract defects",
    grepl("^Q65", Diags), "Hip dysplasia",
    grepl("^Q71|Q72|Q73", Diags), "Limb deficiency defects",
    grepl("^Q790", Diags), "Selected abdominal wall defects",
    grepl("^Q792", Diags), "Selected abdominal wall defects",
    grepl("^Q793", Diags), "Selected abdominal wall defects",
    grepl("^Q90", Diags), "Selected chromosomal defects",
    grepl("^Q914|Q915|Q916|Q917", Diags), "Selected chromosomal defects",
    grepl("^Q910|Q911|Q912|Q913", Diags), "Selected chromosomal defects",
    grepl("^Q96", Diags), "Selected chromosomal defects",
    default = "Other"
  )))] %>%
  .[!Diags %in% c("Q544","Q357") &
      between(BrthYear, 2006, 2020)] %>% 
  .[,`:=` (qcode = stringr::str_c(unique(Diags), collapse="|"),
           # time_cat = cut(BrthYear, breaks = round(length(unique(BrthYear)))/10,
           #                include.lowest = TRUE,
           #                # right = TRUE,
           #                dig.lab = 3),
           time_cat = factor(fcase(
             # data.table::between(BrthYear, 2001, 2005), "2001-2005",
             data.table::between(BrthYear, 2006, 2010), "2006-2010",
             data.table::between(BrthYear, 2011, 2015), "2011-2015",
             data.table::between(BrthYear, 2016, 2020), "2016-2020",
             default = NA
           ), levels = c(#"2001-2005",
             "2006-2010","2011-2015","2016-2020")),
           qcodecat = factor(fcase(
             grepl("^neural tube", tolower(cat)), "Q00, Q01, Q05",
             grepl("^selected central nervous", tolower(cat)), "Q02, Q03, Q04.1, Q04.2",
             grepl("^selected sense", tolower(cat)), "Q11.0-Q11.2, Q16.0, Q17.2, Q30.0",
             grepl("^selected congenital heart", tolower(cat)), "Q20.0, Q20.1, Q20.3, Q20.5, Q21.2, Q21.3, Q23.4, Q25.1",
             grepl("^oro-facial", tolower(cat)), "Q35-Q37, excludes Q35.7",
             grepl("^selected gastrointestinal", tolower(cat)), "Q39.0-Q39.4, Q41, Q42.0-Q42.3, Q43.1, Q44.2",
             grepl("^selected genital", tolower(cat)), "Q53.1, Q53.2, Q53.9, Q54, excludes Q54.4, Q56, Q64.0",
             grepl("^selected urinary", tolower(cat)), "Q60.0-Q60.2, Q61.1-Q61.5, Q61.8, Q61.9, Q64.1-Q64.3",
             grepl("^hip", tolower(cat)), "Q65",
             grepl("^limb", tolower(cat)), "Q71-Q73",
             grepl("^congenital diaphragmatic", tolower(cat)), "Q79.0",
             grepl("^selected abdominal", tolower(cat)), "Q79.2, Q79.3",
             grepl("^selected chromosomal", tolower(cat)), "Q90, Q91.0-Q91.7, Q96",
             grepl("^other", tolower(cat)), "Other"
           ))),
    by = .(cat)] %>%
  # .[,`:=` (
  #   # time_lab = fifelse(grepl("\\((.+),.*", time_cat),
  #   #                    paste0(as.numeric( sub("\\((.+),.*", "\\1", time_cat)) + 1,
  #   #                           "-",
  #   #                           as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", time_cat)) ),
  #   #                    paste0(as.numeric( sub("\\[(.+),.*", "\\1", time_cat)),
  #   #                           "-",
  #   #                           as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", time_cat))))
  #   time_lab = paste0(as.numeric(min(BrthYear, na.rm = TRUE)),
  #                     "-",
  #                     as.numeric(max(BrthYear, na.rm = TRUE)))
  # ),
  # by = .(CD_UID, cat, time_cat)] %>% 
  .[,.(CaseID, BrthYear, CSDuid, CDuid,
       cat, qcode, qcodecat,time_cat)] %>% 
  unique() %>%
  .[,`:=` (count_anom = .N),
    by = .(CDuid, cat, time_cat)] %>%
  .[,.(CDuid, cat, qcode, qcodecat, time_cat, count_anom)] %>% 
  unique()

anom_grp_map <- anom_grp_map %>% 
  # group_by(levels(cat)) %>%
  tidyr::complete(CDuid = as.character(1201:1218),
           nesting(cat, qcode, qcodecat, time_cat),
           fill = list(count_anom = NA)) %>%
  setDT() %>% 
  .[order(CDuid, time_cat, qcode)] %>% 
  unique()

## Births -------

birth <- unique(
  unique(cd_birth[tolower(dlv) %in% c("lvb","stillbirth"),
                  `:=` (count_dlv_yr = .N),
                  by = list(BrthYear, dlv)
                  ][,
                    c("BrthYear", "dlv", "count_dlv_yr")
                  ])[,
                     `:=` (total_lvb_yr = sum(count_dlv_yr,
                                                  na.rm = TRUE)),
                     by = c("BrthYear")
                  ][, c("BrthYear", "total_lvb_yr")
                    ]
                )[order(BrthYear)]

birth_map <- unique(
  unique(cd_birth[tolower(dlv) %in% c("lvb","stillbirth"),
                  `:=` (time_cat = factor(fcase(
                    # data.table::between(BrthYear, 2001, 2005), "2001-2005",
                    data.table::between(BrthYear, 2006, 2010), "2006-2010",
                    data.table::between(BrthYear, 2011, 2015), "2011-2015",
                    data.table::between(BrthYear, 2016, 2020), "2016-2020",
                    default = NA
                  ), levels = c(#"2001-2005",
                    "2006-2010","2011-2015","2016-2020")))][,
                `:=` (count_dlv_geo_yr = .N),
             by = list(time_cat, CDuid, dlv)
             ][,
                  c("CDuid", "CDName", "time_cat", "dlv", "count_dlv_geo_yr")]
         )[, `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr, na.rm = TRUE)),
                    by = c("CDuid","time_cat")][,
                  c("CDuid", "CDName", "time_cat", "total_lvb_geo_yr")
                  ][, `:=` (total_lvb_yr = sum(total_lvb_geo_yr, na.rm = TRUE)),
                    by = c("time_cat")
                    ][,
                      `:=` (total_lvb_geo = sum(total_lvb_geo_yr, na.rm = TRUE)),
                      by = c("CDuid")
                      ][, c("CDuid", "CDName", "time_cat", "total_lvb_geo_yr",
                            "total_lvb_yr","total_lvb_geo")
                        ]
  )[order(CDuid, time_cat)][!is.na(time_cat)]

birth_matage <- unique(
  unique(
    cd_birth[tolower(dlv) %in% c("lvb","stillbirth"),
             `:=` (matage_format = factor(fcase(
               DMMATAGE < 25, "< 25",
 # data.table::between(DMMATAGE, 20, 24.999), "20-24",
 data.table::between(DMMATAGE, 25, 29.999), "25-34",
 # data.table::between(DMMATAGE, 30, 34.999), "30-34",
 DMMATAGE >= 35, "\u2265 35",
 # data.table::between(DMMATAGE, 40, 44.999), "40-44",
 # DMMATAGE >= 45, "\u2265 45",
 default = "-1"
 ), levels = c("< 25", #"20-24", "25-29",
               "25-34",# "35-39", "40-44",
               "\u2265 35", "-1")))
 ][,
  `:=` (count_dlv_yr = .N),
  by = list(BrthYear, dlv, matage_format)
  ][,
    c("BrthYear", "dlv", "count_dlv_yr", "matage_format")
    ])[,
       `:=` (total_lvb_yr = sum(count_dlv_yr,na.rm = TRUE)),
       by = c("BrthYear","matage_format")
       ][, c("BrthYear", "matage_format", "total_lvb_yr")]
 )[
   order(BrthYear, matage_format)
   ][
     !is.na(matage_format) & !matage_format %in% "-1"
   ]

# bringing total birth to anomaly data
anom_ind <- merge(anom_ind,
                  birth,
                  by = ("BrthYear")) %>% 
  .[order(cat, BrthYear)] %>% 
  unique() %>% 
  .[,`:=` (rate = 1000*total_anom/total_lvb_yr)] %>% 
  .[order(BrthYear, rate)]

# bringing total birth to anomaly data
anom_grp <- merge(anom_grp,
                  birth,
                  by = ("BrthYear")) %>% 
  .[order(cat, BrthYear)] %>% 
  unique() %>% 
  .[,`:=` (rate = 1000*total_anom/total_lvb_yr)] %>% 
  .[order(BrthYear, rate)]


anom_ind_matage <- merge(anom_ind_matage,
                  birth_matage,
                  by = c("BrthYear", "matage_format")) %>% 
  .[order(cat, BrthYear)] %>% 
  unique() %>% 
  .[,`:=` (rate = 1000*total_anom/total_lvb_yr)] %>% 
  .[order(BrthYear, rate)]

# bringing total birth to anomaly data
anom_grp_matage <- merge(anom_grp_matage,
                  birth_matage,
                  by = c("BrthYear","matage_format")) %>% 
  .[order(cat, BrthYear)] %>% 
  unique() %>% 
  .[,`:=` (rate = 1000*total_anom/total_lvb_yr)] %>% 
  .[order(BrthYear, rate)]

anom_grp_map <- merge(anom_grp_map,
           birth_map,
           by = c("CDuid", "time_cat")) %>% 
  .[order(CDuid, cat, time_cat)] %>%
  unique() %>% 
  .[,`:=` (rate = 1000*count_anom/total_lvb_geo_yr)] %>% 
  .[order(CDuid, time_cat)] %>% 
  .[,.(CDuid, time_cat, cat, qcode, qcodecat, count_anom, total_lvb_yr, rate)] %>%
  unique()

# key to access cancensus datasets
key = "CensusMapper_f505397ff4bb63467541085d028c9be8"

cd_shp <- data.table::setDT(cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = "12"), 
  level = "CD",
  geo_format = "sf",
  api_key = key
))[,
   `:=` (
     name = stringr::str_remove(name, " \\(CTY\\)"),
     cd_type = "County",
     area = `Shape Area`)][,
     c("GeoUID", "name", "cd_type", "Dwellings 2016","Dwellings", 
     "Population 2016", "Population", "Households 2016",
     "Households", "area", "geometry")
     ]

cd_names <- data.frame(CDuid = cd_shp$GeoUID, CDName = cd_shp$name)


