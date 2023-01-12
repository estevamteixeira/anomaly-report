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
  dplyr::mutate(CD_UID = as.character(CD_UID),
                Birth_Date = as.Date(as.character(Birth_Date))) |>
  ## filtering only NS counties
  dplyr::filter(dplyr::between(CD_UID, 1201, 1299)) |> 
  data.table::setDT()

# births
cd_birth <- read.csv("data/cd_birth.csv", header = TRUE, stringsAsFactors = TRUE) |> 
  dplyr::mutate(CD_UID = as.character(CD_UID)) |> 
  data.table::setDT()

anom_ind <- unique(cd_anom[,
                           .(CaseID, CD_UID, BrthYear, Diags)]) %>% 
  .[,`:=` (cat = fcase(
    grepl("^Q00", Diags), "Anencephaly",
    grepl("^Q01", Diags), "Encephalocele",
    grepl("^Q05|Q760", Diags), "Spina bifida",
    grepl("^Q02", Diags), "Microcephaly",
    grepl("^Q03", Diags), "Congenital hydrocephalus",
    grepl("^Q041|Q042", Diags), "Arhinencephaly/Holoprosencephaly",
    grepl("^Q110|Q111|Q112", Diags), "Anophtalmos/Microphtalmos",
    grepl("^Q160|Q172", Diags), "Anotia/Microtia",
    grepl("^Q30", Diags), "Choanal atresia",
    grepl("^Q200", Diags), "Commom truncus",
    grepl("^Q2030|Q2031|Q2032|Q2038", Diags), "Discordant ventriculoarterial connection",
    grepl("^Q212", Diags), "Atrioventricular septal defect",
    grepl("^Q213", Diags), "Tetralogy of Fallot",
    grepl("^Q234", Diags), "Hypoplastic left heart syndrome",
    grepl("^Q251", Diags), "Coarctation of aorta",
    grepl("^Q35", Diags), "Cleft palate",
    grepl("^Q36", Diags), "Cleft lip",
    grepl("^Q37", Diags), "Cleft palate with cleft lip",
    grepl("^Q390|Q391|Q392|Q393|Q394", Diags), "Oesophageal atresia/stenosis, tracheoesophageal fistula",
    grepl("^Q41", Diags), "Small intestine absence/atresia/stenosis",
    grepl("^Q420|Q421|Q422|Q423", Diags), "Ano-rectal absence/atresia/stenosis",
    grepl("^Q431", Diags), "Hirschsprung disease",
    grepl("^Q442", Diags), "Atresia of bile ducts",
    grepl("^Q531|Q532|Q539", Diags), "Cryptorchidism/undescended testicles",
    grepl("Q54|Q540|Q541|Q542|Q543|
          Q545|Q546|Q547|Q548|Q549", Diags), "Hypospadias",
    grepl("^Q56", Diags), "Indeterminate sex and pseudohermaphroditism",
    grepl("^Q640", Diags), "Epispadias",
    grepl("^Q600|Q601|Q602", Diags), "Renal agenesis",
    grepl("^Q611|Q612|Q613|Q614|
          Q615|Q618|Q619", Diags), "Cystic kidney",
    grepl("^Q641", Diags), "Bladder and cloacal exstroph",
    grepl("^Q642|Q643", Diags), "Lower urinary tract obstruction",
    grepl("^Q65", Diags), "Hip dysplasia",
    grepl("^Q71|Q72|Q73", Diags), "Limb deficiency defects",
    grepl("^Q792", Diags), "Omphalocele/Exomphalos",
    grepl("^Q793", Diags), "Gastroschisis",
    grepl("^Q90", Diags), "Down Syndrome",
    grepl("^Q914|Q915|Q916|Q917", Diags), "Trisomy 13 - Patau",
    grepl("^Q910|Q911|Q912|Q913", Diags), "Trisomy 18 - Edwards",
    grepl("^Q96", Diags), "Turner syndrome",
    default = "Other"
  ))] %>%
  # remove q54 not hypospadias
  .[!Diags %in% c("Q544")] %>%
  .[,`:=` (qcode = stringr::str_c(unique(Diags), collapse="|"),
           qcodecat = fcase(
             grepl("^anencephaly", tolower(cat)), "Q00",
             grepl("^encephalocele", tolower(cat)), "Q01",
             grepl("^spina bifida", tolower(cat)), "Q05-Q76.0",
             grepl("^microcephaly", tolower(cat)), "Q02",
             grepl("^congenital hydrocephalus", tolower(cat)), "Q03",
             grepl("^arhinencephaly", tolower(cat)), "Q04.1, Q04.2",
             grepl("^anophtalmos", tolower(cat)), "Q11.0-Q11.2",
             grepl("^anotia", tolower(cat)), "Q16.0, Q17.2",
             grepl("^choanal", tolower(cat)), "Q30",
             grepl("^commom", tolower(cat)), "Q20.0",
             grepl("^discordant", tolower(cat)), "Q20.30-Q20.32, Q20.38",
             grepl("^atrioventricular septal", tolower(cat)), "Q21.2",
             grepl("^tetralogy", tolower(cat)), "Q21.3",
             grepl("^hypoplastic", tolower(cat)), "Q23.4",
             grepl("^coarctation", tolower(cat)), "Q25.1",
             grepl("^cleft palate$", tolower(cat)), "Q35",
             grepl("^cleft lip$", tolower(cat)), "Q36",
             grepl("^cleft palate with cleft lip$", tolower(cat)), "Q37",
             grepl("^oesophageal", tolower(cat)), "Q39.0-Q39.4",
             grepl("^small", tolower(cat)), "Q41",
             grepl("^ano-rectal", tolower(cat)), "Q42.0-Q42.3",
             grepl("^hirschsprung", tolower(cat)), "Q43.1",
             grepl("^atresia", tolower(cat)), "Q44.2",
             grepl("^cryptorchidism", tolower(cat)), "Q53.1, Q53.2, Q53.9",
             grepl("^hypospadias", tolower(cat)), "Q54:excludes Q54.4",
             grepl("^indeterminate", tolower(cat)), "Q56",
             grepl("^epispadias", tolower(cat)), "Q64.0",
             grepl("^renal", tolower(cat)), "Q60.0-Q60.2",
             grepl("^cystic", tolower(cat)), "Q61.1-Q61.5, Q61.8, Q61.9",
             grepl("^bladder", tolower(cat)), "Q64.1",
             grepl("^lower", tolower(cat)), "Q64.2, Q64.3",
             grepl("^hip dysplasia", tolower(cat)), "Q65",
             grepl("^limb", tolower(cat)), "Q71-Q73",
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

anom_grp <- unique(cd_anom[,
                           .(CaseID, CD_UID, BrthYear, Diags)]) %>% 
  .[,`:=` (cat = fcase(
    grepl("^Q00", Diags), "Neural tube defects",
    grepl("^Q01", Diags), "Neural tube defects",
    grepl("^Q05|Q760", Diags), "Neural tube defects",
    grepl("^Q02", Diags), "Selected central nervous system defects",
    grepl("^Q03", Diags), "Selected central nervous system defects",
    grepl("^Q041|Q042", Diags), "Selected central nervous system defects",
    grepl("^Q110|Q111|Q112", Diags), "Selected sense organ defects",
    grepl("^Q160|Q172", Diags), "Selected sense organ defects",
    grepl("^Q30", Diags), "Selected sense organ defects",
    grepl("^Q20", Diags), "Selected congenital heart defects",
    grepl("^Q2030|Q2031|Q2032|Q2038", Diags), "Selected congenital heart defects",
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
    grepl("^Q792", Diags), "Selected abdominal wall defects",
    grepl("^Q793", Diags), "Selected abdominal wall defects",
    grepl("^Q90", Diags), "Selected chromosomal defects",
    grepl("^Q914|Q915|Q916|Q917", Diags), "Selected chromosomal defects",
    grepl("^Q910|Q911|Q912|Q913", Diags), "Selected chromosomal defects",
    grepl("^Q96", Diags), "Selected chromosomal defects",
    default = "Other"
  ))] %>%
  .[!Diags %in% c("Q544")] %>% 
  .[,`:=` (qcode = stringr::str_c(unique(Diags), collapse="|"),
           qcodecat = fcase(
             grepl("^neural tube", tolower(cat)), "Q00, Q01, Q05, Q76.0",
             grepl("^selected central nervous", tolower(cat)), "Q02, Q03, Q04.1, Q04.2",
             grepl("^selected sense", tolower(cat)), "Q11.0-Q11.2, Q16.0, Q17.2, Q30",
             grepl("^selected congenital heart", tolower(cat)), "Q20, Q21.2, Q21.3, Q23.4, Q25.1",
             grepl("^oro-facial", tolower(cat)), "Q35-Q37",
             grepl("^selected gastrointestinal", tolower(cat)), "Q39.0-Q39.4, Q41, Q42.0-Q42.3, Q43.1, Q44.2",
             grepl("^selected genital", tolower(cat)), "Q53.1, Q53.2, Q53.9,
             Q54:excludes Q54.4, Q56, Q64.0",
             grepl("^selected urinary", tolower(cat)), "Q60.0-Q60.2, Q61.1-Q61.5, Q61.8, Q61.9, Q64.1-Q64.3",
             grepl("^hip", tolower(cat)), "Q65",
             grepl("^limb", tolower(cat)), "Q71-Q73",
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

## Maternal age

anom_ind_matage <- unique(cd_anom[,
                           .(CaseID, CD_UID, BrthYear, Diags, matage)]) %>% 
  .[,`:=` (cat = fcase(
    grepl("^Q00", Diags), "Anencephaly",
    grepl("^Q01", Diags), "Encephalocele",
    grepl("^Q05|Q760", Diags), "Spina bifida",
    grepl("^Q02", Diags), "Microcephaly",
    grepl("^Q03", Diags), "Congenital hydrocephalus",
    grepl("^Q041|Q042", Diags), "Arhinencephaly/Holoprosencephaly",
    grepl("^Q110|Q111|Q112", Diags), "Anophtalmos/Microphtalmos",
    grepl("^Q160|Q172", Diags), "Anotia/Microtia",
    grepl("^Q30", Diags), "Choanal atresia",
    grepl("^Q200", Diags), "Commom truncus",
    grepl("^Q2030|Q2031|Q2032|Q2038", Diags), "Discordant ventriculoarterial connection",
    grepl("^Q212", Diags), "Atrioventricular septal defect",
    grepl("^Q213", Diags), "Tetralogy of Fallot",
    grepl("^Q234", Diags), "Hypoplastic left heart syndrome",
    grepl("^Q251", Diags), "Coarctation of aorta",
    grepl("^Q35", Diags), "Cleft palate",
    grepl("^Q36", Diags), "Cleft lip",
    grepl("^Q37", Diags), "Cleft palate with cleft lip",
    grepl("^Q390|Q391|Q392|Q393|Q394", Diags), "Oesophageal atresia/stenosis, tracheoesophageal fistula",
    grepl("^Q41", Diags), "Small intestine absence/atresia/stenosis",
    grepl("^Q420|Q421|Q422|Q423", Diags), "Ano-rectal absence/atresia/stenosis",
    grepl("^Q431", Diags), "Hirschsprung disease",
    grepl("^Q442", Diags), "Atresia of bile ducts",
    grepl("^Q531|Q532|Q539", Diags), "Cryptorchidism/undescended testicles",
    grepl("Q54|Q540|Q541|Q542|Q543|
          Q545|Q546|Q547|Q548|Q549", Diags), "Hypospadias",
    grepl("^Q56", Diags), "Indeterminate sex and pseudohermaphroditism",
    grepl("^Q640", Diags), "Epispadias",
    grepl("^Q600|Q601|Q602", Diags), "Renal agenesis",
    grepl("^Q611|Q612|Q613|Q614|
          Q615|Q618|Q619", Diags), "Cystic kidney",
    grepl("^Q641", Diags), "Bladder and cloacal exstroph",
    grepl("^Q642|Q643", Diags), "Lower urinary tract obstruction",
    grepl("^Q65", Diags), "Hip dysplasia",
    grepl("^Q71|Q72|Q73", Diags), "Limb deficiency defects",
    grepl("^Q792", Diags), "Omphalocele/Exomphalos",
    grepl("^Q793", Diags), "Gastroschisis",
    grepl("^Q90", Diags), "Down Syndrome",
    grepl("^Q914|Q915|Q916|Q917", Diags), "Trisomy 13 - Patau",
    grepl("^Q910|Q911|Q912|Q913", Diags), "Trisomy 18 - Edwards",
    grepl("^Q96", Diags), "Turner syndrome",
    default = "Other"
  ))] %>%
  # remove q54 not hypospadias
  .[!Diags %in% c("Q544")] %>%
  .[,`:=` (qcode = stringr::str_c(unique(Diags), collapse="|"),
           qcodecat = fcase(
             grepl("^anencephaly", tolower(cat)), "Q00",
             grepl("^encephalocele", tolower(cat)), "Q01",
             grepl("^spina bifida", tolower(cat)), "Q05-Q76.0",
             grepl("^microcephaly", tolower(cat)), "Q02",
             grepl("^congenital hydrocephalus", tolower(cat)), "Q03",
             grepl("^arhinencephaly", tolower(cat)), "Q04.1, Q04.2",
             grepl("^anophtalmos", tolower(cat)), "Q11.0-Q11.2",
             grepl("^anotia", tolower(cat)), "Q16.0, Q17.2",
             grepl("^choanal", tolower(cat)), "Q30",
             grepl("^commom", tolower(cat)), "Q20.0",
             grepl("^discordant", tolower(cat)), "Q20.30-Q20.32, Q20.38",
             grepl("^atrioventricular septal", tolower(cat)), "Q21.2",
             grepl("^tetralogy", tolower(cat)), "Q21.3",
             grepl("^hypoplastic", tolower(cat)), "Q23.4",
             grepl("^coarctation", tolower(cat)), "Q25.1",
             grepl("^cleft palate$", tolower(cat)), "Q35",
             grepl("^cleft lip$", tolower(cat)), "Q36",
             grepl("^cleft palate with cleft lip$", tolower(cat)), "Q37",
             grepl("^oesophageal", tolower(cat)), "Q39.0-Q39.4",
             grepl("^small", tolower(cat)), "Q41",
             grepl("^ano-rectal", tolower(cat)), "Q42.0-Q42.3",
             grepl("^hirschsprung", tolower(cat)), "Q43.1",
             grepl("^atresia", tolower(cat)), "Q44.2",
             grepl("^cryptorchidism", tolower(cat)), "Q53.1, Q53.2, Q53.9",
             grepl("^hypospadias", tolower(cat)), "Q54:excludes Q54.4",
             grepl("^indeterminate", tolower(cat)), "Q56",
             grepl("^epispadias", tolower(cat)), "Q64.0",
             grepl("^renal", tolower(cat)), "Q60.0-Q60.2",
             grepl("^cystic", tolower(cat)), "Q61.1-Q61.5, Q61.8, Q61.9",
             grepl("^bladder", tolower(cat)), "Q64.1",
             grepl("^lower", tolower(cat)), "Q64.2, Q64.3",
             grepl("^hip dysplasia", tolower(cat)), "Q65",
             grepl("^limb", tolower(cat)), "Q71-Q73",
             grepl("^omphalocele", tolower(cat)), "Q79.2",
             grepl("^gastroschisis", tolower(cat)), "Q79.3",
             grepl("^down", tolower(cat)), "Q90",
             grepl("^trisomy 13", tolower(cat)), "Q91.4-Q91.7",
             grepl("^trisomy 18", tolower(cat)), "Q91.0-Q91.3",
             grepl("^turner", tolower(cat)), "Q96",
             default = "Other"
           ),
           matage_format = fcase(
             matage %in% "1", "age < 20",
             matage %in% "2", "20 \u2264 age < 25",
             matage %in% "3", "25 \u2264 age < 30",
             matage %in% "4", "30 \u2264 age < 35",
             matage %in% "5", "35 \u2264 age < 40",
             matage %in% "6", "age \u2265 40"
           )),
    by = .(cat)] %>% 
  .[,`:=` (count_anom = .N),
    by = .(BrthYear, cat, matage_format)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat,
       matage, matage_format, count_anom)] %>% 
  unique() %>% 
  .[,`:=` (total_anom = sum(count_anom,
                            na.rm = TRUE)),
    by = .(BrthYear, cat, matage_format)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat,
       matage, matage_format, total_anom)] %>% 
  unique() %>% 
  .[order(cat, BrthYear)] 

anom_grp_matage <- unique(cd_anom[,
                           .(CaseID, CD_UID, BrthYear, Diags, matage)]) %>% 
  .[,`:=` (cat = fcase(
    grepl("^Q00", Diags), "Neural tube defects",
    grepl("^Q01", Diags), "Neural tube defects",
    grepl("^Q05|Q760", Diags), "Neural tube defects",
    grepl("^Q02", Diags), "Selected central nervous system defects",
    grepl("^Q03", Diags), "Selected central nervous system defects",
    grepl("^Q041|Q042", Diags), "Selected central nervous system defects",
    grepl("^Q110|Q111|Q112", Diags), "Selected sense organ defects",
    grepl("^Q160|Q172", Diags), "Selected sense organ defects",
    grepl("^Q30", Diags), "Selected sense organ defects",
    grepl("^Q20", Diags), "Selected congenital heart defects",
    grepl("^Q2030|Q2031|Q2032|Q2038", Diags), "Selected congenital heart defects",
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
    grepl("^Q792", Diags), "Selected abdominal wall defects",
    grepl("^Q793", Diags), "Selected abdominal wall defects",
    grepl("^Q90", Diags), "Selected chromosomal defects",
    grepl("^Q914|Q915|Q916|Q917", Diags), "Selected chromosomal defects",
    grepl("^Q910|Q911|Q912|Q913", Diags), "Selected chromosomal defects",
    grepl("^Q96", Diags), "Selected chromosomal defects",
    default = "Other"
  ))] %>%
  .[!Diags %in% c("Q544")] %>% 
  .[,`:=` (qcode = stringr::str_c(unique(Diags), collapse="|"),
           qcodecat = fcase(
             grepl("^neural tube", tolower(cat)), "Q00, Q01, Q05, Q76.0",
             grepl("^selected central nervous", tolower(cat)), "Q02, Q03, Q04.1, Q04.2",
             grepl("^selected sense", tolower(cat)), "Q11.0-Q11.2, Q16.0, Q17.2, Q30",
             grepl("^selected congenital heart", tolower(cat)), "Q20, Q21.2, Q21.3, Q23.4, Q25.1",
             grepl("^oro-facial", tolower(cat)), "Q35-Q37",
             grepl("^selected gastrointestinal", tolower(cat)), "Q39.0-Q39.4, Q41, Q42.0-Q42.3, Q43.1, Q44.2",
             grepl("^selected genital", tolower(cat)), "Q53.1, Q53.2, Q53.9,
             Q54:excludes Q54.4, Q56, Q64.0",
             grepl("^selected urinary", tolower(cat)), "Q60.0-Q60.2, Q61.1-Q61.5, Q61.8, Q61.9, Q64.1-Q64.3",
             grepl("^hip", tolower(cat)), "Q65",
             grepl("^limb", tolower(cat)), "Q71-Q73",
             grepl("^selected abdominal", tolower(cat)), "Q79.2, Q79.3",
             grepl("^selected chromosomal", tolower(cat)), "Q90, Q91.0-Q91.7, Q96",
             grepl("^other", tolower(cat)), "Other"
           ),
           matage_format = fcase(
             matage %in% "1", "age < 20",
             matage %in% "2", "20 \u2264 age < 25",
             matage %in% "3", "25 \u2264 age < 30",
             matage %in% "4", "30 \u2264 age < 35",
             matage %in% "5", "35 \u2264 age < 40",
             matage %in% "6", "age \u2265 40"
           )),
    by = .(cat)] %>%
  .[,.(CaseID, BrthYear, cat, qcode, qcodecat, matage, matage_format)] %>% 
  unique() %>%
  .[,`:=` (count_anom = .N),
    by = .(BrthYear, cat, matage_format)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat,
       matage, matage_format, count_anom)] %>% 
  unique() %>% 
  .[,`:=` (total_anom = sum(count_anom,
                            na.rm = TRUE)),
    by = .(BrthYear, cat, matage_format)] %>%
  .[,.(BrthYear, cat, qcode, qcodecat,
       matage, matage_format, total_anom)] %>% 
  unique() %>% 
  .[order(cat, BrthYear)]

## Anomaly grouped for mapping

anom_grp_map <- unique(cd_anom[,
                           .(CaseID, CD_UID, BrthYear, Diags)]) %>% 
  .[,`:=` (cat = fcase(
    grepl("^Q00", Diags), "Neural tube defects",
    grepl("^Q01", Diags), "Neural tube defects",
    grepl("^Q05|Q760", Diags), "Neural tube defects",
    grepl("^Q02", Diags), "Selected central nervous system defects",
    grepl("^Q03", Diags), "Selected central nervous system defects",
    grepl("^Q041|Q042", Diags), "Selected central nervous system defects",
    grepl("^Q110|Q111|Q112", Diags), "Selected sense organ defects",
    grepl("^Q160|Q172", Diags), "Selected sense organ defects",
    grepl("^Q30", Diags), "Selected sense organ defects",
    grepl("^Q20", Diags), "Selected congenital heart defects",
    grepl("^Q2030|Q2031|Q2032|Q2038", Diags), "Selected congenital heart defects",
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
    grepl("^Q792", Diags), "Selected abdominal wall defects",
    grepl("^Q793", Diags), "Selected abdominal wall defects",
    grepl("^Q90", Diags), "Selected chromosomal defects",
    grepl("^Q914|Q915|Q916|Q917", Diags), "Selected chromosomal defects",
    grepl("^Q910|Q911|Q912|Q913", Diags), "Selected chromosomal defects",
    grepl("^Q96", Diags), "Selected chromosomal defects",
    default = "Other"
  ))] %>%
  .[!Diags %in% c("Q544")] %>% 
  .[,`:=` (qcode = stringr::str_c(unique(Diags), collapse="|"),
           time_cat = cut(BrthYear, breaks = round(length(unique(BrthYear)))/10,
                          include.lowest = TRUE,
                          # right = TRUE,
                          dig.lab = 3),
           qcodecat = fcase(
             grepl("^neural tube", tolower(cat)), "Q00, Q01, Q05, Q76.0",
             grepl("^selected central nervous", tolower(cat)), "Q02, Q03, Q04.1, Q04.2",
             grepl("^selected sense", tolower(cat)), "Q11.0-Q11.2, Q16.0, Q17.2, Q30",
             grepl("^selected congenital heart", tolower(cat)), "Q20, Q21.2, Q21.3, Q23.4, Q25.1",
             grepl("^oro-facial", tolower(cat)), "Q35-Q37",
             grepl("^selected gastrointestinal", tolower(cat)), "Q39.0-Q39.4, Q41, Q42.0-Q42.3, Q43.1, Q44.2",
             grepl("^selected genital", tolower(cat)), "Q53.1, Q53.2, Q53.9, Q54:excludes Q54.4, Q56, Q64.0",
             grepl("^selected urinary", tolower(cat)), "Q60.0-Q60.2, Q61.1-Q61.5, Q61.8, Q61.9, Q64.1-Q64.3",
             grepl("^hip", tolower(cat)), "Q65",
             grepl("^limb", tolower(cat)), "Q71-Q73",
             grepl("^selected abdominal", tolower(cat)), "Q79.2, Q79.3",
             grepl("^selected chromosomal", tolower(cat)), "Q90, Q91.0-Q91.7, Q96",
             grepl("^other", tolower(cat)), "Other"
           )),
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
  .[,.(CaseID, BrthYear, CD_UID, cat, qcode, qcodecat, time_cat)] %>% 
  unique() %>%
  .[,`:=` (count_anom = .N),
    by = .(CD_UID, cat, time_cat)] %>%
  .[,.(CD_UID,  BrthYear, cat, qcode, qcodecat, time_cat, count_anom)] %>% 
  unique()

anom_grp_map <- anom_grp_map %>% 
  group_by(cat, time_cat) %>% 
  tidyr::complete(BrthYear = seq(min(BrthYear),
                                 max(BrthYear),
                                 by = 1),
           nesting(CD_UID, qcode, qcodecat),
           fill = list(count_anom = 0)) %>%
  setDT() %>% 
  .[,`:=` (
    time_lab = paste0(as.numeric(min(BrthYear, na.rm = TRUE)),
                      "-",
                      as.numeric(max(BrthYear, na.rm = TRUE)))
  ),
  by = .(CD_UID, cat, time_cat)]

birth <- cd_birth[tolower(dlv) %in% c("lvb","stillbirth"),
                  `:=` (total_lvb = sum(cd.count_dlv,
                                        na.rm = TRUE)),
                  by = .(BrthYear)] %>% 
  .[!is.na(total_lvb), .(BrthYear, total_lvb)] %>% 
  unique()

birth_map <- cd_birth[tolower(dlv) %in% c("lvb","stillbirth"),
                      `:=` (total_lvb = sum(cd.count_dlv,
                                            na.rm = TRUE)),
                      by = .(BrthYear, CD_UID)] %>% 
  .[!is.na(total_lvb), .(CD_UID, BrthYear, total_lvb)] %>% 
  unique()

# bringing total birth to anomaly data
anom_ind <- merge(anom_ind,
                  birth,
                  by = ("BrthYear")) %>% 
  .[order(cat, BrthYear)] %>% 
  unique() %>% 
  .[,`:=` (rate = 1000*total_anom/total_lvb)] %>% 
  .[order(BrthYear, rate)]

# bringing total birth to anomaly data
anom_grp <- merge(anom_grp,
                  birth,
                  by = ("BrthYear")) %>% 
  .[order(cat, BrthYear)] %>% 
  unique() %>% 
  .[,`:=` (rate = 1000*total_anom/total_lvb)] %>% 
  .[order(BrthYear, rate)]

anom_ind_matage <- merge(anom_ind_matage,
                  birth,
                  by = ("BrthYear")) %>% 
  .[order(cat, BrthYear)] %>% 
  unique() %>% 
  .[,`:=` (rate = 1000*total_anom/total_lvb)] %>% 
  .[order(BrthYear, rate)]

# bringing total birth to anomaly data
anom_grp_matage <- merge(anom_grp_matage,
                  birth,
                  by = ("BrthYear")) %>% 
  .[order(cat, BrthYear)] %>% 
  unique() %>% 
  .[,`:=` (rate = 1000*total_anom/total_lvb)] %>% 
  .[order(BrthYear, rate)]

anom_grp_map <- merge(anom_grp_map,
           birth_map,
           by = c("CD_UID", "BrthYear")) %>% 
  .[order(CD_UID, cat, BrthYear)] %>% 
  unique() %>% 
  .[,.(CD_UID, BrthYear, cat, qcode, qcodecat, time_cat, time_lab, count_anom, total_lvb)] %>% 
  unique() %>% 
  .[,`:=` (total_lvb = sum(total_lvb,
                           na.rm = TRUE)),
   by = .(CD_UID, cat, time_cat)] %>%
  .[,.(CD_UID, cat, qcode, qcodecat, time_cat, time_lab, count_anom, total_lvb)] %>% 
  unique() %>% 
  .[,`:=` (total_anom = sum(count_anom,
                            na.rm = TRUE)),
    by = .(CD_UID, cat, time_cat)] %>%
  .[,.(CD_UID, cat, qcode, qcodecat, time_cat, time_lab, total_anom, total_lvb)] %>% 
  unique() %>% 
  .[order(CD_UID, qcode, time_cat)] %>% 
  .[,`:=` (rate = 1000*total_anom/total_lvb)] %>% 
  .[order(CD_UID, time_cat, rate)]

# key to access cancensus datasets
key = "CensusMapper_f505397ff4bb63467541085d028c9be8"

cd_shp <- data.table::setDT(cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = "12"), 
  level = "CD",
  geo_format = "sf",
  api_key = key
))[,`:=` (name = stringr::str_remove(name, " \\(CTY\\)"),
          cd_type = "County",
          area = `Shape Area`)][,
                               c("GeoUID", "name", "cd_type", "Dwellings 2016","Dwellings", 
                                 "Population 2016", "Population", "Households 2016",
                                 "Households", "area", "geometry")]

cd_names <- data.frame(CD_UID = cd_shp$GeoUID, cd_full = cd_shp$name)


