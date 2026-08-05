library(xml2)
library(dplyr)
library(here)

# Carica i file XML
# Leggi i componenti da Sempreanorma
componenti_san_it <- read_xml(here("Sempreanorma", "componenti_san_it.xml"))
componenti_san_it <- xml_find_all(componenti_san_it, ".//nome") %>%
  xml_text() %>% tolower() %>% as.data.frame() 
componenti_san_it <- componenti_san_it %>% distinct()
colnames(componenti_san_it) <- "componente"
length(componenti_san_it$componente)

# Leggi i componenti da Configuratore
componenti_configuratore<- read_xml(here("Configuratore", "componenti_configuratore.xml"))
componenti_configuratore <- xml_find_all(componenti_configuratore, ".//name") %>%
  xml_text() %>% tolower() %>% as.data.frame()
componenti_configuratore <- componenti_configuratore %>% distinct()
colnames(componenti_configuratore) <- "componente"
length(componenti_configuratore$componente)

# Confronta i componenti
componenti_confronto <- componenti_configuratore %>% 
    filter(componente %in% componenti_san_it$componente)
head(componenti_confronto)
length(componenti_confronto$componente) # SOLO 21 MATCH 


