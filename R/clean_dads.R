# ------------------------------------- #
# Vendanges - DADS #
# ------------------------------------- #

library(foreign)
library(tidyverse)
library(writexl)

# Variables nécessaires
# SEXE
# AGE_TR (ou AGE)
# PCS
# POND
# DATDEB
# DATFIN
# DEP
# ETRANGER


# Disponibilité des variables ---------------------------------------------
# On ne conserve que les variables présentes sur toute la période
varlist_ok <- tibble(annee = seq(2005, 2015, 1),
                    fichier = paste0("data/dads_source/varlist_postes", str_sub(annee, 3, 4), ".dbf"),
                    varlist = map(fichier, read.dbf)) %>% 
  unnest(varlist) %>% 
  group_by(VARIABLE) %>% 
  count() %>% 
  filter(n == 11)

# Chargement --------------------------------------------------------------
dads2015_postes <- read.dbf("Data/dads2015_postes/postes15.dbf") %>% 
  as_tibble()

dads2010_postes <- read.dbf("Data/dads2010_postes/postes10.dbf") %>% 
  as_tibble()


# Démographie -------------------------------------------------------------
# 2015
dads_demo_2015 <- dads2015_postes %>%
  filter(PCS %in% c("691D")) %>% 
  group_by(SEXE, AGE_TR) %>% 
  summarise(n = sum(POND)) %>% 
  ungroup() %>% 
  mutate(part_n = n / sum(n) * 100,
         SEXE = paste0(SEXE, "_2015")) %>% 
  select(-n) %>%
  spread(SEXE, part_n)

# 2010
dads_demo_2010 <- dads2010_postes %>%
  filter(PCS %in% c("691D")) %>% 
  mutate(AGE_TR = cut(AGE, breaks = c(0, 15, 19, 23, 27, 31, 35, 39, 
                                      43, 47, 51, 55, 59, 63, 67, 71, 150), 
                      include.lowest = TRUE, right = FALSE,
                      labels = c("00", "19", "23", "27", "31", "35", "39",
                                 "43", "47", "51", "55", "59", "63", "67", "71", "72"))) %>% 
  group_by(SEXE, AGE_TR) %>% 
  summarise(n = sum(POND)) %>% 
  ungroup() %>% 
  mutate(part_n = n / sum(n) * 100,
         SEXE = paste0(SEXE, "_2010")) %>%
  select(-n) %>% 
  spread(SEXE, part_n) %>%
  filter(AGE_TR != "00" & !is.na(AGE_TR))

# Fusion
dads_demo_2015 %>% 
  left_join(dads_demo_2010, by = c("AGE_TR")) %>% 
  write_xlsx("Outputs/dads_demo_age_sexe.xlsx")

# Par jour ------------------------------------------------------------------
seq_empl <- dads2015_postes %>%
  filter(PCS %in% c("691D")) %>% 
  group_by(DATDEB, DATFIN) %>% 
  summarise(n = sum(POND))

seq_empl_jour <- tibble(jour = seq(-30, 360, 1)) %>% 
  mutate(join = "1") %>% 
  full_join((seq_empl %>% 
               mutate(join = "1")),
            by = "join") %>% 
  mutate(ind_ok = case_when(jour >= DATDEB & jour <= DATFIN ~ "1",
                            TRUE ~ "0")) %>% 
  filter(ind_ok == "1") %>% 
  group_by(jour) %>% 
  summarise(n = sum(n))  %>% 
  filter(jour > 0) %>% 
  ungroup() %>% 
  mutate(part_n = n / sum(n))

# Export
write_xlsx(seq_empl_jour, "Outputs/seq_dads.xlsx")
