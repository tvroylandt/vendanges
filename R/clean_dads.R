# ------------------------------------- #
# Vendanges - Nettoyage DADS #
# ------------------------------------- #

library(foreign)
library(tidyverse)

# Disponibilité des variables ---------------------------------------------
varlist_ok <- tibble(
  annee = seq(2005, 2015, 1),
  fichier = paste0(
    "data/dads_source/varlist_postes",
    str_sub(annee, 3, 4),
    ".dbf"
  ),
  varlist = map(fichier, read.dbf)
) %>%
  unnest(varlist) %>%
  group_by(VARIABLE) %>%
  count()

# Import DADS -------------------------------------------------------------
dads_postes <- tibble(
  annee = seq(2005, 2015, 1),
  fichier = paste0("data/dads_source/postes", str_sub(annee, 3, 4), ".dbf"),
  postes = map(fichier, function(x) {
    read.dbf(x) %>%
      as_tibble() %>%
      filter(PCS == "691D")
  })
) %>%
  select(annee, postes) %>% 
  unnest(cols = postes)

# Mise en forme -----------------------------------------------------------



# Export ------------------------------------------------------------------
write_rds(dads_postes, "data/dads_filtre_05_15.rds")
