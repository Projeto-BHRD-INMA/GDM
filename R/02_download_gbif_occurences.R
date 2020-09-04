#install.packages("remotes")
#remotes::install_github("ropensci/taxize")
if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, purrr, readr, magrittr, rgbif)
library(taxize)

#### SUBSTITUIR OS XXXXX pelos seus dados ######
################################################
################################################

user <- "XXXXXXX" # your gbif.org username
pwd <- "XXXXX" # your gbif.org password
email <- "XXXXX" # your email

################################################
################################################

gbif_taxon_keys <-
  read.csv2("GDM/Data/Biotic/lista_BHRD.csv") %>%
  pull(names_wo_author) %>% # use fewer names if you want to just test
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(path = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  filter(kingdom == "Plantae") %>% # remove anything that might have matched to a non-plant
  pull(usagekey) # get the gbif taxonkeys
# gbif_taxon_keys should be a long vector like this c(2977832,2977901,2977966,2977835,2977863)
# !!very important here to use pred_in!!
occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  pred_gte("year", 1950),
  pred("hasCoordinate", TRUE),
  pred("country", "BR"),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)

