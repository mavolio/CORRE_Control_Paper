### Cleaning species names for GEx/Codyn species comp data and calculating % annual
###
### Author: Kevin Wilcox (k_wilcox@uncg.edu)
### Date created May 12, 2023

setwd("C:\\Users\\wilco\\OneDrive - UNCG\\Working groups\\sDiv\\CoRRE data\\") ## wilcox personal laptop
setwd("C:\\Users\\k_wilcox\\OneDrive - UNCG\\Working groups\\sDiv\\CoRRE data\\") ## wilcox office desktop

library(tidyverse)
###
### Calculate % abundance of annual species for each site
###

### THERE ARE MISSING GENUS SPECIES NAMES IN THE RELATIVE COVER DATA FILE
### FOR NOW, I AM JUST REMOVING THESE DATA...

### Read in and clean datasets
corre2try <- read.csv("corre2trykey_2021.csv") %>% # key to merge genus_species column to cat trait data
  dplyr::select(genus_species, species_matched) %>%
  unique(.)
#species_df <- read.csv("CoRRE_TRY_species_list.csv")

codyn2try <- read.csv("GEx_species_family_KWaddedSp20230512.csv")

### Read in and prep Codyn species comp data
grazing<-read.csv("GExforCoRREControlMS.csv")%>%
  mutate(site_project_comm = site, calendar_year = year,
         plot_id = paste (block, plot, sep = "_"))%>%
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)%>%
  mutate(relcov=relcov/100) %>%
  filter(site_project_comm!="Germany2")

codyn_spname <- grazing %>%
  dplyr::select(genus_species) %>%
  unique(.) %>%
  left_join(dplyr::select(codyn2try, genus_species, clean_ejf)) %>%
  rename("species_matched"="clean_ejf")

codyn_cat_traits <- codyn_spname %>%
  left_join(trait_cat_df)

write.csv(codyn_cat_traits, file="codyn_lifespan_traits.csv")

### Sally filled in spreadsheet with lifespan traits for all species generated in above script
### Read in this filled in dataset and calculate %Annual cover

cat_trait_df <- read.csv("codyn_lifespan_traits_GExFilled.csv")

grazing<-read.csv("GExforCoRREControlMS.csv")%>%
  mutate(site_project_comm = site, calendar_year = year,
         plot_id = paste (block, plot, sep = "_"))%>%
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)%>%
  mutate(relcov=relcov/100) %>%
  filter(site_project_comm!="Germany2")

spcomp_and_traits <- grazing %>%
  full_join(cat_trait_df, by="genus_species")

gex_perc_annual <- spcomp_and_traits %>%
  dplyr::select(site_project_comm, calendar_year, plot_id, genus_species, relcov, lifespan) %>%
  group_by(site_project_comm, calendar_year, plot_id, lifespan) %>%
  summarize(lifespan_cover=sum(relcov, na.rm=T)) %>%
  ungroup() %>%
  group_by(site_project_comm, lifespan) %>%
  summarize(lifespan_cover = mean(lifespan_cover, na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from=lifespan, values_from=lifespan_cover) %>%
  replace(is.na(.), 0) %>%
  mutate(annual_relcov = annual/(annual+perennial+biennial+uncertain)) #%>%
#  dplyr::select(site_code, project_name, community_type, annual_relcov)

write.csv(gex_perc_annual, paste0("GEx percent annual_",Sys.Date(),".csv"), row.names=F)

###
### Extra code below
###

exp_info_df <- read.csv("ExperimentInfo.csv")

unkn_sp_lifespan <- read.csv("species with no cat traits.csv") %>%
  rename(species_matched=genus_species)

# Read in trait data and combine with manually entered lifespan data (for problem species)
trait_cat_df <- read.csv("trait data\\sCoRRE categorical trait data_12142022.csv") %>%
  dplyr::select(species_matched, lifespan) %>%
  bind_rows(unkn_sp_lifespan) %>%
  mutate(lifespan=replace(lifespan, lifespan=="moss","perennial")) %>%
  mutate(lifespan=replace(lifespan, species_matched=="Robinia pseudo","perennial")) %>%
  filter(species_matched!="")

## New data to join
new_cov_df <- read.csv("control_subset_dataforAnnulRelcov_May2023_v1.csv")


### join genus_species names with species_matched for future joining with trait data
relcov_df <- read.csv("RelativeCover.csv") %>%
  left_join(corre2try, by="genus_species") %>%
  filter(genus_species != "")

### Insert genus_species names into species_matched column only for species that we do not have trait data for -- this step is so we can include the manually entered lifespan data
relcov_df <- relcov_df %>%
  mutate(species_matched = replace(species_matched, is.na(species_matched),
                                   relcov_df[is.na(relcov_df$species_matched),'genus_species']))

### Checking that no-trait species have been appended to the species_matched column
#missing <- rel_cov_df[is.na(rel_cov_df$species_matched),]
# missing <- rel_cov_df[is.na(rel_cov_df$AccSpeciesID),]
# missing_sp_vec <- unique(missing$genus_species)

### Combine relative cover with trait data
relcov_trait_df <- relcov_df %>%
  left_join(trait_cat_df, by="species_matched")

with(z, table(calendar_year, plot_id))

### Calculate percent unknown and percent annual
perc_annual <- relcov_trait_df %>%
  left_join(exp_info_df) %>%
  dplyr::select(site_code, project_name, community_type, calendar_year, plot_mani, plot_id, species_matched, relcov, lifespan) %>%
  filter(plot_mani==0) %>%
  group_by(site_code, project_name, community_type, calendar_year, lifespan, plot_id) %>%
  summarize(lifespan_cover=sum(relcov, na.rm=T)) %>%
  ungroup() %>%
  group_by(site_code, project_name, community_type, lifespan) %>%
  summarize(lifespan_cover = mean(lifespan_cover, na.rm=T)) %>%
  ungroup() %>%
  mutate(lifespan=replace(lifespan, lifespan=="fungus-lichen","FungusLichen")) %>%
  pivot_wider(names_from=lifespan, values_from=lifespan_cover) %>%
  replace(is.na(.), 0) %>%
  mutate(annual_relcov = annual/(annual+perennial+biennial+uncertain+FungusLichen)) %>%
  dplyr::select(site_code, project_name, community_type, uncertain, annual_relcov)