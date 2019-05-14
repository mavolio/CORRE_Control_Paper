library(tidyverse)
library(codyn)

## Sally's desktop
setwd("~/Dropbox/C2E/Products/Control Paper")

###reading in list of datasets to use
<<<<<<< HEAD
list<-read.csv("controls_data list.csv")%>%
=======
list<-read.csv("~/Dropbox/converge_diverge/Control Paper/controls_data list.csv")%>%
>>>>>>> e58dd5fac2bd257acd5df894550221ddc1de87d7
  mutate(site_project_comm = paste (site_code, project_name, community_type, sep = "_"),
         keep = 1)

###reading in and cleaning corre data
corredat<-read.csv("~/Dropbox/converge_diverge/datasets/LongForm/SpeciesRelativeAbundance_March2019.csv")%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0")%>%
  select(site_code, project_name, community_type, calendar_year, genus_species, relcov, treatment, plot_id, site_project_comm)

corredat_info<-read.csv("~/Dropbox/converge_diverge/datasets/LongForm/ExperimentInformation_March2019.csv")%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0")

corredat_siteinfo<-read.csv("~/Dropbox/converge_diverge/datasets/LongForm/SiteExperimentDetails_March2019.csv")%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0")

corredat_controls<-corredat%>%
  left_join(corredat_info)%>%
  left_join(corredat_siteinfo)%>%
  filter(plot_mani==0, experiment_length>7, successional==0)%>%
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)


unique(corredat_controls$site_project_comm)


### reading in and CLEANING CODYN DATASET
#restrict to species that are present in an experiment
codyndat<-read.csv('relative cover_nceas and converge_12012015_cleaned.csv')%>%
  gather(species, abundance, sp1:sp99)%>%
  filter(site_code!="MISS")

codyndat_info<-read.csv("siteinfo_key.csv")%>%
  filter(site_project_comm!="")

splist<-codyndat%>%
  group_by(site_code, project_name, community_type, species)%>%
  summarize(present=sum(abundance))%>%
  filter(present!=0)%>%
  select(-present)

#merge back and will drop species that do not exist in a dataset
codyndat_clean<-merge(codyndat, splist, by=c("site_code","project_name","community_type","species"))%>%
  select(-site_project_comm)%>%
  mutate(site_project_comm = paste(site_code, project_name, community_type, sep = "_"))%>%
  select(-X, -sitesubplot, -site_code, -project_name, -community_type)%>%
  mutate(id=paste(site_project_comm, plot_id, sep="::"))%>%
  mutate(genus_species=species, calendar_year = experiment_year, relcov = abundance, site_project_comm)%>%
  left_join(codyndat_info)

codyndat_subset<-codyndat_clean%>%
  filter(dataset_length>7, broad_ecosystem_type=="herbaceous", taxa=="plants", succession=="no")%>%
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)


unique(codyndat_subset$site_project_comm)


### reading in and CLEANING grazing DATASET
<<<<<<< HEAD
grazing<-read.csv("GExforCoRREControlMS.csv")%>%
=======
grazing<-read.csv("~/Dropbox/Konza Research/lights in the prairie/GExforCoRREControlMS.csv")%>%
>>>>>>> e58dd5fac2bd257acd5df894550221ddc1de87d7
  mutate(site_project_comm = site, calendar_year = year,
         plot_id = paste (block, plot, sep = "_"))%>%
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)



data<-rbind(grazing, codyndat_subset, corredat_controls)
unique(data$site_project_comm)

write.csv(data, "~/Dropbox/C2E/Products/Control Paper/original_subset_data.csv")


#####look at mult change
spc<-unique(data$site_project_comm)
mult_change<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data%>%
    filter(site_project_comm==spc[i])
  
  out<-multivariate_change(subset, time.var = 'calendar_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  mult_change<-rbind(mult_change, out)
}
write.csv(mult_change, 'Comm_change_all_May2019.csv')
hist(mult_change$composition_change)

#####look at directional change with intervals #I am not sure which one we want
spc<-unique(data$site_project_comm)
rt_change_int<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data%>%
    filter(site_project_comm==spc[i])
  
  out<-rate_change_interval(subset, time.var = 'calendar_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  rt_change_int<-rbind(rt_change_int, out)
}
write.csv(rt_change_int, 'rate_change_interval_all_May2019.csv')
#####look at directional change
spc<-unique(data$site_project_comm)
rt_change<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data%>%
    filter(site_project_comm==spc[i])
  
  out<-rate_change(subset, time.var = 'calendar_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  rt_change<-rbind(rt_change, out)
}
write.csv(rt_change, 'rate_change_all_May2019.csv')

