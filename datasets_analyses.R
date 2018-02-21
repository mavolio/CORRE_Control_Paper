library(tidyverse)
library(codyn)

###reading in list of datasets to use
list<-read.csv("C:\\Users\\megha\\Dropbox\\converge_diverge\\Control Paper\\controls_data list.csv")%>%
  mutate(site_project_comm = paste (site_code, project_name, community_type, sep = "_"),
         keep = 1)

###reading in and cleaning corre data
corredat<-read.csv("~/Dropbox/converge_diverge/datasets/LongForm/SpeciesRelativeAbundance_Oct2017.csv")%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0")%>%
  select(site_code, project_name, community_type, calendar_year, genus_species, relcov, treatment, plot_id, site_project_comm)

corredat_info<-read.csv("~/Dropbox/converge_diverge/datasets/LongForm/ExperimentInformation_Nov2017.csv")%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0")

corredat_controls<-corredat%>%
  left_join(corredat_info)%>%
  filter(plot_mani==0)%>%
  select(site_project_comm, calendar_year, plot_id, genus_species, relcov)%>%
  filter(site_project_comm %in% c("ANG_watering_0","ARC_MAT2_0","BUX_PQ_0","CDR_e001_D","JSP_GCE_0","KBS_T7_0","KLU_BFFert_0", "KUFS_E6_type1", "NWT_bowman_DryBowman","SERC_TMECE_MX"))

unique(corredat_controls$site_project_comm)


### reading in and CLEANING CODYN DATASET
#restrict to species that are present in an experiment
codyndat<-read.csv('~/Dropbox/CoDyn/R Files/11_06_2015_v7/relative cover_nceas and converge_12012015_cleaned.csv')%>%
  gather(species, abundance, sp1:sp99)%>%
  filter(site_code!="MISS")

codyndat_info<-read.csv("~/Dropbox/CoDyn/R Files/11_06_2015_v7/siteinfo_key.csv")%>%
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
  select(site_project_comm, calendar_year, plot_id, genus_species, relcov)

codyndat_subset<-codyndat_clean%>%
  filter(site_project_comm %in% c("HAY_kansas_0","JRN_NPP_0","KNZ_00id_0","SEV_veg_G","SGS_UNUN_0"))

unique(codyndat_subset$site_project_comm)


### reading in and CLEANING grazing DATASET
grazing<-read.csv("~/Dropbox/lights in the prairie/GExforCoRREControlMS.csv")%>%
  mutate(site_project_comm = site, calendar_year = year,
         plot_id = paste (block, plot, sep = "_"))%>%
  filter(site_project_comm != "KRNP_Lammertjiesleegte")%>%
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)



data<-rbind(grazing, codyndat_subset, corredat_controls)
unique(data$site_project_comm)


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
write.csv(mult_change, '~/Dropbox/converge_diverge/Control Paper/Output/Comm_change_all.csv')
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
write.csv(rt_change_int, '~/Dropbox/converge_diverge/Control Paper/Output/rate_change_interval_all.csv')
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
write.csv(rt_change, '~/Dropbox/converge_diverge/Control Paper/Output/rate_change_all.csv')

