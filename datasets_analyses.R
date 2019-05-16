library(tidyverse)
library(codyn)


## Sally's desktop
setwd("~/Dropbox/C2E/Products/Control Paper")


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

datasetlength<-corredat_controls%>%
select(site_project_comm, calendar_year)%>%
  unique()%>%
  group_by(site_project_comm)%>%
  summarise(length=n())%>%
  filter(length>7)%>%
  select(-length)
  
corredat_controls2<-corredat_controls%>%
  right_join(datasetlength)


unique(corredat_controls2$site_project_comm)


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
grazing<-read.csv("GExforCoRREControlMS.csv")%>%
  mutate(site_project_comm = site, calendar_year = year,
         plot_id = paste (block, plot, sep = "_"))%>%
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)%>%
  mutate(relcov=relcov/100)



data<-rbind(grazing, codyndat_subset, corredat_controls2)
unique(data$site_project_comm)

#write.csv(data, "control_subset_data.csv")

#get data list
datalist<-data%>%
  select(site_project_comm) %>% 
  unique()
#write.csv(datalist, "datasets_used_May2019.csv")


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
#write.csv(mult_change, 'Comm_change_all_May2019.csv')

hist(mult_change$composition_change)
mult_change2<-mult_change %>% 
  group_by(site_project_comm)%>%
  summarise(composition_change=mean(composition_change))
hist(mult_change2$composition_change)

change_cumsum<-mult_change%>%
  group_by(site_project_comm)%>%
  mutate_at(vars(composition_change), 
            list(cumsum))%>%
  rename(start=calendar_year)%>%
  rename(end=calendar_year2)%>%
  rename(cumsum=composition_change)%>%
  select(-dispersion_change)
hist(change_cumsum$cumsum)
change_cumsum2<-change_cumsum %>% 
  group_by(site_project_comm)%>%
  summarise(cumsum=mean(cumsum))
hist(change_cumsum2$cumsum)

#write.csv(change_sumsum, 'Change_Cumsum_May2019.csv')

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
#write.csv(rt_change_int, 'rate_change_interval_all_May2019.csv')

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
#write.csv(rt_change, 'rate_change_all_May2019.csv')

rt_change2<-rt_change %>% 
  group_by(site_project_comm)%>%
  summarise(rate_change=mean(rate_change))
hist(rt_change2$rate_change)


####### Codyn Metrics
#Richness and Evar
spc<-unique(data$site_project_comm)
RichEvar<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data%>%
    filter(site_project_comm==spc[i])
  
  out<-community_structure(subset, time.var = 'calendar_year', abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  RichEvar<-rbind(RichEvar, out)
}

hist(RichEvar$richness)
hist(RichEvar$Evar)


#diversity
spc<-unique(data$site_project_comm)
diversity<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data%>%
    filter(site_project_comm==spc[i])
  
  out<-community_diversity(subset, time.var = 'calendar_year', abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  diversity<-rbind(diversity, out)
}

hist(diversity$Shannon)

#RAC_Change
spc<-unique(data$site_project_comm)
RACs<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_change(subset, time.var = 'calendar_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  RACs<-rbind(RACs, out)
}

hist(RACs$richness_change)
hist(RACs$evenness_change)
hist(RACs$rank_change)
hist(RACs$gains)
hist(RACs$losses)

RACs2<-RACs %>% 
  group_by(plot_id, site_project_comm)%>%
  summarise(richness_change=mean(richness_change),
            evenness_change=mean(evenness_change, rn.ma=T),
            rank_change=mean(rank_change),
            gains=mean(gains),
            losses=mean(losses))

hist(RACs2$richness_change)
hist(RACs2$evenness_change)
hist(RACs2$rank_change)
hist(RACs2$gains)
hist(RACs2$losses)



###merge together at plot level alll metrics (RACs, cumsum, richeven?, diversity?)
mult_change
change_cumsum
rt_change
###merge together at site/exp level alll metrics
mult_change2
change_cumsum2
rt_change2
