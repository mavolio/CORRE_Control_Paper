library(tidyverse)
library(vegan)
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
  summarise(composition_change=mean(composition_change, na.rm=T), dispersion_change=mean(dispersion_change, na.rm=T))
hist(mult_change2$composition_change)
hist(mult_change2$dispersion_change)

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
  summarise(cumsum=mean(cumsum, na.rm=T))
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

RichEvar2<-RichEvar %>% 
  group_by(site_project_comm, plot_id) %>% 
  summarise(richness=mean(richness),
            Evar=mean(Evar, rn.ma=T))
hist(RichEvar2$richness)
hist(RichEvar2$Evar)

RichEvar3<-RichEvar2 %>% 
  group_by(site_project_comm) %>% 
  summarise(richness=mean(richness),
            Evar=mean(Evar, rn.ma=T))
hist(RichEvar3$richness)
hist(RichEvar3$Evar)


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
diversity2<-diversity %>% 
  group_by(site_project_comm, plot_id) %>% 
  summarise(Shannon=mean(Shannon, rn.ma=T))
hist(diversity2$Shannon)


diversity3<-diversity2 %>% 
  group_by(site_project_comm) %>% 
  summarise(Shannon=mean(Shannon))
hist(diversity3$Shannon)



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
            evenness_change=mean(evenness_change, na.rm=T),
            rank_change=mean(rank_change),
            gains=mean(gains),
            losses=mean(losses))
RACs3<-RACs2 %>% 
  group_by(site_project_comm) %>% 
  summarise(richness_change=mean(richness_change),
            evenness_change=mean(evenness_change, na.rm=T),
            rank_change=mean(rank_change),
            gains=mean(gains),
            losses=mean(losses))

hist(RACs3$richness_change)
hist(RACs3$evenness_change)
hist(RACs3$rank_change)
hist(RACs3$gains)
hist(RACs3$losses)


####calculate gamma diversity
species <- data%>%
  tbl_df()%>%
  group_by(site_project_comm, calendar_year, plot_id, genus_species)%>%
  summarise(relcov=mean(relcov))%>%
  filter(genus_species!="")%>%
  tbl_df()

SampleIntensity<-species%>%
  tbl_df()%>%
  group_by(site_project_comm, plot_id, calendar_year)%>%
  summarize(SampleIntensity=length(relcov))%>%
  tbl_df()%>%
  group_by(site_project_comm)%>%
  summarize(SampleIntensity=length(SampleIntensity))%>%#how many plots were sampled over the course of the experiment
  tbl_df()

exp<-unique(SampleIntensity$site_project_comm)

#create empty dataframe for loop
estimatedRichness=data.frame(row.names=1)

for(i in 1:length(exp)) {
  #creates a dataset for each unique experiment
  subset <- species%>%
    filter(site_project_comm==exp[i])%>%
    select(site_project_comm, plot_id, calendar_year, genus_species, relcov)
  #transpose data into wide form
  speciesData <- subset%>%
    spread(genus_species, relcov, fill=0)
  #calculate species accumulation curves
  pool <- poolaccum(speciesData[,4:ncol(speciesData)], permutations=100)
  chao <- as.data.frame(as.matrix(pool$chao))#this gives us estimated richness from 1-X samples
  chao$aveChao<-rowMeans(chao)
  chao$n<-row.names(chao)
  chao$exp<-exp[i]
  chao2<-chao%>%
    select(exp,n, aveChao)
  
  #rbind back
  estimatedRichness<-rbind(chao2, estimatedRichness)
  
}



ExpRichness<-estimatedRichness%>%
  filter(n==30)%>%#the lowest sampling intensity -2
  mutate(rrich=aveChao)%>%
  select(-n, -aveChao) %>% 
  rename(site_project_comm=exp)


###merge together at site level alll metrics (RACs, cumsum, richeven?, diversity? - dont do richness, evenness and diversity because all plots are different sizes and comparing across sites means nothing, also leave out cumsum because it will be bigger for the longer datasets)

SiteLevel<- mult_change2 %>% 
  left_join(rt_change2) %>% 
  left_join(RACs3)

### import sitelevel data
SiteInfo<-read.csv("datasets_used_May2019_withAddedSiteInfo.csv")%>%
  left_join(ExpRichness)

SiteLevelData<-SiteInfo %>% 
  left_join((SiteLevel))


####Regressions

pairs(SiteLevelData[,c(2:12)])

### make things in long form

SiteLevelDataLong<-SiteLevelData %>% 
  gather(metric, value, composition_change:losses)

###ANPP
rvalues <- SiteLevelDataLong %>%
  group_by(metric) %>%
  summarize(r.value = round((cor.test(ANPP, value)$estimate), digits=3),
            p.value = (cor.test(ANPP, value)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))

ggplot(data=SiteLevelDataLong, aes(x=ANPP, y=value))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~metric, scales="free")+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)


###MAP
rvalues <- SiteLevelDataLong %>%
  group_by(metric) %>%
  summarize(r.value = round((cor.test(MAP, value)$estimate), digits=3),
            p.value = (cor.test(MAP, value)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))

ggplot(data=SiteLevelDataLong, aes(x=MAP, y=value))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~metric, scales="free")+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

###MAT
rvalues <- SiteLevelDataLong %>%
  group_by(metric) %>%
  summarize(r.value = round((cor.test(MAT, value)$estimate), digits=3),
            p.value = (cor.test(MAT, value)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))

ggplot(data=SiteLevelDataLong, aes(x=MAT, y=value))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~metric, scales="free")+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

###rrich
rvalues <- SiteLevelDataLong %>%
  group_by(metric) %>%
  summarize(r.value = round((cor.test(rrich, value)$estimate), digits=3),
            p.value = (cor.test(rrich, value)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))

ggplot(data=SiteLevelDataLong, aes(x=rrich, y=value))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~metric, scales="free")+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)
