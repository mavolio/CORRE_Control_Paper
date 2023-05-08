library(tidyverse)
library(vegan)
library(codyn)
library(PerformanceAnalytics)
library(data.table)
library(broom)

## Sally's desktop
setwd("~/Dropbox/C2E/Products/Control Paper")


###reading in and cleaning corre data
####sk changed this to be in old files folder on July 2020 becuase wants code to run, but do we need to updat to new version?
###SK (and KK) decided that we need to update to include new and longer datasets - did so on 14 Dec 2021

corredat<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/community composition/CoRRE_RelativeCover_Jan2023.csv")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0")%>%
  select(site_code, project_name, community_type, calendar_year, genus_species, relcov, treatment, plot_id, site_project_comm)

corredat_info<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/community composition/CoRRE_ExperimentInfo_Dec2021.csv")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0")

corredat_siteclimate<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/environmental data/CoRRE_siteLocationClimate_Dec2021.csv") 

corredat_sitebiotic<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/environmental data/CoRRE_siteBiotic_Dec2021.csv")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_code!="GVN") #forest understory
#%>% 
 # select (site_code, project_name, community_type) %>% 
  #unique()


 corredat_controls<-corredat%>%
  left_join(corredat_info)%>%
  left_join(corredat_sitebiotic)%>%
  left_join(corredat_siteclimate)%>%
  filter(plot_mani==0, successional==0)%>%
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)
 
 ### use this to check to see if the data is mergeing right after each join
 #filter(is.na(relcov)) %>% 
 #unique()
 
 
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
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)%>%
  filter(site_project_comm!="BUX_PQ_0")%>%
  filter(site_project_comm!="CDR_e001_A")%>%
  filter(site_project_comm!="CDR_e001_B")%>%
  filter(site_project_comm!="CDR_e001_C")%>%
  filter(site_project_comm!="CDR_e001_D")%>%
  filter(site_project_comm!="JSP_GCE_0")%>%
  filter(site_project_comm!="KNZ_IRG_l")%>%
  filter(site_project_comm!="KNZ_IRG_u")%>%
  filter(site_project_comm!="KNZ_pplots_0")%>%
  filter(site_project_comm!="KNZ_RaMPs_0")%>%
  filter(site_project_comm!="maerc_fireplots_0")%>%
  filter(site_project_comm!="NWT_bowman_DryBowman")%>%
  filter(site_project_comm!="PIE_TIDE_0")%>%
  filter(site_project_comm!="SEV_Nfert_0")

         
unique(codyndat_subset$site_project_comm)


### reading in and CLEANING grazing DATASET
### Remove Germany site based on conversations with site owners - its undergoing direction succession
grazing<-read.csv("GExforCoRREControlMS.csv")%>%
  mutate(site_project_comm = site, calendar_year = year,
         plot_id = paste (block, plot, sep = "_"))%>%
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)%>%
  mutate(relcov=relcov/100) %>% 
  filter(site_project_comm!="Germany2")

unique(grazing$site_project_comm)

data<-rbind(grazing, codyndat_subset, corredat_controls2)
unique(data$site_project_comm)

#write.csv(data, "control_subset_data_May2023_v1.csv")

#get data list
datalist<-data%>%
  group_by(site_project_comm) %>% 
  select(calendar_year) %>% 
  unique() %>% 
  summarise(years=n())
#write.csv(datalist, "datasets_used_May2023_v1.csv")

sitelist<- datalist %>% 
  separate(site_project_comm, into=c("site", "project", "comm"), sep = "_")%>% 
  group_by(site) %>% 
  select(years) %>% 
  unique()  %>% 
  summarise(years=n())

####Figures
theme_set(theme_bw())
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=40, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=34, color='black'),
             axis.title.y=element_text(size=40, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=34, color='black'),
             plot.title = element_text(size=40, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))

####FIGURE 1 - Show that things are changing directionally in the controls
#####look at directional change using all years of the data for all sites
spc<-unique(data$site_project_comm)
rt_change<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data%>%
    filter(site_project_comm==spc[i])
  
  out<-rate_change(subset, time.var = 'calendar_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  rt_change<-rbind(rt_change, out)
}
#write.csv(rt_change, 'rate_change_all_May2023_v1.csv')

###with each plot seperate - DONT use this because certain sites are way over-represented (some have 3, some have 50)
ggplot(data=rt_change, aes(x=rate_change))+
  geom_density(aes(y=.0025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .0025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(rate_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(rate_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Rate of Directional Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")


####Average all plots within a site
rt_change2<-rt_change %>% 
  group_by(site_project_comm)%>%
  summarise(rate_change=mean(rate_change))

ggplot(data=rt_change2, aes(x=rate_change))+
  geom_density(aes(y=.0025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .0025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(rate_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(rate_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Rate of Directional Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")
  

#####FIGURE 2 - Multchange and codyn metrics (all 7 panels together?)

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
write.csv(mult_change, 'Comm_change_all_May2023_v1.csv')


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

RACs2<-RACs %>% 
  group_by(calendar_year, site_project_comm)%>%
  summarise(richness_change=mean(richness_change),
            evenness_change=mean(evenness_change, na.rm=T),
            rank_change=mean(rank_change),
            gains=mean(gains),
            losses=mean(losses))

#write.csv(RACs2, file="Control_RACs_AllYears_May2023_v1.csv", row.names=F)

RACs_subset<- RACs2 %>% 
  left_join(mult_change) %>% 
  group_by(site_project_comm)%>%
  sample_n(7)
#write.csv(RACs_subset, file="Control_RACs_subsetDownTo7_toUSE_May2023_v1.csv", row.names=F)


#######START HERE - was having trouble, stats changed every time because it was a random subset of 7 each time
ToUse<-read.csv("Control_RACs_subsetDownTo7_toUSE_May2023_v1.csv")

YearlyMetrics_subset<- ToUse 

MeanMetrics_subset<-YearlyMetrics_subset %>% 
  group_by(site_project_comm) %>% 
  summarise(composition_change=mean(composition_change, na.rm=T), 
            dispersion_change=mean(dispersion_change, na.rm=T),
            richness_change=mean(richness_change),
            evenness_change=mean(evenness_change, na.rm=T),
            rank_change=mean(rank_change),
            gains=mean(gains),
            losses=mean(losses))

####graphs yearly
CompChange<-ggplot(data=YearlyMetrics_subset, aes(x=composition_change))+
  geom_density(aes(y=.025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(composition_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(composition_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Compositional Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

Disp<-ggplot(data=YearlyMetrics_subset, aes(x=dispersion_change))+
  geom_density(aes(y=.01 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .01, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(dispersion_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(dispersion_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Dispersion Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

Rich<-ggplot(data=YearlyMetrics_subset, aes(x=richness_change))+
  geom_density(aes(y=.05 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .05, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(richness_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(richness_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Richness Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

Even<-ggplot(data=YearlyMetrics_subset, aes(x=evenness_change))+
  geom_density(aes(y=.025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(evenness_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(evenness_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Evenness Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

Rank<-ggplot(data=YearlyMetrics_subset, aes(x=rank_change))+
  geom_density(aes(y=.01 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .01, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(rank_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(rank_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Rank Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

Gains<-ggplot(data=YearlyMetrics_subset, aes(x=gains))+
  geom_density(aes(y=.025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(gains, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(gains, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Species Gains")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

Losses<-ggplot(data=YearlyMetrics_subset, aes(x=losses))+
  geom_density(aes(y=.025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(losses, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(losses, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Species Losses")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")



library(grid)
pushViewport(viewport(layout=grid.layout(2,4)))
print(CompChange, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(Disp, vp=viewport(layout.pos.row = 1, layout.pos.col = 2))
print(Even, vp=viewport(layout.pos.row = 1, layout.pos.col = 3))
print(Rank, vp=viewport(layout.pos.row = 1, layout.pos.col = 4))
print(Rich, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(Gains, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(Losses, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))


####graphs by experiment (avereaged yearly numbers together) --- need to get range into figure in upper right corner - wont work 
range<-paste("-1 to 1")

CompChange<-ggplot(data=MeanMetrics_subset, aes(x=composition_change))+
  geom_density(aes(y=.05 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .05, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(composition_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(composition_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Compositional Change")+
  scale_y_continuous(breaks=c(0, 5, 10 , 15), name="Count")+
  theme(legend.position = "none")
  #annotate("text", x=Inf, y=Inf, hjust=0, label=range, size=8, parse=TRUE)
  #geom_text("-1 to +1", mapping=aes(x=Inf, y = Inf), hjust=1.05, vjust=1.5)
Disp<-ggplot(data=MeanMetrics_subset, aes(x=dispersion_change))+
  geom_density(aes(y=.005 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .005, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(dispersion_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(dispersion_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Dispersion Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

Rich<-ggplot(data=MeanMetrics_subset, aes(x=richness_change))+
  geom_density(aes(y=.015 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .015, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(richness_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(richness_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Richness Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

Even<-ggplot(data=MeanMetrics_subset, aes(x=evenness_change))+
  geom_density(aes(y=.01 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .01, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(evenness_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(evenness_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Evenness Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

Rank<-ggplot(data=MeanMetrics_subset, aes(x=rank_change))+
  geom_density(aes(y=.025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(rank_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(rank_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Rank Change")+
  scale_y_continuous(breaks=c(0, 5, 10 , 15), name="Count")+
  theme(legend.position = "none")

Gains<-ggplot(data=MeanMetrics_subset, aes(x=gains))+
  geom_density(aes(y=.025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(gains, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(gains, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Species Gains")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

Losses<-ggplot(data=MeanMetrics_subset, aes(x=losses))+
  geom_density(aes(y=.025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(losses, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(losses, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Species Losses")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")



library(grid)
pushViewport(viewport(layout=grid.layout(2,4)))
print(CompChange, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(Disp, vp=viewport(layout.pos.row = 1, layout.pos.col = 2))
print(Even, vp=viewport(layout.pos.row = 1, layout.pos.col = 3))
print(Rank, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(Rich, vp=viewport(layout.pos.row = 1, layout.pos.col = 4))
print(Gains, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(Losses, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))























####TABLE 1 - looking at how change varies with site level characteristics
###USE YearlyMetrics_subset and MeanMetrics_subset (which was created earlier)

#Join with yearly data
YearlyLevelData<- YearlyMetrics_subset%>% 
  left_join(corredat_sitebiotic)%>% 
  left_join((corredat_siteclimate))

unique(YearlyLevelData$site_project_comm)

Meta<-YearlyLevelData %>% 
  select(site_project_comm, anpp, MAP, MAT, rrich) %>% 
  group_by(site_project_comm) %>% 
  summarise(ANPP=mean(anpp), MAP=mean(MAP), MAT=mean(MAT), rrich=mean(rrich))

Meta2<-Meta %>% 
  filter (MAP!="NA")

GoodCorre<-Meta2 %>% 
  mutate(drop=1) %>% 
  select(site_project_comm, drop)

Meta3<-read.csv("~/Dropbox/C2E/Products/Control Paper/Meta_Jan2021_needforDec2021Code.csv") %>% 
  filter(site_project_comm!="Germany2") %>% 
  left_join(GoodCorre) %>% 
  mutate_at(c("drop"), ~replace(., is.na(.), 0)) %>% 
  filter(drop=="0") %>% 
  select(-drop)

Meta4<-Meta3 %>% 
  full_join(Meta2) 
#write.csv(Meta, file="Meta_Dec2021.csv", row.names=F)

YearlyLevelData2<- YearlyMetrics_subset%>% 
  left_join(Meta4)
unique(YearlyLevelData2$site_project_comm)

####Correlations
pairs(YearlyLevelData2[,c(3:14)])
chart.Correlation(YearlyLevelData2[,3:14], histogram=TRUE, method="pearson")



#Regressions
YearlyLevelDataLong<-YearlyLevelData2 %>% 
  select(-calendar_year, -calendar_year2)%>%
  gather(metric, value, richness_change:dispersion_change) %>% 
  group_by(site_project_comm, ANPP, MAP, MAT, rrich, metric) %>% 
  summarise_at(vars(value), list(mean), na.rm=T) %>% 
  ungroup()
unique(YearlyLevelDataLong$site_project_comm)
YearlyLevelDataLong<-as_tibble(YearlyLevelDataLong)
Regressions<-YearlyLevelDataLong %>% 
  nest(-metric) %>% 
  mutate(
    fit=map(data, ~lm(value~ANPP, data=.x)), 
    tidied=map(fit, tidy),
    glanced=map(fit, glance)
  )

ANPP_R<-as.data.frame(Regressions %>% 
  unnest(glanced)) %>% 
  select(metric, df, df.residual, statistic, r.squared)

ANPP_P<-as.data.frame(Regressions %>% 
  unnest(tidied)) %>% 
  filter(term!="(Intercept)") %>% 
  select(metric, estimate, std.error, p.value)

ANPP_RegStats<-ANPP_P%>%
  left_join(ANPP_R)

ANPP_RegStats_Adjusted<-ANPP_RegStats %>% 
  mutate(padjust=p.adjust(p.value, method = "BH", n=28))

#write.csv(ANPP_RegStats_Adjusted, file="ANPP_RegStats_May2023_AdjustedP.csv", row.names=F)



Regressions<-YearlyLevelDataLong %>% 
  nest(-metric) %>% 
  mutate(
    fit=map(data, ~lm(value~MAP, data=.x)), 
    tidied=map(fit, tidy),
    glanced=map(fit, glance)
  )

ANPP_R<-as.data.frame(Regressions %>% 
                        unnest(glanced)) %>% 
  select(metric, df, df.residual, statistic, r.squared)

ANPP_P<-as.data.frame(Regressions %>% 
                        unnest(tidied)) %>% 
  filter(term!="(Intercept)") %>% 
  select(metric, estimate, std.error, p.value)

MAP_RegStats<-ANPP_P%>%
  left_join(ANPP_R)
MAP_RegStats_Adjusted<-MAP_RegStats %>% 
  mutate(padjust=p.adjust(p.value, method = "BH", n=28))

#write.csv(MAP_RegStats_Adjusted, file="MAP_RegStats_May2023_AdjustedP.csv", row.names=F)



Regressions<-YearlyLevelDataLong %>% 
  nest(-metric) %>% 
  mutate(
    fit=map(data, ~lm(value~MAT, data=.x)), 
    tidied=map(fit, tidy),
    glanced=map(fit, glance)
  )

ANPP_R<-as.data.frame(Regressions %>% 
                        unnest(glanced)) %>% 
  select(metric, df, df.residual, statistic, r.squared)

ANPP_P<-as.data.frame(Regressions %>% 
                        unnest(tidied)) %>% 
  filter(term!="(Intercept)") %>% 
  select(metric, estimate, std.error, p.value)

MAT_RegStats<-ANPP_P%>%
  left_join(ANPP_R)
MAT_RegStats_Adjusted<-MAT_RegStats %>% 
  mutate(padjust=p.adjust(p.value, method = "BH", n=28))

#write.csv(MAT_RegStats_Adjusted, file="MAT_RegStats_May2023_AdjustedP.csv", row.names=F)


Regressions<-YearlyLevelDataLong %>% 
  nest(-metric) %>% 
  mutate(
    fit=map(data, ~lm(value~rrich, data=.x)), 
    tidied=map(fit, tidy),
    glanced=map(fit, glance)
  )

ANPP_R<-as.data.frame(Regressions %>% 
                        unnest(glanced)) %>% 
  select(metric, df, df.residual, statistic, r.squared)

ANPP_P<-as.data.frame(Regressions %>% 
                        unnest(tidied)) %>% 
  filter(term!="(Intercept)") %>% 
  select(metric, estimate, std.error, p.value)

RRich_RegStats<-ANPP_P%>%
  left_join(ANPP_R)
RRich_RegStats_Adjusted<-RRich_RegStats %>% 
  mutate(padjust=p.adjust(p.value, method = "BH", n=28))

#write.csv(RRich_RegStats_Adjusted, file="RRich_RegStats_May2023_AdjustedP.csv", row.names=F)




###CompChange test, trying to figure out if tibble method gives correct
YearlyLevelDataLong_MAP<-YearlyLevelDataLong %>% 
  filter(metric=="composition_change")
CompMAP<-lm(value~MAP, data=YearlyLevelDataLong_MAP)
summary(CompMAP)
### numbers are the same as above (1,59)



####FIGURES WHICH WE ARE NOT using
### make things in long form

SiteLevelDataLong<-YearlyLevelDataLong
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

