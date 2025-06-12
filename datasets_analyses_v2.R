library(tidyverse)
library(vegan)
library(codyn)
library(PerformanceAnalytics)
library(data.table)
library(broom)
stderror <- function(x) sd(x)/sqrt(length(x))

## Sally's desktop
setwd("~/Dropbox/C2E/Products/Control Paper")


###reading in and cleaning corre data
####sk changed this to be in old files folder on July 2020 becuase wants code to run, but do we need to updat to new version?
###SK (and KK) decided that we need to update to include new and longer datasets - did so on 14 Dec 2021
names<-read.csv("~/Dropbox/C2E/Products/Control Paper/FullList_Nov2021.csv")

corredat<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/community composition/old files/CoRRE_RelativeCover_Jan2023.csv")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0")%>%
  select(site_code, project_name, community_type, calendar_year, genus_species, relcov, treatment, plot_id, site_project_comm) %>% 
  full_join(names) %>% 
  filter(relcov!="NA") %>% 
  group_by(site_code, project_name, community_type, site_project_comm, calendar_year, treatment, plot_id,  species_matched) %>% 
  summarise(relcov=(mean(relcov))) %>% 
  filter(species_matched!="NA") %>%  ###MAY 10, 2023 --- this last line here is dropping 6 instances of an unknown species at KNZ that we cannot figure out. But maybe one day try adding them back in if kevin can figrue it out 
  ungroup()


corredat_info<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/community composition/old files/CoRRE_ExperimentInfo_Dec2021.csv")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0")

corredat_siteclimate<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/environmental data/CoRRE_siteLocationClimate_Dec2021.csv") 

corredat_sitebiotic<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/environmental data/CoRRE_siteBiotic_May2023.csv")%>%
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
  select(site_project_comm, calendar_year, species_matched, relcov, plot_id)

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
  mutate(species_matched=species, calendar_year = experiment_year, relcov = abundance, site_project_comm)%>%
  left_join(codyndat_info)

codyndat_subset<-codyndat_clean%>%
  filter(dataset_length>7, broad_ecosystem_type=="herbaceous", taxa=="plants", succession=="no")%>%
  select(site_project_comm, calendar_year, species_matched, relcov, plot_id)%>%
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
grazing_names<-read.csv("GEx_species_family.csv")
grazing<-read.csv("GExforCoRREControlMS.csv")%>%
  mutate(site_project_comm = site, calendar_year = year,
         plot_id = paste (block, plot, sep = "_"))%>%
  select(site_project_comm, calendar_year, genus_species, relcov, plot_id)%>%
  mutate(relcov=relcov/100) %>% 
  filter(site_project_comm!="Germany2") %>% 
  rename(species_matched=genus_species)
#full_join(grazing_names) %>% 
#filter(relcov!=0) %>% 
#rename(species_matchedA=clean_ejf) %>% 
#mutate(species_matched= ifelse(species_matchedA=="NA", genus_species, species_matchedA))
#select(-genus_species, -tnrs_accepted_name, -family_ejf)

#write.csv(grazing, "findthesenames_forKev_May2023.csv")

unique(grazing$site_project_comm)
#splist_forAnnual<- as.data.frame(unique(grazing$genus_species))%>% 
#  mutate(species_matched= ifelse(species_matched=="NA", genus_species))
# select(-genus_species, -tnrs_accepted_name, -family_ejf)
#unique(grazing$site_project_comm)

data<-rbind(grazing, codyndat_subset, corredat_controls2) #%>% 
#right_join(datasetlength_timesteps) # droping sites with long timessteps that mean there are less than 7 time comparisons - this uses something created way below on line ~280 so you have to run later code to then be able to come back and drop it for the rate of directional change stuff

### SK note on 29 Nov 2023 - ok firs run this without the right join all the way through down to where you get the data, then come back and right join and get the datalist?

unique(data$site_project_comm)
YearLengthForTableS1<-data %>% 
  group_by(site_project_comm) %>% 
  select(calendar_year) %>% 
  unique() %>% 
  summarise(years=n()) 

#write.csv(data, "control_subset_data_May2023_v1.csv")

#this was something for Kevin to calc % annual
#data_forAnnual<- data %>% 
# filter(site_project_comm=="HAY_kansas_0"|
#          site_project_comm=="JRN_NPP_0"|
#          site_project_comm=="JSP_control_0"|
#          site_project_comm=="KNZ_00id_0"|
#           site_project_comm=="SEV_veg_B"|
#          site_project_comm=="SEV_veg_C"|
#           site_project_comm=="SEV_veg_G"|
#           site_project_comm=="SGS_UNUN_0"|
#           site_project_comm=="VAC_ebrp_0"|
##          site_project_comm=="FortKeogh"|
#           site_project_comm=="KLEE_wildlife"|
#           site_project_comm=="KRNP_Lammertjiesleegte"|
#          site_project_comm=="KRNP_Sandrivier")

#write.csv(data, "control_subset_dataforAnnulRelcov_May2023_v1.csv")
ungroup()

#splist_forAnnual<- as.data.frame(unique(data_forAnnual$species_matched))
### you need the later on code to run this
data_directionalchange<-rbind(grazing, codyndat_subset, corredat_controls2) %>% 
  right_join(datasetlength_timesteps)

#get data list
datalist<-data_directionalchange%>%
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
  summarise(exp=n())

####Figures
theme_set(theme_bw())
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=40, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=34, color='black'),
             axis.title.y=element_text(size=40, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=34, color='black'),
             plot.title = element_text(size=40, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))






#write.csv(data, "control_subset_data_May2023_v1.csv")

####FIGURE 1 - Show that things are changing directionally in the controls
#####look at directional change using all years of the data for all sites
spc<-unique(data_directionalchange$site_project_comm)
rt_change<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data_directionalchange%>%
    filter(site_project_comm==spc[i])
  
  out<-rate_change(subset, time.var = 'calendar_year', species.var = "species_matched", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  rt_change<-rbind(rt_change, out)
}
#write.csv(rt_change, 'rate_change_all_May2023_v1.csv')

###with each plot seperate - DONT use this because certain sites are way over-represented (some have 3, some have 50)
timelag<-ggplot(data=rt_change, aes(x=rate_change))+
  geom_density(aes(y=.0025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .0025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(rate_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(rate_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Rate of Directional Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none",
        plot.margin = margin(t = 5, r = 30, b = 20, l = 5, unit = "pt"))
#Export 1300*1000

####Average all plots within a site
rt_change2<-rt_change %>% 
  group_by(site_project_comm)%>%
  summarise(rate_change=mean(rate_change))

timelag2<-ggplot(data=rt_change2, aes(x=rate_change))+
  geom_density(aes(y=.0025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .0025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(rate_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(rate_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Rate of Directional Change", limits = c(-0.03, .175))+
  scale_y_continuous(name="Count", limits = c(0, 10))+
  theme(legend.position = "none",
        plot.margin = margin(t = 5, r = 30, b = 20, l = 5, unit = "pt"))
 timelag2


#####FIGURE 2 - Multchange and codyn metrics (all 7 panels together?)

#####look at mult change
spc<-unique(data$site_project_comm)
mult_change<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data%>%
    filter(site_project_comm==spc[i])
  
  out<-multivariate_change(subset, time.var = 'calendar_year', species.var = "species_matched", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  mult_change<-rbind(mult_change, out)
}
#write.csv(mult_change, 'Comm_change_all_May2023_v1.csv')

mult_change2<-mult_change %>% 
  mutate(timestep = calendar_year2-calendar_year) %>% 
  filter(timestep==1)

#RAC_Change
spc<-unique(data$site_project_comm)
RACs<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_change(subset, time.var = 'calendar_year', species.var = "species_matched", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  RACs<-rbind(RACs, out)
}

RACs2<-RACs %>% 
  group_by(calendar_year,calendar_year2,  site_project_comm)%>%
  summarise(richness_change=mean(richness_change),
            evenness_change=mean(evenness_change, na.rm=T),
            rank_change=mean(rank_change),
            gains=mean(gains),
            losses=mean(losses)) %>% 
  mutate(timestep = calendar_year2-calendar_year) %>% 
  filter(timestep==1)

#write.csv(RACs2, file="Control_RACs_AllYears_May2023_v1.csv", row.names=F)

RACs_subset_fixforLONGtimesteps<- RACs2 %>% 
  left_join(mult_change2) 

datasetlength_timesteps<-RACs_subset_fixforLONGtimesteps%>%
  select(site_project_comm, calendar_year)%>%
  unique()%>%
  group_by(site_project_comm)%>%
  summarise(length=n())%>%
  filter(length>6)%>%
  select(-length)

RACs_subset <- RACs2 %>% 
  left_join(mult_change) %>%
  right_join(datasetlength_timesteps) %>% 
  group_by(site_project_comm)%>%
  sample_n(7)
#write.csv(RACs_subset, file="Control_RACs_subsetDownTo7_toUSE_May2023_v1.csv", row.names=F)
#write.csv(RACs_subset, file="Control_RACs_subsetDownTo7_toUSE_May2023_v1_dropmutipletimepoints.csv", row.names=F)
#write.csv(RACs_subset, file="Control_RACs_subsetDownTo7_toUSE_Nov2023_v1_dropmutipletimesteps.csv", row.names=F)

#### now go back up and run lines 170- to get the first directional change figure and supp doc figure. 


#######
#######
#######
#######
#######
#######
#######
#######
#######
#######
#######START HERE - was having trouble, stats changed every time because it was a random subset of 7 each time
ToUse<-read.csv("Control_RACs_subsetDownTo7_toUSE_Nov2023_v1_dropmutipletimesteps.csv") 

##SIDE TANGENT TO GET SUPP FIG
#histogram of years included
YearsUsed<-ggplot(data=ToUse, aes(x=calendar_year))+
  geom_density(aes(y=5 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= 5, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(calendar_year, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(calendar_year, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Calendar Year")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

statsyears<-ToUse %>% 
  summarise(mean=mean(calendar_year))
#mean=2004.711
statsyears<-ToUse %>% 
  summarise(CI=confint(calendar_year, level=.95))
summary(ToUse)
sample_mean <- mean(ToUse$calendar_year)
sample_sd <- sd(ToUse$calendar_year)
margin <- qnorm(0.975) * (sample_sd / sqrt(length(ToUse$calendar_year)))
lower_bound <- sample_mean - margin
upper_bound <- sample_mean + margin


YearlyMetrics_subset<- ToUse 
q1SiteList<-unique(YearlyMetrics_subset$site)
#write.csv(q1SiteList, file="q1SiteList.csv", row.names=F)

KonzaData <- YearlyMetrics_subset %>% 
  separate(site_project_comm, into=c("site", "project", "comm"), sep = "_")%>% 
  filter(site == "KNZ") %>% 
  group_by(site) %>% 
  summarise(CC=mean(richness_change, na.rm=TRUE))


MeanMetrics_subset<-YearlyMetrics_subset %>% 
  group_by(site_project_comm) %>% 
  summarise(composition_change=mean(composition_change, na.rm=T), 
            dispersion_change=mean(dispersion_change, na.rm=T),
            richness_change=mean(richness_change),
            evenness_change=mean(evenness_change, na.rm=T),
            rank_change=mean(rank_change),
            gains=mean(gains),
            losses=mean(losses))

Means<-MeanMetrics_subset %>% 
  summarise(richness_change=mean(richness_change))
CIs<-MeanMetrics_subset %>% 
  summarise(CI=stderror(gains))

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



























####TABLE 1 - looking at how change varies with site level characteristics ####
###USE YearlyMetrics_subset and MeanMetrics_subset (which was created earlier)

#Join with yearly data
YearlyLevelData<- YearlyMetrics_subset%>% 
  left_join(corredat_sitebiotic)%>% 
  left_join((corredat_siteclimate))

unique(YearlyLevelData$site_project_comm)

Meta<-YearlyLevelData %>% 
  select(site_project_comm, anpp, MAP, MAT, rrich, annual_relcov) %>% 
  group_by(site_project_comm) %>% 
  summarise(ANPP=mean(anpp), MAP=mean(MAP), MAT=mean(MAT), rrich=mean(rrich), annual_relcov=mean(annual_relcov))

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
#write.csv(Meta4, file="Meta_May2023.csv", row.names=F)



#### calculating %annual for JRN from Codyn with the RAW data
WhatsInHere<-read.csv("~/Dropbox/C2E/Products/Control Paper/LTER_master_cleanVC.csv") 
unique(WhatsInHere$sitesubplot)
unique(WhatsInHere$duration)

JRNAnnual<-read.csv("~/Dropbox/C2E/Products/Control Paper/LTER_master_cleanVC.csv") %>% 
  filter(location=="JRN") %>% 
  filter(experiment=="NPP quadrats") %>% 
  group_by(sitesubplot, year, duration) %>% 
  summarise(abundance=sum(abundance)) %>% 
  ungroup() %>% 
  group_by(sitesubplot, year) %>% 
  filter(duration!="") %>% 
  pivot_wider(names_from = duration, values_from = abundance) %>% 
  mutate(total=sum(annual, perennial, biennial, na.rm=TRUE)) %>% 
  mutate(annual_relcov=annual/total) %>% 
  mutate(annual_relcov = replace_na(annual_relcov, 0)) %>% 
  ungroup() %>% 
  group_by(year) %>%
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE)) %>% 
  ungroup() %>% 
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE))

HAYAnnual<-read.csv("~/Dropbox/C2E/Products/Control Paper/LTER_master_cleanVC.csv") %>% 
  filter(location=="HAY") %>% 
  filter(experiment=="Kansas") %>% 
  group_by(sitesubplot, year, duration) %>% 
  summarise(abundance=sum(abundance)) %>% 
  ungroup() %>% 
  group_by(sitesubplot, year) %>% 
  filter(duration!="") %>% 
  pivot_wider(names_from = duration, values_from = abundance) %>% 
  mutate(total=sum(annual, perennial, na.rm=TRUE)) %>% 
  mutate(annual_relcov=annual/total) %>% 
  mutate(annual_relcov = replace_na(annual_relcov, 0)) %>% 
  ungroup() %>% 
  group_by(year) %>%
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE)) %>% 
  ungroup() %>% 
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE))

KNZAnnual<-read.csv("~/Dropbox/C2E/Products/Control Paper/LTER_master_cleanVC.csv") %>% 
  filter(location=="KNZ") %>% 
  filter(experiment=="001d") %>% 
  group_by(sitesubplot, year, duration) %>% 
  summarise(abundance=sum(abundance)) %>% 
  ungroup() %>% 
  group_by(sitesubplot, year) %>% 
  filter(duration!="") %>% 
  pivot_wider(names_from = duration, values_from = abundance) %>% 
  mutate(total=sum(annual, perennial, na.rm=TRUE)) %>% 
  mutate(annual_relcov=annual/total) %>% 
  mutate(annual_relcov = replace_na(annual_relcov, 0)) %>% 
  ungroup() %>% 
  group_by(year) %>%
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE)) %>% 
  ungroup() %>% 
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE))

JRGAnnual<-read.csv("~/Dropbox/C2E/Products/Control Paper/LTER_master_cleanVC.csv") %>% 
  filter(location=="JRG") %>% 
  filter(experiment=="Control") %>% 
  group_by(sitesubplot, year, duration) %>% 
  summarise(abundance=sum(abundance)) %>% 
  ungroup() %>% 
  group_by(sitesubplot, year) %>% 
  filter(duration!="") %>% 
  pivot_wider(names_from = duration, values_from = abundance) %>% 
  mutate(total=sum(annual, perennial, na.rm=TRUE)) %>% 
  mutate(annual_relcov=annual/total) %>% 
  mutate(annual_relcov = replace_na(annual_relcov, 0)) %>% 
  ungroup() %>% 
  group_by(year) %>%
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE)) %>% 
  ungroup() %>% 
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE))

SEVGAnnual<-read.csv("~/Dropbox/C2E/Products/Control Paper/LTER_master_cleanVC.csv") %>% 
  filter(location=="SEV") %>% 
  filter(experiment=="G") %>% 
  group_by(sitesubplot, year, duration) %>% 
  summarise(abundance=sum(abundance)) %>% 
  ungroup() %>% 
  group_by(sitesubplot, year) %>% 
  filter(duration!="") %>% 
  pivot_wider(names_from = duration, values_from = abundance) %>% 
  mutate(total=sum(annual, perennial, na.rm=TRUE)) %>% 
  mutate(annual_relcov=annual/total) %>% 
  mutate(annual_relcov = replace_na(annual_relcov, 0)) %>% 
  ungroup() %>% 
  group_by(year) %>%
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE)) %>% 
  ungroup() %>% 
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE))

SGSAnnual<-read.csv("~/Dropbox/C2E/Products/Control Paper/LTER_master_cleanVC.csv") %>% 
  filter(location=="SGS") %>% 
  filter(experiment=="UNUN") %>% 
  group_by(sitesubplot, year, duration) %>% 
  summarise(abundance=sum(abundance)) %>% 
  ungroup() %>% 
  group_by(sitesubplot, year) %>% 
  filter(duration!="") %>% 
  pivot_wider(names_from = duration, values_from = abundance) %>% 
  mutate(total=sum(annual, perennial, na.rm=TRUE)) %>% 
  mutate(annual_relcov=annual/total) %>% 
  mutate(annual_relcov = replace_na(annual_relcov, 0)) %>% 
  ungroup() %>% 
  group_by(year) %>%
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE)) %>% 
  ungroup() %>% 
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE))

SGSAnnual<-read.csv("~/Dropbox/C2E/Products/Control Paper/LTER_master_cleanVC.csv") %>% 
  filter(location=="SGS") %>% 
  filter(experiment=="UNUN") %>% 
  group_by(sitesubplot, year, duration) %>% 
  summarise(abundance=sum(abundance)) %>% 
  ungroup() %>% 
  group_by(sitesubplot, year) %>% 
  filter(duration!="") %>% 
  pivot_wider(names_from = duration, values_from = abundance) %>% 
  mutate(total=sum(annual, perennial, na.rm=TRUE)) %>% 
  mutate(annual_relcov=annual/total) %>% 
  mutate(annual_relcov = replace_na(annual_relcov, 0)) %>% 
  ungroup() %>% 
  group_by(year) %>%
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE)) %>% 
  ungroup() %>% 
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE))

VACAnnual<-read.csv("~/Dropbox/C2E/Products/Control Paper/LTER_master_cleanVC.csv") %>% 
  filter(location=="EBRP") %>% 
  filter(site=="VC") %>% 
  group_by(sitesubplot, year, duration) %>% 
  summarise(abundance=sum(abundance)) %>% 
  ungroup() %>% 
  group_by(sitesubplot, year) %>% 
  filter(duration!="") %>% 
  pivot_wider(names_from = duration, values_from = abundance) %>% 
  mutate(total=sum(annual, perennial, na.rm=TRUE)) %>% 
  mutate(annual_relcov=annual/total) %>% 
  mutate(annual_relcov = replace_na(annual_relcov, 0)) %>% 
  ungroup() %>% 
  group_by(year) %>%
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE)) %>% 
  ungroup() %>% 
  summarise(annual_relcov=mean(annual_relcov, na.rm=TRUE))

### export and add in annual for the sites needed by hand (all CODYN and GEx sites)
Meta5<-read.csv("~/Dropbox/C2E/Products/Control Paper/Meta_May2023_handfilledforcodyn.csv") %>% 
  mutate(annual_relcov=as.numeric(annual_relcov))
head(Meta5)

YearlyLevelData2<- YearlyMetrics_subset%>% 
  select(-timestep) %>% 
  left_join(Meta5)
unique(YearlyLevelData2$site_project_comm)

#Regressions
YearlyLevelDataLong<-YearlyLevelData2 %>% 
  select(-calendar_year, -calendar_year2)%>%
  gather(metric, value, richness_change:dispersion_change) %>% 
  group_by(site_project_comm, ANPP, MAP, MAT, rrich, annual_relcov, metric) %>% 
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
  left_join(ANPP_R) %>% 
  mutate(Independet="ANPP")

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
  left_join(ANPP_R)%>% 
  mutate(Independet="MAP")


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
  left_join(ANPP_R)%>% 
  mutate(Independet="MAT")


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
  left_join(ANPP_R)%>% 
  mutate(Independet="RRich")

#%annual
Regressions<-YearlyLevelDataLong %>% 
  nest(-metric) %>% 
  mutate(
    fit=map(data, ~lm(value~annual_relcov, data=.x)), 
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

Annual_RegStats<-ANPP_P%>%
  left_join(ANPP_R)%>% 
  mutate(Independet="Annual")

#write.csv(RRich_RegStats_Adjusted, file="RRich_RegStats_May2023_AdjustedP.csv", row.names=F)

PVals<-ANPP_RegStats %>% 
  rbind(MAP_RegStats) %>% 
  rbind(MAT_RegStats) %>% 
  rbind(RRich_RegStats) %>% 
  rbind(Annual_RegStats)
AdjustPVals<-PVals %>% 
  group_by(metric) %>% 
  mutate(padjust=p.adjust(p.value, method = "BH"))
write.csv(AdjustPVals, file="PValsAdjusted_Nov2023.csv", row.names=F)


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

















###### KEEPING ALL YEARS OF DATA
####graphs by experiment (avereaged yearly numbers together) --- need to get range into figure in upper right corner - wont work 
range<-paste("-1 to 1")

MeanMetrics_subset<-RACs2 %>%
  left_join(mult_change) %>%
  right_join(datasetlength_timesteps) %>% 
  group_by(site_project_comm) %>% 
  summarise(composition_change=mean(composition_change, na.rm=T), 
            dispersion_change=mean(dispersion_change, na.rm=T),
            richness_change=mean(richness_change),
            evenness_change=mean(evenness_change, na.rm=T),
            rank_change=mean(rank_change),
            gains=mean(gains),
            losses=mean(losses))

Means<-MeanMetrics_subset %>% 
  summarise(richness_change=mean(richness_change))
CIs<-MeanMetrics_subset %>% 
  summarise(CI=stderror(gains))

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



