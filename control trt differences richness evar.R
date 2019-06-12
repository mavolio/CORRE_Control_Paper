##richness changes through time

library(tidyverse)
library(codyn)

theme_set(theme_bw(12))

setwd("~/Dropbox/")

corredat<-read.csv("converge_diverge/datasets/LongForm/SpeciesRelativeAbundance_March2019.csv")%>%
  select(-X)

#gvn face - only 2 years of data so will only have one point for the dataset, therefore we are removing this dataset from these analyses.
corredat1<-corredat%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0", site_project_comm!="AZI_NitPhos_0", site_project_comm!="JRN_study278_0", site_project_comm!="KNZ_GFP_4F", site_project_comm!="Saskatchewan_CCD_0", project_name!="e001", project_name!="e002", site_project_comm!="CHY_EDGE_0", site_project_comm!="SGS_EDGE_0", site_project_comm!="HYS_EDGE_0")

##several studies only have two measurments of a plot. I am dropping those plots
azi<-corredat%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_code=="AZI")%>%
  filter(plot_id!=11&plot_id!=15&plot_id!=35&plot_id!=37)

jrn<-corredat%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm=="JRN_study278_0")%>%
  filter(plot_id!=211&plot_id!=210)

knz<-corredat%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm=="KNZ_GFP_4F")%>%
  filter(plot_id!="7_1_1"&plot_id!="7_2_1")

sak<-corredat%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm=="Saskatchewan_CCD_0")%>%
  filter(plot_id!=2)

###remove extra treatments from CDR e001 and e002
cdr <- corredat%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(project_name=="e001"|project_name=="e002")%>%
  filter(treatment==1|treatment==6|treatment==8|treatment==9|treatment=='1_f_u_n'|treatment=='6_f_u_n'|treatment=='8_f_u_n'|treatment=='9_f_u_n')

##remove one of 2 pre-treatment years in edge for CHY, SGS, and HAYS
edge<-corredat%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_code=="CHY"|site_code=="SGS"|site_code=="HYS"&project_name=="EDGE")%>%
  filter(calendar_year!=2012)



###final dataset to use
corredat_raw<-rbind(corredat1, azi, jrn, knz, sak, cdr, edge)%>%
  mutate(spct=paste(site_project_comm, treatment, sep="::"))

treatment_info<-read.csv("converge_diverge/datasets/LongForm/ExperimentInformation_March2019.csv")%>%
  select(site_code, project_name, community_type, treatment,plot_mani, trt_type)%>%
  unique()%>%
  #filter(plot_mani!=0)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  mutate(use=ifelse(trt_type=="N"|trt_type=="P"|trt_type=="CO2"|trt_type=="irr"|trt_type=="temp"|trt_type=="N*P"|trt_type=="mult_nutrient"|trt_type=='precip_vari', 1, 0))%>%
  mutate(trt_type2=ifelse(trt_type=="N"|trt_type=="control","N", 
                          ifelse(trt_type=="P", "P", 
                                 ifelse(trt_type=="CO2", "CO2",
                                        ifelse(trt_type=="irr", "Irrigation",
                                               ifelse(trt_type=="temp", "Temperature", 
                                                      ifelse(trt_type=="N*P"|trt_type=="mult_nutrient", "Mult. Nuts.", 
                                                             ifelse(trt_type=="drought", "drought", 
                                                                    ifelse(trt_type=="CO2*temp", "CO2*temp", 
                                                                           ifelse(trt_type=="drought*temp", "drought*temp", 
                                                                                  ifelse(trt_type=="irr*temp", "irr*temp",
                                                                                         ifelse(trt_type=="irr*CO2*temp"|trt_type=="N*CO2*temp"|trt_type=="N*irr*temp"|trt_type=="N*irr*CO2*temp", "mult_res*temp", 
                                                                                                ifelse(trt_type=="irr*herb_removal"|trt_type=="irr*plant_mani"|trt_type=="irr*plant_mani*herb_removal", "irr*NR", 
                                                                                                       ifelse(trt_type=="herb_removal"|trt_type=="till"|trt_type=="mow_clip"|trt_type=="burn"|trt_type=="plant_mani"|trt_type=="stone"|trt_type=="graze"|trt_type=="burn*graze"|trt_type=="fungicide"|trt_type=="plant_mani*herb_removal"|trt_type=="burn*mow_clip", "NR", 
                                                                                                              ifelse(trt_type=="precip_vari", "Precip. Vari.",  
                                                                                                                     ifelse(trt_type=="N*plant_mani"|trt_type=="N*burn"|trt_type=="N*mow_clip"|trt_type=="N*till"|trt_type=="N*stone"|trt_type=="N*burn*graze"|trt_type=="N*burn*mow_clip", "N*NR", 
                                                                                                                            ifelse(trt_type=="N*temp", "N*temp", 
                                                                                                                                   ifelse(trt_type=="N*CO2", "N*CO2",
                                                                                                                                          ifelse(trt_type=="irr*CO2", "irr*CO2",
                                                                                                                                                 ifelse(trt_type=="N*irr", "N*irr",
                                                                                                                                                        ifelse(trt_type=="mult_nutrient*herb_removal"|trt_type=="mult_nutrient*fungicide"|trt_type=="N*P*burn*graze"|trt_type=="N*P*burn"|trt_type=="*P*mow_clip"|trt_type=="N*P*burn*mow_clip"|trt_type=="N*P*mow_clip", "mult_nutrients*NR",
                                                                                                                                                               ifelse(trt_type=="P*mow_clip"|trt_type=="P*burn"|trt_type=="P*burn*graze"|trt_type=="P*burn*mow_clip", "P*NR", 
                                                                                                                                                                      ifelse(trt_type=="precip_vari*temp", "precip_vari*temp", 
                                                                                                                                                                             ifelse(trt_type=="N*irr*CO2", "mult_res", 999))))))))))))))))))))))))


spct_id<-unique(corredat_raw$spct)
div_evar<-data.frame()

for (i in 1:length(spct_id)){
  subset<-corredat_raw%>%
    filter(spct==spct_id[i])
  
  out<-community_structure(subset, time.var = 'treatment_year', abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spct_id[i]
  
  div_evar<-rbind(div_evar, out)
}


div_info<-div_evar %>% 
  separate(site_project_comm, into=c("site_project_comm", "treatment"), sep="::") %>% 
  left_join(treatment_info)


ggplot(data=subset(div_info, site_project_comm=="KNZ_pplots_0"), aes(x=treatment_year, y=Evar, color=treatment))+
  geom_point()+
  geom_smooth(method="lm", se=F)

ggplot(data=subset(div_info, site_project_comm=="SERC_CXN_0"), aes(x=treatment_year, y=Evar, color=treatment))+
  geom_point()+
  geom_smooth(method="lm", se=F)


###comparing change in controls over time versus C-T differences over time.
corredat2<-corredat_raw%>%
  left_join(treatment_info)

controls<-corredat2%>%
  filter(plot_mani==0)

spc<-unique(controls$site_project_comm)
control_change<-data.frame()

for (i in 1:length(spc)){
  subset<-controls%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_change(subset, time.var = 'treatment_year', species.var="genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  control_change<-rbind(control_change, out)
}

cont_ave<-control_change %>% 
  group_by(site_project_comm, plot_id)%>%
  summarize_at(vars(richness_change, evenness_change, rank_change, gains, losses), list(mean), na.rm=T)%>%
  ungroup() %>% 
  group_by(site_project_comm) %>% 
  summarize_at(vars(richness_change, evenness_change, rank_change, gains, losses), list(mean), na.rm=T)

###looking at C-T differences over time
corredat_ct<-corredat2%>%
  mutate(treatment2=as.character(treatment)) 
corredat_ct<-corredat_ct%>% 
  mutate(trt=ifelse(plot_mani==0, "C", corredat_ct$treatment2))

spc<-unique(corredat_ct$site_project_comm)
ct_diff<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_ct%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_difference(subset, time.var = 'treatment_year', species.var="genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', reference.treatment = "C", pool=T, treatment.var = "trt")
  
  out$site_project_comm<-spc[i]
  
  ct_diff<-rbind(ct_diff, out)
}

ct_ave<-ct_diff %>% 
  group_by(trt, trt2, site_project_comm) %>% 
  summarize_at(vars(richness_diff, evenness_diff, rank_diff, species_diff), list(mean), na.rm=T)

ct_cont_compare<-ct_ave%>%
  left_join(cont_ave)

ggplot(data=ct_cont_compare, aes(x=abs(richness_change), y=abs(richness_diff)))+
  geom_point()+
  geom_smooth(method="lm", se=F)

ggplot(data=ct_cont_compare, aes(x=abs(evenness_change), y=abs(evenness_diff)))+
  geom_point()+
  geom_smooth(method="lm", se=F)

ggplot(data=ct_cont_compare, aes(x=rank_change, y=rank_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)

ggplot(data=ct_cont_compare, aes(x=gains, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)

ggplot(data=ct_cont_compare, aes(x=losses, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)

