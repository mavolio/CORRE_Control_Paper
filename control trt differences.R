##richness and evenness changes through time
library(codyn)
library(ggplot2)
theme_set(theme_bw(12))+
  theme_update(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
library(devtools)
#install_github("NCEAS/codyn", ref = "sp_diff_test")
library(tidyverse)
library(broom)
library(codyn)
setwd("~/Dropbox/")

#setwd("C:\\Users\\megha\\Dropbox\\")

names<-read.csv("~/Dropbox/C2E/Products/Control Paper/FullList_Nov2021.csv")

corredat<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/community composition/old files/CoRRE_RelativeCover_Jan2023.csv") %>% 
  full_join(names) %>% 
  filter(relcov!="NA") %>% 
  group_by(site_code, project_name, community_type, calendar_year, treatment_year, treatment, block, plot_id, data_type, version, type, remove, species_matched) %>% 
  summarise(relcov=(mean(relcov))) %>% 
  filter(species_matched!="NA") %>%  ###MAY 10, 2023 --- this last line here is dropping 6 instances of an unknown species at KNZ that we cannot figure out. But maybe one day try adding them back in if kevin can figrue it out 
  ungroup()

#gvn face - only 2 years of data so will only have one point for the dataset, therefore we are removing this dataset from these analyses.
corredat1<-corredat%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0", site_project_comm!="AZI_NitPhos_0", site_project_comm!="JRN_study278_0", site_project_comm!="KNZ_GFP_4F", site_project_comm!="Saskatchewan_CCD_0", project_name!="e001", project_name!="e002", site_project_comm!="CHY_EDGE_0", site_project_comm!="SGS_EDGE_0", site_project_comm!="HYS_EDGE_0") %>% 
  mutate(plot_id= ifelse(project_name=="NSFC", paste(plot_id,treatment,sep="_"), plot_id))

##several studies only have two measurments of a plot. I am dropping those plots
azi<-corredat%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_code=="AZI", project_name!="EELplot")%>%
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
  filter(site_code=="CHY"|site_code=="SGS"|site_code=="HYS")%>%
  filter(project_name=="EDGE") %>% 
  filter(calendar_year!=2012)



###final dataset to use
corredat_raw<-rbind(corredat1, azi, jrn, knz, sak, cdr, edge)%>%
  mutate(spct=paste(site_project_comm, treatment, sep="::"))

treatment_info<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/community composition/old files/CoRRE_ExperimentInfo_Dec2021.csv")%>%
  select(site_code, project_name, community_type, treatment,plot_mani, trt_type, pulse, successional, nutrients, carbon, water)%>%
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


#ggplot(data=subset(div_info, site_project_comm=="KNZ_pplots_0"), aes(x=treatment_year, y=Evar, color=treatment))+
#  geom_point()+
#  geom_smooth(method="lm", se=F)

#ggplot(data=subset(div_info, site_project_comm=="SERC_CXN_0"), aes(x=treatment_year, y=Evar, color=treatment))+
#  geom_point()+
#  geom_smooth(method="lm", se=F)

###getting metrics for change in both control and trt plots.
corredat2<-corredat_raw%>%
  left_join(treatment_info) %>% 
  filter(pulse==0) %>% 
  filter(successional==0)


spc<-unique(corredat2$site_project_comm)
change<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat2%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_change(subset, time.var = 'treatment_year', species.var="species_matched", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  change<-rbind(change, out)
}

ave<-change %>% 
  group_by(site_project_comm, plot_id)%>%
  summarize_at(vars(richness_change, evenness_change, rank_change, gains, losses), list(mean), na.rm=T)%>%
  ungroup() %>% 
  group_by(site_project_comm) %>% 
  summarize_at(vars(richness_change, evenness_change, rank_change, gains, losses), list(mean), na.rm=T)


###comparing change in controls over time versus C-T differences over time.


controls<-corredat2%>%
  filter(plot_mani==0)

spc<-unique(controls$site_project_comm)
control_change<-data.frame()

for (i in 1:length(spc)){
  subset<-controls%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_change(subset, time.var = 'treatment_year', species.var="species_matched", abundance.var = 'relcov', replicate.var = 'plot_id')
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
  #filter(version==2) %>% 
  mutate(treatment2=as.character(treatment)) 
corredat_ct<-corredat_ct%>% 
  mutate(trt=ifelse(plot_mani==0, "C", corredat_ct$treatment2)) %>% 
  mutate(trt=replace(trt, spct=="JSP_GCE_0::C", "CO2")) %>% 
  ungroup()

spc<-unique(corredat_ct$site_project_comm)
ct_diff<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_ct%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_difference(subset, time.var = 'treatment_year', species.var="species_matched", abundance.var = 'relcov', replicate.var = 'plot_id', reference.treatment = "C", pool=T, treatment.var = "trt")
  
  out$site_project_comm<-spc[i]
  
  ct_diff<-rbind(ct_diff, out)
}

ct_ave<-ct_diff %>% 
  group_by(trt, trt2, site_project_comm) %>% 
  summarize_at(vars(richness_diff, evenness_diff, rank_diff, species_diff), list(mean), na.rm=T)

ct_cont_compare<-ct_ave%>%
  left_join(cont_ave)

#ggplot(data=ct_cont_compare, aes(x=abs(richness_change), y=abs(richness_diff)))+
#  geom_point()+
 # geom_smooth(method="lm", se=F)

#ggplot(data=ct_cont_compare, aes(x=abs(evenness_change), y=abs(evenness_diff)))+
#  geom_point()+
#  geom_smooth(method="lm", se=F)

#ggplot(data=ct_cont_compare, aes(x=rank_change, y=rank_diff))+
#  geom_point()+
#  geom_smooth(method="lm", se=F)

#ggplot(data=ct_cont_compare, aes(x=gains, y=species_diff))+
#  geom_point()+
#  geom_smooth(method="lm", se=F)

#ggplot(data=ct_cont_compare, aes(x=losses, y=species_diff))+
#  geom_point()+
#  geom_smooth(method="lm", se=F)





#####################################################################################This is what we use,but some of the code above is needed (like where we create control_change)
#####################################################################################
#####################################################################################
##################Control_Change vs Difference using 4 yrs only######################
#####################################################################################
#####################################################################################
#####################################################################################
#### drop sites with less than 5 years
datasetlength<-corredat_ct%>%
  select(site_project_comm, treatment_year)%>%
  unique()%>%
  filter(treatment_year!=0) %>% 
  group_by(site_project_comm)%>%
  summarise(length=n())%>%
  filter(length>4)%>%
  select(-length) 


####OVERALL using first ten year and SUPP figure using all years (but subsetting to random)
corredat_ct2<-corredat_ct%>%
  right_join(datasetlength)%>%
  filter(treatment_year!=0)

#### find full length of years
years<-corredat_ct2 %>% 
  ungroup() %>% 
  select(site_code, project_name, community_type, calendar_year) %>% 
  group_by(site_code, project_name, community_type) %>% 
  unique() %>% 
  summarise(years=length(calendar_year))

###get mult_change
#####look at mult change
corredat_ct3<-corredat_ct2 %>% 
  filter(plot_mani==0) 

spc<-unique(corredat_ct3$spct)
mult_change<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_ct3%>%
    filter(spct==spc[i])
  
  out<-multivariate_change(subset, time.var = 'treatment_year', species.var = "species_matched", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$spct<-spc[i]
  
  mult_change<-rbind(mult_change, out)
}


#####look at mult difference (need a double loop)
spc<-unique(corredat_ct2$site_project_comm)
diff_mult<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_ct2%>%
    filter(site_project_comm==spc[i])
  
  ref_trt <- unique(subset(subset, plot_mani==0)$trt)
  
  out<-multivariate_difference(subset, time.var = 'treatment_year', species.var = "species_matched", abundance.var = 'relcov', replicate.var = 'plot_id', treatment.var = "trt", reference.treatment = ref_trt)
  
  out$site_project_comm<-spc[i]
  
  diff_mult<-rbind(diff_mult, out)
}

diff_mult2<-diff_mult %>% 
  rename(treatment_year2=treatment_year)
mult_change2<-mult_change %>% 
  separate(spct, into=c("site_project_comm", "treatment"), sep="::") %>% 
  select(-treatment, -treatment_year)

multivariate<-mult_change2 %>% 
  right_join(diff_mult2) 


###use ct_diff
###use control_change, but average across plots
control_change2<-control_change %>% 
  group_by(site_project_comm, treatment_year, treatment_year2)%>%
  summarize_at(vars(richness_change, evenness_change, rank_change, gains, losses), list(mean), na.rm=T)%>%
  ungroup() 

RACs<-control_change2 %>% 
  right_join(ct_diff) %>% 
  right_join(datasetlength)

#get data list
datalist<-RACs%>%
  group_by(site_project_comm) %>% 
  select(treatment_year) %>% 
  unique() %>% 
  summarise(years=n())

#####Merger RACs with Mult and drop all but four timepoints for all
Metrics<-RACs%>%
  right_join(multivariate)%>%
  filter(treatment_year!="NA")%>%
  filter(treatment_year!=0) %>% 
  group_by(site_project_comm, trt, trt2)%>%
  mutate(timestep=treatment_year2-treatment_year) %>% 
  filter(timestep==1) %>%
  filter(site_project_comm!="ASGA_Exp1_a")%>%
  filter(treatment_year2<11) %>% 
  sample_n(4) 
 
#### Metrics == CvT_Metrics_RACsMult_4timepoints_July2019.csv IS THE DATA TO USE - Every time you run this and export it, it changes. ALSO somehow above 9 datasets are added in that are not supposed to be now. so the JULY2019 export is the correct export to use. 

#Export it now, and then reimport it that way you can skip all the precvious steps. It takes a long time to run.
#write.csv(Metrics,"C2E/Products/Control Paper/Output/CvT_Metrics_RACsMult_4timepoints_Oct2020_D.csv" , row.names=F)
write.csv(Metrics,"C2E/Products/Control Paper/Output/CvT_Metrics_RACsMult_4timepoints_May2023_10yrless_UGH.csv" , row.names=F)
#####################################################################################
##################START HERE NOW THAT THINGS ARE CALCULATED##########################
###################Control_Change vs Difference using 4 yrs only######################
#####################################################################################

metrics2<-read.csv("C2E/Products/Control Paper/Output/CvT_Metrics_RACsMult_4timepoints_May2023_10yrless_Ugh.csv")
#metrics2<-Metrics
###list
list<-  as.data.frame(unique(metrics2$site_project_comm))
#write.csv(list, "C2E/Products/Control Paper/Output/listofQ2sites_May2023_5ormoreyrs.csv")

#### find number of sites, numbers of experiments, and number of control trt comparisons
site<-metrics2 %>% 
  separate(site_project_comm, into=c("site", "project", "comm"), sep="_") %>% 
  select(site) %>% 
  unique()

exp<-metrics2 %>% 
  select(site_project_comm) %>% 
  unique()

comparisons<-metrics2 %>% 
  separate(site_project_comm, into=c("site", "project", "comm"), sep="_") %>%
  select(site, project, comm, trt2) %>% 
  unique()

#check years above before subsetting to random years

metrics3<-metrics2 %>% 
  group_by(site_project_comm,trt2)%>%
  summarise(richness_change=mean(richness_change, na.rm=T),
            richness_diff=mean(richness_diff, na.rm=T),
            evenness_change=mean(evenness_change, na.rm=T),
            evenness_diff=mean(evenness_diff, na.rm=T),
            rank_change=mean(rank_change, na.rm=T),
            rank_diff=mean(rank_diff, na.rm=T),
            gains=mean(gains, na.rm=T),
            losses=mean(losses, na.rm=T),
            species_diff=mean(species_diff, na.rm=T),
            composition_change=mean(composition_change, na.rm=T),
            composition_diff=mean(composition_diff, na.rm=T),
            dispersion_change=mean(dispersion_change, na.rm=T),
            abs_dispersion_diff=mean(abs_dispersion_diff, na.rm=T))%>%
  ungroup()

### get only GCD plots


rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(composition_change, composition_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(composition_change, composition_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0)) %>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

A<-ggplot(data=metrics3, aes(x=composition_change, y=composition_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)


rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(dispersion_change), abs_dispersion_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(abs(dispersion_change), abs_dispersion_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

B<-ggplot(data=metrics3, aes(x=abs(dispersion_change), y=abs_dispersion_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(richness_change), abs(richness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(richness_change), abs(richness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

C<-ggplot(data=metrics3, aes(x=abs(richness_change), y=abs(richness_diff)))+
  geom_point()+
 # geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(evenness_change), abs(evenness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(evenness_change), abs(evenness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

D<-ggplot(data=metrics3, aes(x=abs(evenness_change), y=abs(evenness_diff)))+
  geom_point()+
 # geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(rank_change, rank_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(rank_change, rank_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

E<-ggplot(data=metrics3, aes(x=rank_change, y=rank_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(gains, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(gains, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

G<-ggplot(data=metrics3, aes(x=gains, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(losses, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(losses, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

H<-ggplot(data=metrics3, aes(x=losses, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)



library(grid)
pushViewport(viewport(layout=grid.layout(2,4)))
print(CvCT, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(A, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(B, vp=viewport(layout.pos.row = 1, layout.pos.col = 2))
print(C, vp=viewport(layout.pos.row = 1, layout.pos.col = 3))
print(D, vp=viewport(layout.pos.row = 1, layout.pos.col = 4))
print(E, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(G, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(H, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))
print(I, vp=viewport(layout.pos.row = 2, layout.pos.col = 4))

modA<- lm(data=metrics3, composition_diff~composition_change)
summary(modA)
modB<- lm(data=metrics3, abs_dispersion_diff~abs(dispersion_change))
summary(modB)
modC<- lm(data=metrics3, abs(richness_diff)~abs(richness_change))
summary(modC)
modD<- lm(data=metrics3, abs(evenness_diff)~abs(evenness_change))
summary(modD)
modE<- lm(data=metrics3, rank_diff~rank_change)
summary(modE)
modF<- lm(data=metrics3, species_diff~gains)
summary(modF)
modG<- lm(data=metrics3, species_diff~losses)
summary(modG)





####Nutrients Only using only the first ten years of the experiments
corredat_ct2<-corredat_ct%>%
  right_join(datasetlength)%>%
  filter(treatment_year!=0)%>% 
  filter(nutrients==1) %>% 
  filter(trt_type=="N"|trt_type=="P"|trt_type=="N*P"|trt_type=="mult_nutrient"|trt_type=="control") %>% 
  filter(site_project_comm!="SIU_TON_0")

#### find full length of dataset

years<-corredat_ct2 %>% 
  ungroup() %>% 
  select(site_code, project_name, community_type, calendar_year) %>% 
  group_by(site_code, project_name, community_type) %>% 
  unique() %>% 
  summarise(years=length(calendar_year))

###get mult_change
#####look at mult change
corredat_ct3<-corredat_ct2 %>% 
  filter(plot_mani==0) 

spc<-unique(corredat_ct3$spct)
mult_change<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_ct3%>%
    filter(spct==spc[i])
  
  out<-multivariate_change(subset, time.var = 'treatment_year', species.var = "species_matched", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$spct<-spc[i]
  
  mult_change<-rbind(mult_change, out)
}


#####look at mult difference (need a double loop)
spc<-unique(corredat_ct2$site_project_comm)
diff_mult<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_ct2%>%
    filter(site_project_comm==spc[i])
  
  ref_trt <- unique(subset(subset, plot_mani==0)$trt)
  
  out<-multivariate_difference(subset, time.var = 'treatment_year', species.var = "species_matched", abundance.var = 'relcov', replicate.var = 'plot_id', treatment.var = "trt", reference.treatment = ref_trt)
  
  out$site_project_comm<-spc[i]
  
  diff_mult<-rbind(diff_mult, out)
}

diff_mult2<-diff_mult %>% 
  rename(treatment_year2=treatment_year)
mult_change2<-mult_change %>% 
  separate(spct, into=c("site_project_comm", "treatment"), sep="::") %>% 
  select(-treatment, -treatment_year)

multivariate<-mult_change2 %>% 
  right_join(diff_mult2) 


###use ct_diff
###use control_change, but average across plots
control_change2<-control_change %>% 
  group_by(site_project_comm, treatment_year, treatment_year2)%>%
  summarize_at(vars(richness_change, evenness_change, rank_change, gains, losses), list(mean), na.rm=T)%>%
  ungroup() 

RACs<-control_change2 %>% 
  right_join(ct_diff) %>% 
  right_join(datasetlength)

#get data list
datalist<-RACs%>%
  group_by(site_project_comm) %>% 
  select(treatment_year) %>% 
  unique() %>% 
  summarise(years=n())

#####Merger RACs with Mult and drop all but four timepoints for all
Metrics<-RACs%>%
  right_join(multivariate)%>%
  filter(treatment_year!="NA")%>%
  filter(treatment_year!=0) %>% 
  group_by(site_project_comm, trt, trt2)%>%
  mutate(timestep=treatment_year2-treatment_year) %>% 
  filter(timestep==1) %>%
  filter(site_project_comm!="ASGA_Exp1_a")%>%
  filter(treatment_year2<11) %>% 
  sample_n(4) 


#### Metrics == CvT_Metrics_RACsMult_4timepoints_July2019.csv IS THE DATA TO USE - Every time you run this and export it, it changes. ALSO somehow above 9 datasets are added in that are not supposed to be now. so the JULY2019 export is the correct export to use. 

#Export it now, and then reimport it that way you can skip all the precvious steps. It takes a long time to run.
#write.csv(Metrics,"C2E/Products/Control Paper/Output/CvT_Metrics_RACsMult_4timepoints_Oct2020_D.csv" , row.names=F)
write.csv(Metrics,"C2E/Products/Control Paper/Output/CvT_Metrics_RACsMult_4timepoints_May2023_nutonly_10yrless.csv" , row.names=F)
#####################################################################################
##################START HERE NOW THAT THINGS ARE CALCULATED##########################
###################Control_Change vs Difference using 4 yrs only######################
#####################################################################################

metrics2<-read.csv("C2E/Products/Control Paper/Output/CvT_Metrics_RACsMult_4timepoints_May2023_nutonly_10yrless.csv")

###list
list<-  as.data.frame(unique(metrics2$site_project_comm))
write.csv(list, "C2E/Products/Control Paper/Output/listofQ2sites_May2023_5ormoreyrs_nutonly.csv")

#### find number of sites, numbers of experiments, and number of control trt comparisons
site<-metrics2 %>% 
  separate(site_project_comm, into=c("site", "project", "comm"), sep="_") %>% 
  select(site) %>% 
  unique()

exp<-metrics2 %>% 
  select(site_project_comm) %>% 
  unique()

comparisons<-metrics2 %>% 
  separate(site_project_comm, into=c("site", "project", "comm"), sep="_") %>%
  select(site, project, comm, trt2) %>% 
  unique()


metrics3<-metrics2 %>% 
  group_by(site_project_comm,trt2)%>%
  summarise(richness_change=mean(richness_change, na.rm=T),
            richness_diff=mean(richness_diff, na.rm=T),
            evenness_change=mean(evenness_change, na.rm=T),
            evenness_diff=mean(evenness_diff, na.rm=T),
            rank_change=mean(rank_change, na.rm=T),
            rank_diff=mean(rank_diff, na.rm=T),
            gains=mean(gains, na.rm=T),
            losses=mean(losses, na.rm=T),
            species_diff=mean(species_diff, na.rm=T),
            composition_change=mean(composition_change, na.rm=T),
            composition_diff=mean(composition_diff, na.rm=T),
            dispersion_change=mean(dispersion_change, na.rm=T),
            abs_dispersion_diff=mean(abs_dispersion_diff, na.rm=T))%>%
  ungroup()

### get only GCD plots


rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(composition_change, composition_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(composition_change, composition_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0)) %>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

A<-ggplot(data=metrics3, aes(x=composition_change, y=composition_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)


rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(dispersion_change), abs_dispersion_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(abs(dispersion_change), abs_dispersion_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

B<-ggplot(data=metrics3, aes(x=abs(dispersion_change), y=abs_dispersion_diff))+
  geom_point()+
 # geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(richness_change), abs(richness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(richness_change), abs(richness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

C<-ggplot(data=metrics3, aes(x=abs(richness_change), y=abs(richness_diff)))+
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(evenness_change), abs(evenness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(evenness_change), abs(evenness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

D<-ggplot(data=metrics3, aes(x=abs(evenness_change), y=abs(evenness_diff)))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(rank_change, rank_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(rank_change, rank_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

E<-ggplot(data=metrics3, aes(x=rank_change, y=rank_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(gains, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(gains, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

G<-ggplot(data=metrics3, aes(x=gains, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(losses, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(losses, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

H<-ggplot(data=metrics3, aes(x=losses, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)


library(grid)
pushViewport(viewport(layout=grid.layout(2,4)))
print(CvCT, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(A, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(B, vp=viewport(layout.pos.row = 1, layout.pos.col = 2))
print(C, vp=viewport(layout.pos.row = 1, layout.pos.col = 3))
print(D, vp=viewport(layout.pos.row = 1, layout.pos.col = 4))
print(E, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(G, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(H, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))
print(I, vp=viewport(layout.pos.row = 2, layout.pos.col = 4))



















#####
#####
#####
#####
#####
#####
#####
#####
#####Including all available years of data
Metrics2b<-RACs%>%
  right_join(multivariate)%>%
  filter(treatment_year!="NA")%>%
  filter(treatment_year!=0) %>% 
  group_by(site_project_comm, trt, trt2)%>%
  mutate(timestep=treatment_year2-treatment_year) %>% 
  filter(timestep==1) %>%
  filter(site_project_comm!="ASGA_Exp1_a")

metrics2b<-Metrics2b

list<-  as.data.frame(unique(metrics2b$site_project_comm))

#### find number of sites, numbers of experiments, and number of control trt comparisons
site<-metrics2b %>% 
  separate(site_project_comm, into=c("site", "project", "comm"), sep="_") %>% 
  select(site) %>% 
  unique()

exp<-metrics2b %>% 
  select(site_project_comm) %>% 
  unique()

comparisons<-metrics2b %>% 
  separate(site_project_comm, into=c("site", "project", "comm"), sep="_") %>%
  select(site, project, comm, trt2) %>% 
  unique()

#check years above before subsetting to random years

metrics3<-metrics2b %>% 
  group_by(site_project_comm,trt2)%>%
  summarise(richness_change=mean(richness_change, na.rm=T),
            richness_diff=mean(richness_diff, na.rm=T),
            evenness_change=mean(evenness_change, na.rm=T),
            evenness_diff=mean(evenness_diff, na.rm=T),
            rank_change=mean(rank_change, na.rm=T),
            rank_diff=mean(rank_diff, na.rm=T),
            gains=mean(gains, na.rm=T),
            losses=mean(losses, na.rm=T),
            species_diff=mean(species_diff, na.rm=T),
            composition_change=mean(composition_change, na.rm=T),
            composition_diff=mean(composition_diff, na.rm=T),
            dispersion_change=mean(dispersion_change, na.rm=T),
            abs_dispersion_diff=mean(abs_dispersion_diff, na.rm=T))%>%
  ungroup()

### get only GCD plots


rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(composition_change, composition_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(composition_change, composition_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0)) %>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

A<-ggplot(data=metrics3, aes(x=composition_change, y=composition_diff))+
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)


rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(dispersion_change), abs_dispersion_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(abs(dispersion_change), abs_dispersion_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

B<-ggplot(data=metrics3, aes(x=abs(dispersion_change), y=abs_dispersion_diff))+
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(richness_change), abs(richness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(richness_change), abs(richness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

C<-ggplot(data=metrics3, aes(x=abs(richness_change), y=abs(richness_diff)))+
  geom_point()+
 # geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(evenness_change), abs(evenness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(evenness_change), abs(evenness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

D<-ggplot(data=metrics3, aes(x=abs(evenness_change), y=abs(evenness_diff)))+
  geom_point()+
   geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(rank_change, rank_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(rank_change, rank_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

E<-ggplot(data=metrics3, aes(x=rank_change, y=rank_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.1, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(gains, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(gains, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

G<-ggplot(data=metrics3, aes(x=gains, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.1, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(losses, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(losses, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

H<-ggplot(data=metrics3, aes(x=losses, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)



library(grid)
pushViewport(viewport(layout=grid.layout(2,4)))
print(CvCT, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(A, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(B, vp=viewport(layout.pos.row = 1, layout.pos.col = 2))
print(C, vp=viewport(layout.pos.row = 1, layout.pos.col = 3))
print(D, vp=viewport(layout.pos.row = 1, layout.pos.col = 4))
print(E, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(G, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(H, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))
print(I, vp=viewport(layout.pos.row = 2, layout.pos.col = 4))

modA<- lm(data=metrics3, composition_diff~composition_change)
summary(modA)
modB<- lm(data=metrics3, abs_dispersion_diff~abs(dispersion_change))
summary(modB)
modC<- lm(data=metrics3, abs(richness_diff)~abs(richness_change))
summary(modC)
modD<- lm(data=metrics3, abs(evenness_diff)~abs(evenness_change))
summary(modD)
modE<- lm(data=metrics3, rank_diff~rank_change)
summary(modE)
modF<- lm(data=metrics3, species_diff~gains)
summary(modF)
modG<- lm(data=metrics3, species_diff~losses)
summary(modG)


#####
#####
#####
#####
#####
#####
#####
##### Make Figure S1 with this trt-contorl data - seperate for trt and control
####FIGURE S1 - Show that things are NOT changing directionally in the controls but they are for the trt plots
####use all years of the data for all sites
data_directionalchange_alldata<-corredat_ct2 %>% ### this is the relativized sp comp data of only the experiments used in analysis 2
  filter(site_project_comm!="ASGA_Exp1_a") %>% 
  mutate(dropplots=paste(spct, plot_id, sep="_")) %>% 
  filter(plot_id!='CR-FA-1') %>% 
  filter(plot_id!='CR-FA-2') %>% 
  filter(plot_id!='CR-FA3') %>% 
  filter(plot_id!='CR-FA-4') %>% 
  filter(plot_id!='CR-FA-5') %>% 
  filter(plot_id!='NR-FA-1') %>% 
  filter(plot_id!='NR-FA-2') %>% 
  filter(plot_id!='NR-FA-3') %>% 
  filter(plot_id!='NR-FA-4') %>% 
  filter(plot_id!='NR-FA-5') %>% 
  filter(dropplots!="Naiman_Nprecip_0::CK_W1_209_3" )%>% 
  filter(dropplots!="Naiman_Nprecip_0::CK_W2_209_2" )

error<- data_directionalchange_alldata %>% 
  filter(spct=="Naiman_Nprecip_0::CK_W2") %>% 
  group_by(spct, plot_id, calendar_year) %>% 
  summarise(sum=n()) %>% 
  group_by(spct, plot_id) %>% 
  summarise(sum=n())

spc<-unique(data_directionalchange_alldata$spct)
rt_change<-data.frame()

for (i in 1:length(spc)){
  
  subset<-data_directionalchange_alldata%>%
    filter(spct==spc[i])
  
  out<-rate_change(subset, time.var = 'calendar_year', species.var = "species_matched", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$spct<-spc[i]
  
  rt_change<-rbind(rt_change, out)
}

treatment_info_2<-treatment_info %>% 
  mutate(spct=paste(site_project_comm, treatment, sep="::"))

rt_change_controls<-rt_change %>% 
  left_join(treatment_info_2) %>% 
  filter(plot_mani==0)

rt_change_treatments<-rt_change %>% 
  left_join(treatment_info_2) %>% 
  filter(plot_mani!=0)

theme_set(theme_bw())
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=40, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=34, color='black'),
             axis.title.y=element_text(size=40, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=34, color='black'),
             plot.title = element_text(size=40, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))


###with each plot seperate - DONT use this because certain sites are way over-represented (some have 3, some have 50)
control<-ggplot(data=rt_change_controls, aes(x=rate_change))+
  geom_density(aes(y=.0025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .0025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(rate_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(rate_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Rate of Directional Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")

treatments<-ggplot(data=rt_change_treatments, aes(x=rate_change))+
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
rt_change_controls2<-rt_change_controls %>% 
  group_by(spct)%>%
  summarise(rate_change=mean(rate_change))

controls_average<-ggplot(data=rt_change_controls2, aes(x=rate_change))+
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
controls_average

rt_change_treatments2<-rt_change_treatments %>% 
  group_by(spct)%>%
  summarise(rate_change=mean(rate_change))

treatments_average<-ggplot(data=rt_change_treatments2, aes(x=rate_change))+
  geom_density(aes(y=.0025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .0025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(rate_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(rate_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Rate of Directional Change", limits = c(-0.03, .175))+
  scale_y_continuous(name="Count", limits = c(0, 25))+
  theme(legend.position = "none",
        plot.margin = margin(t = 5, r = 30, b = 20, l = 5, unit = "pt"))
treatments_average

###need to run timelag from other code "datasets_analyses_v2" in order to get panel 1
library(grid)
pushViewport(viewport(layout=grid.layout(3,1)))
print(timelag2, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(controls_average, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(treatments_average, vp=viewport(layout.pos.row = 3, layout.pos.col = 1))

#export at 800x2000