##richness and evenness changes through time


library(codyn)
library(ggplot2)
theme_set(theme_bw(12))
library(devtools)
install_github("NCEAS/codyn", ref = "sp_diff_test")
library(tidyverse)
library(broom)

setwd("~/Dropbox/")

setwd("C:\\Users\\megha\\Dropbox\\")


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
  select(site_code, project_name, community_type, treatment,plot_mani, trt_type, pulse)%>%
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

###getting metrics for change in both control and trt plots.
corredat2<-corredat_raw%>%
  left_join(treatment_info)

spc<-unique(corredat2$site_project_comm)
change<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat2%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_change(subset, time.var = 'treatment_year', species.var="genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
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





#####################################################################################
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
  group_by(site_project_comm)%>%
  summarise(length=n())%>%
  filter(length>5)%>%
  select(-length)

corredat_ct2<-corredat_ct%>%
  right_join(datasetlength)%>%
  filter(treatment_year!=0)

###get mult_change
#####look at mult change
corredat_ct3<-corredat_ct2 %>% 
  filter(plot_mani==0) 

spc<-unique(corredat_ct3$spct)
mult_change<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_ct3%>%
    filter(spct==spc[i])
  
  out<-multivariate_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
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
  
  out<-multivariate_difference(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', treatment.var = "trt", reference.treatment = ref_trt)
  
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


#####Merger RACs with Mult and drop all but four timepoints for all
Metrics<-RACs%>%
  right_join(multivariate)%>%
  filter(treatment_year!="NA")%>%
  filter(treatment_year!=0) %>% 
  group_by(site_project_comm, trt, trt2)%>%
  sample_n(4) 
  
#### Metrics" IS THE DATA TO USE - Export it now, and then reimport it that way you can skip all the precvious steps. It takes a long time to run.
write.csv(Metrics,"C2E/Products/Control Paper/Output/CvT_Metrics_RACsMult_4timepoints_July2019.csv" , row.names=F)

#####################################################################################
##################START HERE NOW THAT THINGS ARE CALCULATED##########################
###################Control_Change vs Difference using 4 yrs only######################
#####################################################################################

metrics2<-read.csv("C2E/Products/Control Paper/Output/CvT_Metrics_RACsMult_4timepoints_July2019.csv")

subset_GCDs<-metrics2 %>% 
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
  mutate(sig=ifelse(p.value<0.05, 1, 0))
A<-ggplot(data=metrics3, aes(x=composition_change, y=composition_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(dispersion_change, abs_dispersion_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(dispersion_change, abs_dispersion_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
B<-ggplot(data=metrics3, aes(x=abs(dispersion_change), y=abs_dispersion_diff))+
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(richness_change, richness_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(richness_change, richness_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
C<-ggplot(data=metrics3, aes(x=abs(richness_change), y=abs(richness_diff)))+
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(evenness_change), abs(evenness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(evenness_change), abs(evenness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
D<-ggplot(data=metrics3, aes(x=abs(evenness_change), y=abs(evenness_diff)))+
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(rank_change, rank_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(rank_change, rank_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
E<-ggplot(data=metrics3, aes(x=rank_change, y=rank_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(gains, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(gains, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
G<-ggplot(data=metrics3, aes(x=gains, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(losses, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(losses, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
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
modB<- lm(data=metrics3, abs_dispersion_diff~dispersion_change)
summary(modB)
modC<- lm(data=metrics3, abs(richness_diff)~abs(richness_change))
summary(modC)
modD<- lm(data=metrics3, abs(evenness_diff)~abs(evenness_change))
summary(modD)
modE<- lm(data=metrics3, rank_diff~rank_change)
summary(modE)
modF<- lm(data=metrics3, species_diff~gains)
summary(modG)
modF<- lm(data=metrics3, species_diff~losses)
summary(modG)



### get only GCD plots and remove Temp

subset_GCDs<-metrics3 %>%
  rename(treatment=trt2) %>% 
  left_join(treatment_info)%>%
  filter(use==1)%>%
  filter(trt_type2!="Temperature")

rvalues <- subset_GCDs %>%
  summarize(r.value = round((cor.test(composition_change, composition_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(composition_change, composition_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
A<-ggplot(data=subset_GCDs, aes(x=composition_change, y=composition_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- subset_GCDs %>%
  summarize(r.value = round((cor.test(dispersion_change, abs_dispersion_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(dispersion_change, abs_dispersion_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
B<-ggplot(data=subset_GCDs, aes(x=abs(dispersion_change), y=abs_dispersion_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- subset_GCDs %>%
  summarize(r.value = round((cor.test(richness_change, richness_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(richness_change, richness_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
C<-ggplot(data=subset_GCDs, aes(x=abs(richness_change), y=abs(richness_diff)))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- subset_GCDs %>%
  summarize(r.value = round((cor.test(abs(evenness_change), abs(evenness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(evenness_change), abs(evenness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
D<-ggplot(data=subset_GCDs, aes(x=abs(evenness_change), y=abs(evenness_diff)))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- subset_GCDs %>%
  summarize(r.value = round((cor.test(rank_change, rank_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(rank_change, rank_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
E<-ggplot(data=subset_GCDs, aes(x=rank_change, y=rank_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- subset_GCDs %>%
  summarize(r.value = round((cor.test(gains, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(gains, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
G<-ggplot(data=subset_GCDs, aes(x=gains, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

rvalues <- subset_GCDs %>%
  summarize(r.value = round((cor.test(losses, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(losses, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))
H<-ggplot(data=subset_GCDs, aes(x=losses, y=species_diff))+
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



####################################################################################
#######################Restart here - this is something new)#########################
####################################################################################





#######doing the slope and mean differences for evenness and richness
div_info2<-div_info%>% 
  mutate(trt=ifelse(plot_mani==0, "C", div_info$treatment))%>%
  mutate(site_project_comm_trt=paste(site_project_comm, trt, sep="::"))

##subset to experiments with 5 more years of collected data
numyrs<-div_info2%>%
  select(site_project_comm, treatment_year)%>%
  unique()%>%
  filter(treatment_year!=0)%>%
  group_by(site_project_comm)%>%
  summarise(nmyrs=length(treatment_year))%>%
  filter(nmyrs>4)

div_info3<-div_info2%>%
  right_join(numyrs)

means<-div_info3%>%
  group_by(site_project_comm, trt)%>%
  summarize(even=mean(Evar, na.rm = T),
            rich = mean(richness))

cont_means<-means%>%
  filter(trt=="C")%>%
  rename(C_mean_rich=rich,
         C_mean_even=even) %>% 
  select(-trt)

ctmeans<-means%>%
  filter(trt!="C")%>%
  rename(T_mean_rich=rich,
         T_mean_even=even)%>%
  left_join(cont_means)

slopes<-data.frame()

spc<-unique(div_info3$site_project_comm_trt)

for (i in 1:length(spc)){
  
  subset<-div_info3%>%
    filter(site_project_comm_trt==spc[i])
  
  rich.lm<-lm(richness~treatment_year, data=subset)
  output.rich<-data.frame(site_project_comm_trt=unique(subset$site_project_comm_trt), 
                        rich_est=summary(rich.lm)$coef["treatment_year", c("Estimate")])
  
  even.lm<-lm(Evar~treatment_year, data=subset)
  output.even<-data.frame(site_project_comm_trt=unique(subset$site_project_comm_trt), 
                         even_est=summary(even.lm)$coef["treatment_year", c("Estimate")])%>%
    left_join(output.rich)
  
  slopes<-rbind(slopes, output.even)
}

slopes2<-slopes%>%
  separate(site_project_comm_trt, into=c("site_project_comm","trt"), sep = "::")
  

cslopes<-slopes2%>%
  filter(trt=="C")%>%
  rename(C_slope_rich=rich_est, 
         C_slope_even=even_est)%>%
  select(-trt)

CT_slopes<-slopes2%>%
  filter(trt!="C")%>%
  rename(T_slope_rich=rich_est, 
         T_slope_even=even_est)%>%
  left_join(cslopes)

  ####merge to one

treat_info<-treatment_info%>%
  select(site_project_comm, use, trt_type, trt_type2, treatment, pulse)%>%
  rename(trt=treatment)

CT_all<-ctmeans%>%
  left_join(CT_slopes)%>%
  left_join(treat_info)

write.csv(CT_all,"C2E/Products/Control Paper/Output/CT_compare_June2019.csv" )


####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
#reimport the data
dat<-read.csv("C2E/Products/Control Paper/Output/CT_compare_June2019.csv")%>%
  select(-X)%>%
  filter(pulse==0)

subset<-dat %>% 
  filter(use==1)


# Make all slope of control vs slope of treatment graphs
####RICH
#abs
modrichabs <- lm(abs(dat$T_slope_rich)~abs(dat$C_slope_rich))
summary(modrichabs)
rsq<-summary(modrichabs)$r.squared
pval<-summary(modrichabs)$coefficients[2,4]
lab<-paste("R^2 ==", round(rsq, 3))
lab2<-paste("p < 0.001")

ggplot(data=dat, aes(x=abs(C_slope_rich), y=abs(T_slope_rich)))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  annotate("text", x=0, y=3.65, hjust=0, label=lab, size=8, parse=TRUE)+
  annotate("text", x=0, y=3.2, hjust=0, label=lab2, size=8, parse=TRUE)+
  scale_x_continuous(name="\nSlope of richness through time of control plots\n(absolute value)")+
  scale_y_continuous(name="\nSlope of richness through time of treatment plots\n(absolute value)")

#not abs
modrich <- lm(dat$T_slope_rich~dat$C_slope_rich)
summary(modrich)
rsq<-summary(modrich)$r.squared
pval<-summary(modrich)$coefficients[2,4]
lab<-paste("R^2 ==", round(rsq, 3))
lab2<-paste("p < 0.001")

ggplot(data=dat, aes(x=(C_slope_rich), y=(T_slope_rich)))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  annotate("text", x=-1.8, y=3.5, hjust=0, label=lab, size=7, parse=TRUE)+
  annotate("text", x=-1.8, y=3, hjust=0, label=lab2, size=7, parse=TRUE)+
  scale_x_continuous(name="\nSlope of richness through time of control plots")+
  scale_y_continuous(name="\nSlope of richness through time of treatment plots")

####Diff Color line by trt
modrichabs <- lm(abs(subset$T_slope_rich)~abs(subset$C_slope_rich))
summary(modrichabs)
rsq<-summary(modrichabs)$r.squared
pval<-summary(modrichabs)$coefficients[2,4]
lab<-paste("R^2 ==", round(rsq, 3))
lab2<-paste("p < 0.001")

ggplot(data=subset, aes(x=abs(C_slope_rich), y=abs(T_slope_rich)))+
  geom_point()+
  geom_smooth(method="lm", se=F,  aes(color=trt_type2))+
  annotate("text", x=0, y=3.65, hjust=0, label=lab, size=6, parse=TRUE)+
  annotate("text", x=0, y=3.2, hjust=0, label=lab2, size=6, parse=TRUE)+
  scale_x_continuous(name="\nSlope of richness through time of control plots\n(absolute value)")+
  scale_y_continuous(name="\nSlope of richness through time of treatment plots\n(absolute value)")


modrich <- lm((subset$T_slope_rich)~(subset$C_slope_rich))
summary(modrich)
rsq<-summary(modrich)$r.squared
pval<-summary(modrich)$coefficients[2,4]
lab<-paste("R^2 ==", round(rsq, 3))
lab2<-paste("p < 0.001")
ggplot(data=subset, aes(x=(C_slope_rich), y=(T_slope_rich)))+
  geom_point()+
  geom_smooth(method="lm", se=F, aes(color=trt_type2))+
  annotate("text", x=-1.5, y=3.65, hjust=0, label=lab, size=8, parse=TRUE)+
  annotate("text", x=-1.5, y=3.2, hjust=0, label=lab2, size=8, parse=TRUE)+
  scale_x_continuous(name="\nSlope of richness through time of control plots")+
  scale_y_continuous(name="\nSlope of richness through time of treatment plots")



####Evenness
#abs
modevenabs <- lm(abs(dat$T_slope_even)~abs(dat$C_slope_even))
summary(modevenabs)
rsq<-summary(modevenabs)$r.squared
pval<-summary(modevenabs)$coefficients[2,4]
lab<-paste("R^2 ==", round(rsq, 3))
lab2<-paste("p < 0.001")

ggplot(data=dat, aes(x=abs(C_slope_even), y=abs(T_slope_even)))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  annotate("text", x=0, y=.05, hjust=0, label=lab, size=8, parse=TRUE)+
  annotate("text", x=0, y=.045, hjust=0, label=lab2, size=8, parse=TRUE)+
  scale_x_continuous(name="\nSlope of evenness through time of control plots\n(absolute value)")+
  scale_y_continuous(name="\nSlope of evenness through time of treatment plots\n(absolute value)")

#not abs
modeven <- lm(dat$T_slope_even~dat$C_slope_even)
summary(modeven)
rsq<-summary(modeven)$r.squared
pval<-summary(modeven)$coefficients[2,4]
lab<-paste("R^2 ==", round(rsq, 3))
lab2<-paste("p < 0.001")

ggplot(data=dat, aes(x=(C_slope_even), y=(T_slope_even)))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  annotate("text", x=-.030, y=.03, hjust=0, label=lab, size=7, parse=TRUE)+
  annotate("text", x=-.030, y=.02, hjust=0, label=lab2, size=7, parse=TRUE)+
  scale_x_continuous(name="\nSlope of evenness through time of control plots")+
  scale_y_continuous(name="\nSlope of evenness through time of treatment plots")

####Diff Color line by trt
modevenabs <- lm(abs(subset$T_slope_even)~abs(subset$C_slope_even))
summary(modevenabs)
rsq<-summary(modevenabs)$r.squared
pval<-summary(modevenabs)$coefficients[2,4]
lab<-paste("R^2 ==", round(rsq, 3))
lab2<-paste("p < 0.001")

ggplot(data=subset, aes(x=abs(C_slope_even), y=abs(T_slope_even)))+
  geom_point()+
  geom_smooth(method="lm", se=F,  aes(color=trt_type2))+
  annotate("text", x=0, y=.05, hjust=0, label=lab, size=8, parse=TRUE)+
  annotate("text", x=0, y=.045, hjust=0, label=lab2, size=8, parse=TRUE)+
  scale_x_continuous(name="\nSlope of evenness through time of control plots\n(absolute value)")+
  scale_y_continuous(name="\nSlope of evenness through time of treatment plots\n(absolute value)")


modeven <- lm((subset$T_slope_even)~(subset$C_slope_even))
summary(modeven)
rsq<-summary(modeven)$r.squared
pval<-summary(modeven)$coefficients[2,4]
lab<-paste("R^2 ==", round(rsq, 3))
lab2<-paste("p < 0.001")
ggplot(data=subset, aes(x=(C_slope_even), y=(T_slope_even)))+
  geom_point()+
  geom_smooth(method="lm", se=F, aes(color=trt_type2))+
  annotate("text", x=-.030, y=.03, hjust=0, label=lab, size=7, parse=TRUE)+
  annotate("text", x=-.030, y=.02, hjust=0, label=lab2, size=7, parse=TRUE)+
  scale_x_continuous(name="\nSlope of evenness through time of control plots")+
  scale_y_continuous(name="\nSlope of evenness through time of treatment plots")



#Make time ambient would take to equal treatment effect
dat2<-dat %>% 
  mutate(AtoTRich=abs((T_mean_rich-C_mean_rich)/C_slope_rich)) %>% 
  mutate(AtoTEven=abs((T_mean_even-C_mean_even)/C_slope_even)) %>%
  mutate(AttToTrtRich=T_slope_rich-C_slope_rich)%>%
  mutate(AttToTrtEven=T_slope_even-C_slope_even)

subset<-dat2 %>% 
  filter(use==1) %>% 
  filter(AtoTRich<100) %>% 
  filter(AtoTEven<100) %>% 
  select(site_project_comm, trt_type2, AtoTRich, AtoTEven, 
         AttToTrtRich, AttToTrtEven) %>% 
  gather(type, value,AtoTRich:AttToTrtEven)%>%
  group_by(trt_type2, type) %>% 
  summarise(mean_value=mean(value), sd_value=sd(value))%>%
  mutate(se_value=sd_value/sqrt(10))

ggplot(data=subset, aes(x=trt_type2, y=mean_value))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(~type, scales="free")+
  geom_errorbar(aes(ymin=mean_value-se_value, ymax=mean_value+se_value), position = position_dodge(0.9), width=0.2)


  
###question 2B  
  ### how do things vary by site level characteristics? CANNOT do until I get data that gives the ANPP, ect, Site Info
subset2<-subset %>% 
  mutate(TrtEffect=T_slope_rich-C_slope_rich)%>% 
  mutate(TimeCatch=abs(C_mean_rich-T_mean_rich)/C_slope_rich)

rvalues <- subset2 %>%
  summarize(r.value = round((cor.test(ANPP, TrtEffect)$estimate), digits=3),
            p.value = (cor.test(ANPP, TrtEffect)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))

ggplot(data=SiteLevelDataLong, aes(x=ANPP, y=value))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~metric, scales="free")+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)

