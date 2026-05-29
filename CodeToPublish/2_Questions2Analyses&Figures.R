#All Code needed for Question 2 Analyses

## Sally's 
setwd("~/Dropbox/C2E/Products/Control Paper/DataForPublications")

library(codyn)
library(ggplot2)
theme_set(theme_bw(12))+
  theme_update(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
library(devtools)
library(tidyverse)
library(broom)
library(codyn)


##Import Data for Main Analysis - Subset 1 Data - where data was only used from the first ten years of experiments and four timesteps were choosen randomly
metrics2<-read.csv("Question2_Subset1-4timepoints-from<11thYr.csv")

###list so we know which datasets we are using in this analysis
list<-  as.data.frame(unique(metrics2$site_project_comm))


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

#Get average so only 1 dot per sitextrt (i.e., average across yrs)
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


#Make Figure 4
rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(composition_change, composition_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(composition_change, composition_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0)) %>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

A<-ggplot(data=metrics3, aes(x=composition_change, y=composition_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "Composition Change",
    y = "Composition Difference")


rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(dispersion_change), abs_dispersion_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(abs(dispersion_change), abs_dispersion_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

B<-ggplot(data=metrics3, aes(x=abs(dispersion_change), y=abs_dispersion_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "|Dispersion Change|",
    y = "|Dispersion Difference|")

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(richness_change), abs(richness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(richness_change), abs(richness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

C<-ggplot(data=metrics3, aes(x=abs(richness_change), y=abs(richness_diff)))+
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "|Richness Change|",
    y = "|Richness Difference|")

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(evenness_change), abs(evenness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(evenness_change), abs(evenness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

D<-ggplot(data=metrics3, aes(x=abs(evenness_change), y=abs(evenness_diff)))+
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "|Evenness Change|",
    y = "|Evenness Difference|")


rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(rank_change, rank_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(rank_change, rank_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

E<-ggplot(data=metrics3, aes(x=rank_change, y=rank_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "Rank Change",
    y = "Rank Difference")

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(gains, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(gains, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

G<-ggplot(data=metrics3, aes(x=gains, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "Gains",
    y = "Species Difference")

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(losses, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(losses, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

H<-ggplot(data=metrics3, aes(x=losses, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "Losses",
    y = "Species Difference")


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
#Export 1000x500

#### Calculate Residuals from the models in Figure 4
modA<- lm(data=metrics3, composition_diff~composition_change)
summary(modA)
ResidualData<-cbind (metrics3, modA$residuals)

modB<- lm(data=metrics3, abs_dispersion_diff~abs(dispersion_change))
summary(modB)
ResidualData<-cbind (ResidualData, modB$residuals)

modC<- lm(data=metrics3, abs(richness_diff)~abs(richness_change))
summary(modC)
ResidualData<-cbind (ResidualData, modC$residuals)

modD<- lm(data=metrics3, abs(evenness_diff)~abs(evenness_change))
summary(modD)
ResidualData<-cbind (ResidualData, modD$residuals)

modE<- lm(data=metrics3, rank_diff~rank_change)
summary(modE)
ResidualData<-cbind (ResidualData, modE$residuals)

modF<- lm(data=metrics3, species_diff~gains)
summary(modF)
ResidualData<-cbind (ResidualData, modF$residuals)

modG<- lm(data=metrics3, species_diff~losses)
summary(modG)
ResidualData<-cbind (ResidualData, modG$residuals)


###Import Metadata
corredat_sitebiotic<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/environmental data/CoRRE_siteBiotic_May2023.csv")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_code!="GVN")
corredat_siteclimate<-read.csv("~/Dropbox/sDiv_sCoRRE_shared/CoRRE data/CoRRE data/environmental data/CoRRE_siteLocationClimate_Dec2021.csv") 


ScaledSiteData<-corredat_sitebiotic %>% 
  full_join(corredat_siteclimate) %>% 
  select(site_project_comm, anpp, MAP, MAT, rrich, annual_relcov) %>% 
  group_by(site_project_comm) %>% 
  summarise (ANPP_scaled=mean(anpp),
             MAP_scaled=mean(MAP),
             MAT_scaled=mean(MAT),
             rrich_scaled=mean(rrich), 
             annual_recov_scaled=mean(annual_relcov)) %>% 
  mutate(across(where(is.numeric), scale))

Resid_Meta<-full_join(ResidualData, ScaledSiteData) %>% 
  filter(richness_change!="NA")
#This is the dataset you now use for Table S3 (which needs metrics3 for the data and Resid_Meta for the metadata) and for Figure 5 which just uses Resid_Meta as the analysis is on the residuals in that one. 

#Table S3 - Checking to see if pattern in Fig 4 is driven by site level characteristics
TableS3_models <- list(
  Composition = lm(composition_diff ~ composition_change + MAP_scaled + MAT_scaled +
                     MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                   data = Resid_Meta),
  
  Dispersion = lm(abs_dispersion_diff ~ dispersion_change + MAP_scaled + MAT_scaled +
                    MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                  data = Resid_Meta),
  
  Richness = lm(richness_diff ~ richness_change + MAP_scaled + MAT_scaled +
                  MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                data = Resid_Meta),
  
  Evenness = lm(evenness_diff ~ evenness_change + MAP_scaled + MAT_scaled +
                  MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                data = Resid_Meta),
  
  Rank = lm(rank_diff ~ rank_change + MAP_scaled + MAT_scaled +
              MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
            data = Resid_Meta),
  
  `Species gains` = lm(species_diff ~ gains + MAP_scaled + MAT_scaled +
                         MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                       data = Resid_Meta),
  
  `Species losses` = lm(species_diff ~ losses + MAP_scaled + MAT_scaled +
                          MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                        data = Resid_Meta)
)

change_terms <- c(
  Composition = "composition_change",
  Dispersion = "dispersion_change",
  Richness = "richness_change",
  Evenness = "evenness_change",
  Rank = "rank_change",
  `Species gains` = "gains",
  `Species losses` = "losses"
)

TableS3 <- purrr::imap_dfr(TableS3_models, function(mod, metric_name) {
  broom::tidy(mod) %>%
    filter(term == change_terms[[metric_name]]) %>%
    mutate(
      Metric = metric_name,
      `β ± SE for change predictor` = paste0(round(estimate, 3), " ± ", round(std.error, 3)),
      `p-value` = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3))),
      `Model R²` = round(broom::glance(mod)$r.squared, 3),
      Support = ifelse(p.value < 0.05, "Strong", "Weak")
    ) %>%
    select(Metric, `β ± SE for change predictor`, `p-value`, `Model R²`, Support)
})

TableS3
write.csv(TableS3, "TableS3_RobustnessModels.csv",
          row.names = FALSE)







####Figure 5
####multiple regression of residuals vs site level characteristics

#Multiple Regression
comp_change<-(lm(modA$residuals~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=Resid_Meta))
summary(comp_change)

disp_change<-(lm(modB$residuals~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=Resid_Meta))
summary(disp_change)

rich_change<-(lm(modC$residuals~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=Resid_Meta))
summary(rich_change)

even_change<-(lm(modD$residuals~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=Resid_Meta))
summary(even_change)

rank_change<-(lm(modE$residuals~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=Resid_Meta))
summary(rank_change)

gains_change<-(lm(modF$residuals~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=Resid_Meta))
summary(gains_change)

losses_change<-(lm(modG$residuals~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=Resid_Meta))
summary(losses_change)

EffectSize<-read.csv("~/Dropbox/C2E/Products/Control Paper/Q2_MultipleRegs_Dec2025_scaled.csv", header = TRUE)

# Prepare data
dat_plot <- EffectSize %>%
  filter(drivers != "(Intercept)") %>%
  select(-X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6) %>% 
  mutate(drivers2=drivers) %>% 
  mutate(
    drivers2 = factor(drivers, levels = rev(c("MAT", "MAP", "MAP:MAT", "annual_relcov", "rrich"))),
    CI_lower = Estimate - 1.96 * Std.Error,
    CI_upper = Estimate + 1.96 * Std.Error,
    Effect = case_when(
      P < 0.05 & Estimate > 0 ~ "Positive",
      P < 0.05 & Estimate < 0 ~ "Negative",
      TRUE ~ "Not significant"
    )
  ) %>% 
  select(-drivers2)

# Plot

metric_labels <- c(
  comp_change = "Composition\nChange",
  disp_change = "Dispersion\nChange",
  even_change = "Evenness\nChange",
  rich_change = "Richness\nChange",
  rankk_change = "Rank\nChange",
  gains = "Species\nGains",
  losses = "Species\nLosses"
)

driver_labels <- c(
  MAT_scaled="MAT",
  MAP_scaled="MAP",
  "MAP_scaled:MAT_scaled"="MAP:MAT",
  annual_recov_scaled = "% Annual\nCover",
  rrich_scaled= "Gamma\nRichness"
)

dat_plot2<-dat_plot %>% 
  mutate(across(community_metric, ~factor(., levels=c("comp_change","disp_change", "even_change", "rich_change", "rankk_change", "gains", "losses")))) %>% 
  mutate(drivers = factor(drivers, levels = rev(names(driver_labels))))

ggplot(dat_plot2, aes(x = drivers, y = Estimate, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(color = Effect), size = 1.8, fatten =2.8) +
  scale_color_manual(values = c(
    "Positive" = "blue",
    "Negative" = "darkorange",
    "Not significant" = "gray60"
  )) +
  facet_grid(. ~ community_metric,
             labeller = labeller(community_metric = metric_labels)) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01)))+
  scale_x_discrete( labels = driver_labels,
                    expand = expansion(mult = c(0.2, 0.2))) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 13.5, margin = margin(r = 2), ),
    strip.text = element_text(face = "bold", size = 14.5,  margin = margin(b = 4)),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, size=12, vjust=.5),
    legend.key.height = unit(1.6, "lines"),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    plot.margin       = margin(t = 20, r = 5, b = 5, l = 5, unit = "pt") 
  ) +
  labs(
    y = "Estimate (± 95% CI)"
  )
#expor 1200x400








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
write.csv(Metrics,"C2E/Products/Control Paper/Output/CvT_Metrics_RACsMult_4timepoints_Dec2025_nutonly_10yrless.csv" , row.names=F)
#####################################################################################
##################START HERE NOW THAT THINGS ARE CALCULATED##########################
###################Control_Change vs Difference using 4 yrs only######################
#####################################################################################

metrics2<-read.csv("C2E/Products/Control Paper/Output/CvT_Metrics_RACsMult_4timepoints_Dec2025_nutonly_10yrless.csv")

###list
list<-  as.data.frame(unique(metrics2$site_project_comm))
#write.csv(list, "C2E/Products/Control Paper/Output/listofQ2sites_May2023_5ormoreyrs_nutonly.csv")

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
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "Composition Change",
    y = "Composition Difference")


rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(dispersion_change), abs_dispersion_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(abs(dispersion_change), abs_dispersion_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

B<-ggplot(data=metrics3, aes(x=abs(dispersion_change), y=abs_dispersion_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "|Dispersion Change|",
    y = "|Dispersion Difference|")

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(richness_change), abs(richness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(richness_change), abs(richness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

C<-ggplot(data=metrics3, aes(x=abs(richness_change), y=abs(richness_diff)))+
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "|Richness Change|",
    y = "|Richness Difference|")

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(abs(evenness_change), abs(evenness_diff))$estimate),
                            digits=3), 
            p.value = (cor.test(abs(evenness_change), abs(evenness_diff))$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

D<-ggplot(data=metrics3, aes(x=abs(evenness_change), y=abs(evenness_diff)))+
  geom_point()+
  #geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "|Evenness Change|",
    y = "|Evenness Difference|")


rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(rank_change, rank_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(rank_change, rank_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

E<-ggplot(data=metrics3, aes(x=rank_change, y=rank_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "Rank Change",
    y = "Rank Difference")

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(gains, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(gains, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

G<-ggplot(data=metrics3, aes(x=gains, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "Gains",
    y = "Species Difference")

rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(losses, species_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(losses, species_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0))%>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

H<-ggplot(data=metrics3, aes(x=losses, y=species_diff))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_text(data=rvalues, mapping=aes(x=Inf, y = Inf, label = r.value), hjust=1.05, vjust=1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40")+
  labs(
    x = "Losses",
    y = "Species Difference")


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
#Export 1000x500
