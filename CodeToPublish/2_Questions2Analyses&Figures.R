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
metrics3 <- metrics3 %>%
  mutate(row_id = row_number())

get_resids <- function(data, formula, resid_name) {
  model_data <- model.frame(formula, data = data, na.action = na.omit)
  
  used_rows <- as.numeric(rownames(model_data))
  
  mod <- lm(formula, data = data, na.action = na.omit)
  
  tibble(
    row_id = used_rows,
    !!resid_name := residuals(mod)
  )
}

ResidualData <- metrics3 %>%
  left_join(get_resids(metrics3, composition_diff ~ composition_change, "resid_composition"),
            by = "row_id") %>%
  left_join(get_resids(metrics3, abs_dispersion_diff ~ abs(dispersion_change), "resid_dispersion"),
            by = "row_id") %>%
  left_join(get_resids(metrics3, abs(richness_diff) ~ abs(richness_change), "resid_richness"),
            by = "row_id") %>%
  left_join(get_resids(metrics3, abs(evenness_diff) ~ abs(evenness_change), "resid_evenness"),
            by = "row_id") %>%
  left_join(get_resids(metrics3, rank_diff ~ rank_change, "resid_rank"),
            by = "row_id") %>%
  left_join(get_resids(metrics3, species_diff ~ gains, "resid_gains"),
            by = "row_id") %>%
  left_join(get_resids(metrics3, species_diff ~ losses, "resid_losses"),
            by = "row_id")


###Import Metadata
Questions2_MetaData<-read.csv("Questions2_MetaData.csv") %>% 
  filter(site_project_comm!="NA")

ScaledSiteData<-Questions2_MetaData %>% 
  group_by(site_project_comm) %>% 
  summarise (ANPP_scaled=mean(anpp),
             MAP_scaled=mean(MAP),
             MAT_scaled=mean(MAT),
             rrich_scaled=mean(rrich), 
             annual_recov_scaled=mean(annual_relcov)) %>% 
  mutate(across(where(is.numeric), scale))

Resid_Meta<-full_join(ResidualData, ScaledSiteData) %>% 
  filter(!is.na(richness_change))
#This is the dataset you now use for Table S3 and for Figure 5 

#Table S3 - Checking to see if pattern in Fig 4 is driven by site level characteristics
TableS3_models <- list(
  Composition = lm(composition_diff ~ composition_change + MAP_scaled + MAT_scaled +
                     MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                   data = Resid_Meta),
  
  Dispersion = lm(abs_dispersion_diff ~ abs(dispersion_change) + MAP_scaled + MAT_scaled +
                    MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                  data = Resid_Meta),
  
  Richness = lm(abs(richness_diff) ~ abs(richness_change) + MAP_scaled + MAT_scaled +
                  MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                data = Resid_Meta),
  
  Evenness = lm(abs(evenness_diff) ~ abs(evenness_change) + MAP_scaled + MAT_scaled +
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
  Dispersion = "abs(dispersion_change)",
  Richness = "abs(richness_change)",
  Evenness = "abs(evenness_change)",
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

#write.csv(TableS3, "TableS3_RobustnessModels.csv", row.names = FALSE)



####Figure 5
####multiple regression of residuals vs site level characteristics

#Multiple Regression

# Run Figure 5 residual models
Fig5_models <- list(
  comp_change = lm(resid_composition ~ MAP_scaled + MAT_scaled +
                     MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                   data = Resid_Meta),
  
  disp_change = lm(resid_dispersion ~ MAP_scaled + MAT_scaled +
                     MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                   data = Resid_Meta),
  
  rich_change = lm(resid_richness ~ MAP_scaled + MAT_scaled +
                     MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                   data = Resid_Meta),
  
  even_change = lm(resid_evenness ~ MAP_scaled + MAT_scaled +
                     MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                   data = Resid_Meta),
  
  rankk_change = lm(resid_rank ~ MAP_scaled + MAT_scaled +
                      MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
                    data = Resid_Meta),
  
  gains = lm(resid_gains ~ MAP_scaled + MAT_scaled +
               MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
             data = Resid_Meta),
  
  losses = lm(resid_losses ~ MAP_scaled + MAT_scaled +
                MAP_scaled:MAT_scaled + rrich_scaled + annual_recov_scaled,
              data = Resid_Meta)
)

# Extract model coefficients directly
EffectSize <- purrr::imap_dfr(Fig5_models, function(mod, metric_name) {
  broom::tidy(mod) %>%
    filter(term != "(Intercept)") %>%
    mutate(community_metric = metric_name)
}) %>%
  rename(
    drivers = term,
    Estimate = estimate,
    Std.Error = std.error,
    tvalue = statistic,
    P = p.value
  )
EffectSize

# Prepare data
dat_plot <- EffectSize %>%
  filter(drivers != "(Intercept)") %>%
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






###CLEAR EVERYTHING AND STart fresh here as some of the names are repeat used here again.
##Import Data for Subset2 Analysis - where data was used from any years of experiments and four timesteps were chosen randomly
metrics2<-read.csv("Question2_Subset2-4timepoints-fromAnyYr.csv")

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


#Make Figure S5
rvalues <- metrics3 %>%
  summarize(r.value = round((cor.test(composition_change, composition_diff)$estimate),
                            digits=3), 
            p.value = (cor.test(composition_change, composition_diff)$p.value))%>%
  mutate(sig=ifelse(p.value<0.05, 1, 0)) %>% 
  mutate(PAdjust=p.adjust(p.value, method="BH", n=7))

A<-ggplot(data=metrics3, aes(x=composition_change, y=composition_diff))+
  geom_point()+
 # geom_smooth(method="lm", se=F)+
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
  geom_smooth(method="lm", se=F)+
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
print(A, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(B, vp=viewport(layout.pos.row = 1, layout.pos.col = 2))
print(C, vp=viewport(layout.pos.row = 1, layout.pos.col = 3))
print(D, vp=viewport(layout.pos.row = 1, layout.pos.col = 4))
print(E, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(G, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(H, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))
print(I, vp=viewport(layout.pos.row = 2, layout.pos.col = 4))
#Export 1000x500











####SUBSET 3 Analysis for Supp Figure S6
###CLEAR EVERYTHING AND STart fresh here as some of the names are repeat used here again.
##Import Data for Subset3 Analysis - where data was used from any years of experiments and four timesteps were chosen randomly
metrics2<-read.csv("Question2_Subset3-NutrientOnly-4timepoints-from<11thYr.csv")

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


#Make Figure S6
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
print(A, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(B, vp=viewport(layout.pos.row = 1, layout.pos.col = 2))
print(C, vp=viewport(layout.pos.row = 1, layout.pos.col = 3))
print(D, vp=viewport(layout.pos.row = 1, layout.pos.col = 4))
print(E, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(G, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(H, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))
print(I, vp=viewport(layout.pos.row = 2, layout.pos.col = 4))
#Export 1000x500