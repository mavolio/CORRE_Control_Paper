## Sally's 
setwd("~/Dropbox/C2E/Products/Control Paper/DataForPublications")

library(tidyverse)
library(vegan)
library(codyn)
library(PerformanceAnalytics)
library(data.table)
library(broom)
stderror <- function(x) sd(x)/sqrt(length(x))


#Import Metrics of Change calculated from raw community composition data - this dataset is subset to 7 annual timesteps so that all datsets no matter how long are each represented by 7 timesteps. 
YearlyMetrics_subset<-read.csv("Question1_ChangeMetrics_7timestepssubset.csv") 

#Get the site list so you know what data you are including in Q1
q1SiteList<-unique(YearlyMetrics_subset$site)

####Figure S4 - Histogram of change metrics where each site x year is the unit of replication - so each site is represented by 7 different time comparisons
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


####Figure 2 - Change Histograms where the 7 timesteps per site are averaged together making each site represented once 

MeanMetrics_subset<-YearlyMetrics_subset %>% 
  group_by(site_project_comm) %>% 
  summarise(composition_change=mean(composition_change, na.rm=T), 
            dispersion_change=mean(dispersion_change, na.rm=T),
            richness_change=mean(richness_change),
            evenness_change=mean(evenness_change, na.rm=T),
            rank_change=mean(rank_change),
            gains=mean(gains),
            losses=mean(losses))

bottom_margin <- theme(
  plot.margin = margin(t = 5, r = 5, b = 18, l = 5),
  axis.title.x = element_text(margin = margin(t = 8))
)
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
  theme(legend.position = "none")+
  bottom_margin
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
  theme(legend.position = "none")+
  bottom_margin

Rich<-ggplot(data=MeanMetrics_subset, aes(x=richness_change))+
  geom_density(aes(y=.015 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .015, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(richness_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(richness_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Richness Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")+
  bottom_margin

Even<-ggplot(data=MeanMetrics_subset, aes(x=evenness_change))+
  geom_density(aes(y=.01 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .01, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(evenness_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(evenness_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Evenness Change")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")+
  bottom_margin

Rank<-ggplot(data=MeanMetrics_subset, aes(x=rank_change))+
  geom_density(aes(y=.025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(rank_change, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(rank_change, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Rank Change")+
  scale_y_continuous(breaks=c(0, 5, 10 , 15), name="Count")+
  theme(legend.position = "none")+
  bottom_margin

Gains<-ggplot(data=MeanMetrics_subset, aes(x=gains))+
  geom_density(aes(y=.025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(gains, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(gains, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Species Gains")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")+
  bottom_margin

Losses<-ggplot(data=MeanMetrics_subset, aes(x=losses))+
  geom_density(aes(y=.025 * ..count..), alpha=1, fill="grey")+
  geom_histogram(binwidth= .025, fill="white", colour="black", aes(alpha=.5))+
  geom_vline(aes(xintercept=mean(losses, na.rm=T)),   
             color="red", linetype="solid", size=.5)+
  geom_vline(aes(xintercept=median(losses, na.rm=T)),   
             color="red", linetype="dashed", size=.5)+
  scale_x_continuous(name="Species Losses")+
  scale_y_continuous(name="Count")+
  theme(legend.position = "none")+
  bottom_margin

library(grid)
pushViewport(viewport(layout=grid.layout(2,4)))
print(CompChange, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(Disp, vp=viewport(layout.pos.row = 1, layout.pos.col = 2))
print(Even, vp=viewport(layout.pos.row = 1, layout.pos.col = 3))
print(Rank, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(Rich, vp=viewport(layout.pos.row = 1, layout.pos.col = 4))
print(Gains, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(Losses, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))



####TABLE S2 - looking at how change varies with site level characteristics ####


### Bring in Site Level Metadata 
Meta5<-read.csv("Question1_MetaData.csv") %>% 
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
#write.csv(AdjustPVals, file="PValsAdjusted_Nov2023.csv", row.names=F)


####Multiple regressions

#correlations
head(YearlyLevelData2)

panel.cor <- function(x, y, digits = 3, ...){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  cor.test <- cor.test(x, y)
  r <- round(cor.test$estimate, digits = digits)
  p <- round(cor.test$p.value, digits = digits)
  text(0.5, 0.5, paste("r = ", r, "\np = ", p))
}
#Panel of correlations
panel.corr <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=3)
  txt <- paste0("Corr: ", r)
  text(0.5, 0.5, txt, cex = 1)
}

#Panel of histograms
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  len <- length(breaks)
  y <- h$counts/max(h$counts)
  rect(breaks[-len], 0, breaks[-1], y, col = "lightblue")
}

#Panel of scatterplots
panel.scat <- function(x, y){
  points(x,y, pch = 19, cex = 1, col = "coral")
}

pairs(YearlyLevelData2[11:15],
      lower.panel = panel.scat,
      upper.panel = panel.corr,
      diag.panel = panel.hist,
      labels = c("ANPP","MAP","MAT",
                 "rrich","annual_relcov"),
      gap = 0.3)

pairs(YearlyLevelData2[11:15], lower.panel = panel.cor)

###DROP ANPP - its .71 corr with MAP, and instead do a MAP*MAT interaction term

ScaledSiteData<-YearlyLevelData2 %>% 
  select(site_project_comm, ANPP, MAP, MAT, rrich, annual_relcov) %>% 
  group_by(site_project_comm) %>% 
  summarise (ANPP_scaled=mean(ANPP),
             MAP_scaled=mean(MAP),
             MAT_scaled=mean(MAT),
             rrich_scaled=mean(rrich), 
             annual_recov_scaled=mean(annual_relcov)) %>% 
  mutate(across(where(is.numeric), scale))
# standardize data now!

### PCA
dat<- ScaledSiteData
pca <- princomp(dat[3:6])
biplot(pca)
summary(pca)

library(ggfortify)
autoplot(pca, data = dat, 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = FALSE, loadings.label.size = 2.5)


YearlyLevelData3<- YearlyLevelData2 %>% 
  full_join(ScaledSiteData)
head(YearlyLevelData3)

comp_change<-(lm(composition_change~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=YearlyLevelData3))
summary(comp_change)

disp_change<-(lm(dispersion_change~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=YearlyLevelData3))
summary(disp_change)

rich_change<-(lm(richness_change~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=YearlyLevelData3))
summary(rich_change)

even_change<-(lm(evenness_change~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=YearlyLevelData3))
summary(even_change)

rank_change<-(lm(rank_change~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=YearlyLevelData3))
summary(rank_change)

gains_change<-(lm(gains~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=YearlyLevelData3))
summary(gains_change)

losses_change<-(lm(losses~MAP_scaled+MAT_scaled+MAP_scaled*MAT_scaled+rrich_scaled+annual_recov_scaled, data=YearlyLevelData3))
summary(losses_change)

EffectSize<-read.csv("Q1_MultipleRegs_June2025_scaled.csv", header = TRUE)

# Prepare data
dat_plot <- EffectSize %>%
  filter(drivers != "(Intercept)") %>%
  mutate(
    drivers = factor(drivers, levels = rev(c("MAT", "MAP", "MAP:MAT", "annual_relcov", "rrich"))),
    CI_lower = Estimate - 1.96 * Std.Error,
    CI_upper = Estimate + 1.96 * Std.Error,
    Effect = case_when(
      P < 0.05 & Estimate > 0 ~ "Positive",
      P < 0.05 & Estimate < 0 ~ "Negative",
      TRUE ~ "Not significant"
    )
  )

# Plot
dat_plot2<-dat_plot %>% 
    mutate(across(community_metric, ~factor(., levels=c("comp_change","disp_change", "even_change", "rich_change", "rank_change", "gains", "losses"))))
metric_labels <- c(
  comp_change = "Composition\nChange",
  disp_change = "Dispersion\nChange",
  even_change = "Evenness\nChange",
  rich_change = "Richness\nChange",
  rank_change = "Rank\nChange",
  gains = "Species\nGains",
  losses = "Species\nLosses"
)
driver_labels <- c(
  annual_relcov = "% Annual\nCover",
  rrich = "Gamma\nRichness"
)

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
  scale_x_discrete(labels = function(x) dplyr::recode(
    x,
    annual_relcov = "% Annual\nCover",
    rrich        = "Gamma\nRichness",
    .default     = x   # keep other labels as-is
  ), expand = expansion(mult = c(0.2, 0.2)), ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 13.5, margin = margin(r = 2)),
    strip.text = element_text(face = "bold", size = 16),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, size=12, vjust=.5),
    legend.key.height = unit(1.6, "lines"),
    panel.spacing.x = unit(0.1, "lines"),
    panel.spacing.y = unit(0.1, "lines")
  ) +
  labs(
    y = "Estimate (± 95% CI)"
  )
#expor 1200x400


