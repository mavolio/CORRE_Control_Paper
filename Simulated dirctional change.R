library(tidyverse)
library(gridExtra)
library(grid)
library(gtable)
library(codyn)
library(vegan)
theme_set(theme_bw(12))

#home
setwd("~/Dropbox/")
#Meghan Work
setwd("C:\\Users\\megha\\Dropbox")

#to subset only the treatments I want
dat<-read.csv("C2E/Products/Control Paper/Simulated_dataset.csv")

data<-dat%>%
  mutate(time=as.numeric(iteration))%>%
  separate(id, into=c("alpha","theta","scenario","rep"), sep="_", remove=F)%>%
  select(-X)%>%
  mutate(relcov=abundance/1000)%>%
  rename(plot=site)
  
  

# in the sim dataset, communites are differentiated by alpha (richness), theta (evenness) and scenario (rate of temporal and spatial variability: four scenarios: a: high temporal and high spatial variability; b: low temporal, low spatial variability; c: low temporal, high spatial variability; d: high temporal, low spatial variability"). For each richness-evennes combination (9 combinations) there are each community type. Each of these 10 community types have 10 replicates, called "sites" at a given point in time. Each community type, time, and site is then replicated 10 times.


# ### to get all data time lags
rate_change_interval<-data.frame()
com_rep<-unique(data$id)

for (i in 1:length(com_rep)){
  
  subset<-data%>%
    filter(id==com_rep[i])
  
  out<-rate_change_interval(subset, time.var = 'time', species.var = "species", abundance.var = 'relcov', replicate.var = 'plot')
  
  out$id<-com_rep[i]
  
  rate_change_interval<-rbind(rate_change_interval, out)

}

write.csv(rate_change_interval, "C2E/Products/Control Paper/simulated_rate_change_relcov.csv", row.names = F)

##to get slopes and p-values for each treatment through time (i.e., putting in all plot to plot combinations into a single linear regression across replicates)
##get slopes for each treatment including controls

rate_change_interval<-read.csv("C2E/Products/Control Paper/simulated_rate_change_relcov.csv")

sample<-unique(rate_change_interval$id)

lm.slopes<-data.frame()
for (i in 1:length(sample)){
  subset<-rate_change_interval%>%
    filter(id==sample[i])
  test.lm<-lm(distance~interval, data=subset)
  output.lm<-data.frame(sample=unique(subset$id), 
                        est=summary(test.lm)$coef["interval", c("Estimate")], 
                        st.er=summary(test.lm)$coef["interval", c("Std. Error")],
                        p.val=summary(test.lm)$coef["interval","Pr(>|t|)"])
  lm.slopes<-rbind(lm.slopes, output.lm)
}

lm.slopes2<-lm.slopes%>%
  separate(sample, into=c("alpha","theta","scenario","rep"), sep="_", remove=F)
  
datacheck<-rate_change_interval%>%
  filter(id=="5_0.7_d_10")

ggplot(data=datacheck, aes(x=interval, y=distance))+
  geom_point()

###not sig for sample: 5_0.7_d_5
###sig for sample: 5_0.7_a_1

nsreg<-rate_change_interval%>%
  filter(id=="5_0.7_d_5")

nsplot<-ggplot(data=nsreg, aes(x=interval, y=distance))+
  geom_point()+
  ggtitle("Not Sig.")

ns<-data%>%
  filter(id=="5_0.7_d_5")%>%
  spread(species, abundance, fill=0)

pca<-prcomp(ns[,10:24])
summary(pca)
scores<-data.frame(pca$x)
scores2<- cbind(plots, scores) # binds the NMDS scores of year i to all years previously run

nspca<-ggplot(data=scores2, aes(x=PC1, y = PC2, color=as.factor(time)))+
  geom_point()+
  ggtitle("Not Sig.")

###sig

sreg<-rate_change_interval%>%
  filter(id=="5_0.7_a_1")

splot<-ggplot(data=sreg, aes(x=interval, y=distance))+
  geom_point()+
  ggtitle("Sig")

s<-data%>%
  filter(id=="5_0.7_a_1")%>%
  spread(species, abundance, fill=0)

pca<-prcomp(s[,10:58])
scores<-data.frame(pca$x)
plots<-ns[,c(1, 7)]
scores2<- cbind(plots, scores) # binds the NMDS scores of year i to all years previously run


sigpca<-ggplot(data=scores2, aes(x=PC1, y = PC2, color=as.factor(time)))+
  geom_point()+
  ggtitle("Sig.")

grid.arrange(nsplot, splot)
grid.arrange(nspca, sigpca)


###doing rate_change
rate_change<-data.frame()
com_rep<-unique(data$id)

for (i in 1:length(com_rep)){
  
  subset<-data%>%
    filter(id==com_rep[i])
  
  out<-rate_change(subset, time.var = 'time', species.var = "species", abundance.var = 'relcov', replicate.var = 'plot')
  
  out$id<-com_rep[i]
  
  rate_change<-rbind(rate_change, out)
  
}

ave_rc<-rate_change%>%
  group_by(id)%>%
  summarize(rt=mean(rate_change))%>%
  separate(id, into=c("alpha","theta","scenario","rep"), sep="_", remove=F) %>%
  group_by(scenario) %>% 
  summarize(rt=mean(rt))

##linking slopes to numbers
rate_change_interval2<-rate_change_interval %>% 
  rename(plot=site)%>%
  filter(interval==1) %>% 
  group_by(id, plot)%>%
  summarize(distance=mean(distance))

all<-rate_change_interval2%>%
  left_join(rate_change)
