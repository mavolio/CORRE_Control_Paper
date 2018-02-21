library(tidyverse)
library(codyn)
library(ggplot2)
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))

## Kim's desktop
setwd("C:/Users/la pierrek/Dropbox (Smithsonian)/working groups/converge diverge working group/converge_diverge/Control Paper/Output")

#Read in data
change<-read.csv("Comm_change_all.csv")%>%
  separate(calendar_year_pair, c("start", "end"), sep="-")

ggplot(data=change, aes(x=end, y=composition_change))+
  geom_point()+
  facet_wrap(~site_project_comm, scales="free_x")

#Read in data
slope<-read.csv("rate_change_all.csv")

meanslope<-slope%>%
  group_by(site_project_comm)%>%
  summarise(meanslope=mean(rate_change))%>%
  ungroup()


##trying intervals It is ALL directional change except for 1 or 2!!
##you need to read in intervals - I already have it in my working environment and didn't set the path for you.

rt_change_int<- read.csv("rate_change_interval_all.csv")

ave_int<-rt_change_int%>%
  group_by(site_project_comm, interval)%>%
  summarise(meandist=mean(distance))

ggplot(data=ave_int, aes(x=interval, y=meandist))+
  geom_point()+
  facet_wrap(~site_project_comm, scales="free")
  

ggplot(data=rt_change_int, aes(x=interval, y=distance))+
  geom_boxplot(aes(group=interval))+
  facet_wrap(~site_project_comm, scales="free")


#looking at replicates at a single site
ggplot(data=subset(rt_change_int, site_project_comm=="JSP_GCE_0"), aes(x=interval, y=distance))+
  geom_boxplot(aes(group=interval))+
  facet_wrap(~plot_id, scales="free")


ave_int_plot<-rt_change_int%>%
  group_by(site_project_comm, interval, plot_id)%>%
  summarise(meandist=mean(distance))
ggplot(data=ave_int_plot, aes(x=interval, y=meandist, group=plot_id))+
  #geom_smooth(method="lm", formula=y~x+I(x^2), se=F)+
  geom_line()+
  facet_wrap(~site_project_comm, scales="free")+
  theme(legend.position = "none")
