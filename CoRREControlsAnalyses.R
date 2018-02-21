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
change<-read.csv("Comm_change.csv")%>%
  separate(calendar_year_pair, c("start", "end"), sep="-")

ggplot(data=change, aes(x=end, y=composition_change))+
  geom_point()+
  facet_wrap(~site_project_comm, scales="free_x")