library(ggmap)

sites<-read.csv('~/Dropbox/converge_diverge/Control Paper/controls_data list.csv')



mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
ggplot() +
  mapWorld+
  geom_point(data=sites, aes(x=Long, y=Lat), color="black", shape=19, color="black", size=2)+
  labs(x="Longitude", y = "Lattitude")

