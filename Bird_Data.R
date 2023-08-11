#install.packages("OptCirClust")
#install.packages("ape")
#install.packages("knitr")
#install.packages("seqinr")
#install.packages("SpatialVx")
#install.packages("NPCirc")
#install.packages("Directional)
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("CircStats")
#install.packages("map")


library(Directional)
library(circular)
#library(Directional)
library(CircStats)
library(tidyverse)
library(ggplot2)
#library(SpatialVx)
library(NPCirc)
library(CircStats)


# Data 1

library(readr)
B_data = read.csv(file.choose())
#View(B_data)
attach(B_data)
head(B_data)
length(B_data$birdID)
#install.packages("tidyverse")
d = B_data[c(1:620),]
#length($longitude)

length(B_data$longitude)

final = read.csv(file.choose())
attach(final)
head(final)

world_coordinates <- map_data("world")

ggplot() +
  
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region)
  ) + 
  
  geom_point(
    data = B_data,
    aes(B_data$longitude, B_data$latitude, color = B_data$species),
    alpha = 1
  ) +    
  theme(legend.position="bottom")

d1=data.frame(B_data)


d11=d1[1:536,]
d12=d1[122:167,]
d13=d1[168:243,]
d14=d1[244:375,]
d15=d1[376:536,]
d16=d1[537:703,]


watson.test(circular(d12$longitude) , alpha = 0.01 , dist = "vonmises")
watson.test(circular(d12$latitude) , alpha = 0.01 , dist = "vonmises")
watson.test(circular(d13$longitude) , alpha = 0.01 , dist = "vonmises")
watson.test(circular(d13$latitude) , alpha = 0.01 , dist = "vonmises")
watson.test(circular(d13$latitude) , alpha = 0.01 , dist = "vonmises")
watson.test(circular(d14$longitude) , alpha = 0.01 , dist = "vonmises")
watson.test(circular(d14$latitude) , alpha = 0.01 , dist = "uniform")
watson.test(circular(d15$longitude) , alpha = 0.01 , dist = "uniform")
watson.test(circular(d15$latitude) , alpha = 0.01 , dist = "uniform")

#d16 is follwoing a von mises distribution (Swainson's Hawk)
watson.test(circular(d16$longitude) , alpha = 0.01 , dist = "vonmises")
watson.test(circular(d16$latitude) , alpha = 0.01 , dist = "vonmises")

summary(d16)
circ.mean(d16$latitude)
est.kappa(d16$latitude)
circ.mean(d16$longitude)
est.kappa(d16$longitude)
##############################################################################

dataframe = sort(d11$latitude)
length(dataframe)
head(dataframe)
ewq = dataframe[c(1:536)]
vec1 = c(ewq)
vec1
dfoo = data.frame(col1 = c(d1$latitude) , col2 = c(d1$longitude))
head(dfoo)
sub_df6 <- dfoo[dfoo$col1 %in% vec1,]
head(sub_df6)

###############################################################################
world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6,
    aes(col2 , col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

##############################################################################

cdladl = circular(sub_df6$col1)
cdlongl = circular(sub_df6$col2)
watson.test(cdladl , alpha = 0.01 , dist = "vonmises")
watson.test(cdlongl , alpha = 0.01 , dist = "vonmises")
##############################################################################

length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(1:46)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df611 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df611)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df611,
    aes(sub_df611$col2 , sub_df611$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df611$col1 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df611$col2 , alpha = 0.01 , dist = "vonmises")
summary(sub_df611)
circ.mean(sub_df611$col1)
est.kappa(sub_df611$col1)
circ.mean(sub_df611$col2)
est.kappa(sub_df611$col2)
##############################################################################

length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(47:48)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df61 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df61)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df61,
    aes(sub_df61$col2 , sub_df61$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df61$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df61$col1 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df61$col2 , alpha = 0.01 , dist = "uniform")
watson.test(sub_df61$col1 , alpha = 0.01 , dist = "uniform")
summary(sub_df61)
circ.summary(sub_df61$col1)
circ.mean(sub_df61$col1)
##############################################################################


length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(49:57)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df612 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df612)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df612,
    aes(sub_df612$col2 , sub_df612$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df612$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df612$col1 , alpha = 0.01 , dist = "vonmises")
summary(sub_df612)
circ.mean(sub_df612$col1)
circ.mean(sub_df612$col2)
est.kappa(sub_df6$col1)
est.kappa(sub_df612$col2)
############################################################################

length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(58:65)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df613 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df613)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df613,
    aes(sub_df613$col2 , sub_df613$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df613$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df613$col1 , alpha = 0.01 , dist = "vonmises")

summary(sub_df613)
circ.mean(sub_df613$col1)
circ.mean(sub_df613$col2)
est.kappa(sub_df613$col1)
est.kappa(sub_df613$col2)
##############################################################################
length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(66:82)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df614 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df614)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df614,
    aes(sub_df614$col2 , sub_df614$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df614$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df614$col1 , alpha = 0.01 , dist = "vonmises")

summary(sub_df614)
circ.mean(sub_df614$col1)
circ.mean(sub_df614$col2)
est.kappa(sub_df614$col1)
est.kappa(sub_df614$col2)
##############################################################################

length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(83:91)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df615 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df615)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df615,
    aes(sub_df615$col2 , sub_df615$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df615$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df615$col1 , alpha = 0.01 , dist = "vonmises")

summary(sub_df615)
circ.mean(sub_df615$col1)
circ.mean(sub_df615$col2)
est.kappa(sub_df615$col1)
est.kappa(sub_df615$col2)
##############################################################################

length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(100:113)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df616 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df616)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df616,
    aes(sub_df616$col2 , sub_df616$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df616$col2 , alpha = 0.05 , dist = "vonmises")
watson.test(sub_df616$col1 , alpha = 0.01 , dist = "vonmises")
summary(sub_df616)
circ.mean(sub_df616$col1)
circ.mean(sub_df616$col2)
est.kappa(sub_df616$col1)
est.kappa(sub_df616$col2)
##############################################################################

length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(114:121)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df617 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df617)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df617,
    aes(sub_df617$col2 , sub_df617$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df617$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df617$col1 , alpha = 0.01 , dist = "vonmises")
summary(sub_df617)
circ.mean(sub_df617$col1)
circ.mean(sub_df617$col2)
est.kappa(sub_df617$col1)
est.kappa(sub_df617$col2)
##############################################################################

length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(122:149)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df618 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df618)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df618,
    aes(sub_df618$col2 , sub_df618$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df618$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df618$col1 , alpha = 0.01 , dist = "vonmises")

summary(sub_df618)
circ.mean(sub_df618$col1)
circ.mean(sub_df618$col2)
est.kappa(sub_df618$col1)
est.kappa(sub_df618$col2)
##############################################################################

length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(531:536)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df619 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df619)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df619,
    aes(sub_df619$col2 , sub_df619$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df619$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df619$col1 , alpha = 0.01 , dist = "vonmises")

summary(sub_df619)
circ.mean(sub_df619$col1)
circ.mean(sub_df619$col2)
est.kappa(sub_df619$col1)
est.kappa(sub_df619$col2)
##############################################################################

length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(151:227)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df6111 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df6111)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6111,
    aes(sub_df6111$col2 , sub_df6111$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df6111$col2 , alpha = 0.01 , dist = "uniform")
watson.test(sub_df6111$col1 , alpha = 0.01 , dist = "uniform")

summary(sub_df6111)
circ.mean(sub_df6111$col1)
circ.mean(sub_df6111$col2)
##############################################################################



length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(93:99)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df6112 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df6112)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6112,
    aes(sub_df6112$col2 , sub_df6112$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(circular(sub_df6112$col2) , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6112$col1 , alpha = 0.01 , dist = "vonmises")
summary(sub_df6112)
circ.mean(sub_df6112$col1)
circ.mean(sub_df6112$col2)
est.kappa(sub_df6112$col1)
est.kappa(sub_df6112$col2)
############################################################################


length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(235:245)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df6113 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df6113)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6113,
    aes(sub_df6113$col2 , sub_df6113$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df6113$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6113$col1 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6113$col2 , alpha = 0.01 , dist = "uniform")
watson.test(sub_df6113$col1 , alpha = 0.01 , dist = "uniform")

summary(sub_df6113)
circ.mean(sub_df6113$col1)
circ.mean(sub_df6113$col2)
est.kappa(sub_df6113$col1)
est.kappa(sub_df6113$col2)
###############################################################################


length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(430:440)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df6114 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df6114)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6114,
    aes(sub_df6114$col2 , sub_df6114$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df6114$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6114$col1 , alpha = 0.01 , dist = "vonmises")

summary(sub_df6114)
circ.mean(sub_df6114$col1)
circ.mean(sub_df6114$col2)
est.kappa(sub_df6114$col1)
est.kappa(sub_df6114$col2)
#############################################################################

length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(441:447)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df6115 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df6115)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6115,
    aes(sub_df6115$col2 , sub_df6115$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df6115$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6115$col1 , alpha = 0.01 , dist = "vonmises")
pp.plot(sub_df6115$col1)
summary(sub_df6115)
circ.mean(sub_df6115$col1)
circ.mean(sub_df6115$col2)
est.kappa(sub_df6115$col1)
est.kappa(sub_df6115$col2)

##############################################################################
length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(228:234)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df6116 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df6116)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6116,
    aes(sub_df6116$col2 , sub_df6116$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df6116$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6116$col1 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6116$col2 , alpha = 0.01 , dist = "uniform")
watson.test(sub_df6116$col1 , alpha = 0.01 , dist = "uniform")

summary(sub_df6116)
circ.mean(sub_df6116$col1)
circ.mean(sub_df6116$col2)
est.kappa(sub_df6116$col1)
est.kappa(sub_df6116$col2)
#############################################################################


length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(235:245)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df6117 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df6117)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6117,
    aes(sub_df6117$col2 , sub_df6117$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")



watson.test(sub_df6117$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6117$col1 , alpha = 0.01 , dist = "vonmises")

summary(sub_df6117)
circ.mean(sub_df6117$col1)
circ.mean(sub_df6117$col2)
est.kappa(sub_df6117$col1)
est.kappa(sub_df6117$col2)
##########################################################################


length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(246:254)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df6118 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df6118)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6118,
    aes(sub_df6118$col2 , sub_df6118$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df6118$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6118$col1 , alpha = 0.01 , dist = "vonmises")

summary(sub_df6118)
circ.mean(sub_df6118$col1)
circ.mean(sub_df6118$col2)
est.kappa(sub_df6118$col1)
est.kappa(sub_df6118$col2)
#############################################################################



length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(448:452)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df6119 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df6119)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6119,
    aes(sub_df6119$col2 , sub_df6119$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df6119$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6119$col1 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6119$col2 , alpha = 0.01 , dist = "uniform")
watson.test(sub_df6119$col1 , alpha = 0.01 , dist = "uniform")

summary(sub_df6119)
circ.mean(sub_df6119$col1)
circ.mean(sub_df6119$col2)
est.kappa(sub_df6119$col1)
est.kappa(sub_df6119$col2)
#############################################################################


length(sub_df6$col1)
dataframe = sort(sub_df6$col2)
length(dataframe)
head(dataframe)
ewq2 = dataframe[c(255:262)]
vec12 = c(ewq2)
vec12
dfoooo = data.frame(col1 = c(sub_df6$col1) , col2 = c(sub_df6$col2))
head(dfoooo)
sub_df6120 <- dfoooo[dfoooo$col2 %in% vec12,]
head(sub_df6120)
##############################################################################

world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill= "lightyellow"
  )+
  geom_point(
    data = sub_df6120,
    aes(sub_df6120$col2 , sub_df6120$col1, color = "red",),
    alpha = 1
  ) +
  theme(legend.position="top")

watson.test(sub_df6120$col2 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6120$col1 , alpha = 0.01 , dist = "vonmises")
watson.test(sub_df6120$col2 , alpha = 0.01 , dist = "uniform")
watson.test(sub_df6120$col1 , alpha = 0.01 , dist = "uniform")

summary(sub_df6120)
circ.mean(sub_df6120$col1)
circ.mean(sub_df6120$col2)
est.kappa(sub_df6120$col1)
est.kappa(sub_df6120$col2)
############################################################################

new_data = B_data
library(maps)
world_map <- map_data("world")
northern_map <- subset(world_map, long <= 0)
# Create a basic plot using ggplot and specify the northern_map as the data source
plot <- ggplot() +
  geom_polygon(data = northern_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
  coord_fixed() +
  geom_point(
    data = new_data,
    aes(new_data$longitude , new_data$latitude, color = new_data$species,),
    alpha = 1
  ) 
theme(legend.position="top") # Fix the aspect ratio
# Customize the plot with additional layers and labels
plot <- plot +
  labs(title = "American Region") +
  xlab("Longitude") +
  ylab("Latitude")

# Print the plot
print(plot)

date <- as.Date(new_data$date, format = "%d-%m-%Y")

# Extract the year from the date
year <- format(date, "%Y")

# Create a new date object with only the year
startOfYear <- as.Date(paste0(year, "-01-01"))


# Calculate the number of days between the date and the start of the year
dayOfYear <- as.numeric(date - startOfYear) + 1

# Print the number of days in the year
print(dayOfYear)

new_data = data.frame(new_data , dayOfYear)
head(new_data)
new_data$delta = 23.45*sin(0.986*(new_data$dayOfYear + 284))
new_data$h = asin(sin(new_data$latitude)*sin(new_data$delta) + cos(new_data$latitude)*cos(new_data$delta))
new_data$The_turbidity_factor = 0.796 - 0.01*sin(0.986*(new_data$dayOfYear + 284))       
head(new_data)
new_data$C_t = 1 + 0.034*cos(new_data$dayOfYear - 2)
new_data$I_h = 1367*new_data$C_t*new_data$The_turbidity_factor*exp(-0.13/sin(new_data$h))*sin(new_data$h)
head(new_data)
new_data$D_h = 120*new_data$The_turbidity_factor*exp(-1/(0.4511+ sin(new_data$h)))
new_data$G_h = new_data$I_h + new_data$D_h
head(new_data)
filename <- "birdpath_rad_data.csv"  # Specify the filename and path as desired
write.csv(new_data, file = filename, row.names = FALSE)
library(circular)
plot(density.circular(new_data$latitude , bw = 50))
plot(density.circular(new_data$longitude , bw = 50))



# Function to calculate the initial bearing between two points
get_initial_bearing <- function(longA, latA, longB, latB) {
  delta_long <- longB - longA
  rad_latA <- latA * (pi / 180)  # Convert latA to radians
  rad_latB <- latB * (pi / 180)  # Convert latB to radians
  
  bearing <- atan2(sin(delta_long) * cos(rad_latB), cos(rad_latA) * sin(rad_latB) - sin(rad_latA) * cos(rad_latB) * cos(delta_long))
  bearing_degrees <- bearing * (180 / pi)  # Convert bearing to degrees
  
  return(bearing_degrees)
}


Black_bellied_plover = new_data[1:121,]
Black_crowned_night_heron = new_data[122:167,]
Brown_pelican = new_data[168:243,]
Long_billed_curlew = new_data[244:375,]
Pacific_loon = new_data[376:536,]
Swainsons_hawk = new_data[537:703,]

Black_bellied_plover$bird = 1
Black_crowned_night_heron$bird = 2
Brown_pelican$bird = 3
Long_billed_curlew$bird = 4
Pacific_loon$bird = 5
Swainsons_hawk$bird = 6


dff = bind_rows(Black_bellied_plover , Black_crowned_night_heron , Brown_pelican , 
                Long_billed_curlew , Pacific_loon , Swainsons_hawk)

library(dplyr)
tail(dff)


for (
  i in 1:46) {
  Black_crowned_night_heron$s[i] = Black_crowned_night_heron$G_h[i+1] - Black_crowned_night_heron$G_h[i] 
  Black_crowned_night_heron$theta[i] = get_initial_bearing(Black_crowned_night_heron$longitude[i] , 
                                                           Black_crowned_night_heron$latitude[i],
                                                           Black_crowned_night_heron$longitude[i+1], 
                                                           Black_crowned_night_heron$latitude[i+1] )
}
tail(Black_crowned_night_heron)
p1 = na.omit(Black_crowned_night_heron) 
p1$s = as.numeric(p1$s)
fit_BCNH = lm.circular(p1$theta , p1$s, init = 0.0 , type = "c-l" )
fit_BCNH$p.values
fit_BCNH
watson.test(Black_crowned_night_heron$theta , alpha = 0.05 , dist = "vonmises")
summary(fit_BCNH)


for (i in 1:121) {
  Black_bellied_plover$s[i] = Black_bellied_plover$G_h[i+1] - Black_bellied_plover$G_h[i] 
  Black_bellied_plover$theta[i] = get_initial_bearing(Black_bellied_plover$longitude[i] , 
                                                      Black_bellied_plover$latitude[i],
                                                      Black_bellied_plover$longitude[i+1], 
                                                      Black_bellied_plover$latitude[i+1] )
}
tail(Black_bellied_plover)
p2 = na.omit(Black_bellied_plover)
p2$s = as.numeric(p2$s)
fit_BBP = lm.circular(p2$theta , p2$s, init = 0.0, type = "c-l" )
fit_BBP$p.values
fit_BBP
watson.test(Black_bellied_plover$theta , alpha = 0.05 , dist = "vonmises")



for (i in 1:76) {
  Brown_pelican$s[i] = Brown_pelican$G_h[i+1] - Brown_pelican$G_h[i] 
  Brown_pelican$theta[i] = get_initial_bearing(Brown_pelican$longitude[i] , 
                                               Brown_pelican$latitude[i],
                                               Brown_pelican$longitude[i+1], 
                                               Brown_pelican$latitude[i+1] )
}
tail(Brown_pelican)
watson.test(Brown_pelican$theta , alpha = 0.05 , dist = "vonmises")
Brown_pelican$theta = as.numeric(Brown_pelican$theta)
Brown_pelican$s = as.numeric(Brown_pelican$s)
Brown_pelican = na.omit(Brown_pelican)
fit_BP = lm.circular(Brown_pelican$theta , Brown_pelican$s , init = 0.0 , type = "c-l")
fit_BP$p.values
fit_BP
for (i in 1:132) {
  Long_billed_curlew$s[i] = Long_billed_curlew$G_h[i+1] - Long_billed_curlew$G_h[i] 
  Long_billed_curlew$theta[i] = get_initial_bearing(Long_billed_curlew$longitude[i] , 
                                                    Long_billed_curlew$latitude[i],
                                                    Long_billed_curlew$longitude[i+1], 
                                                    Long_billed_curlew$latitude[i+1] )
}
tail(Long_billed_curlew)
watson.test(Long_billed_curlew$theta , alpha = 0.05 , dist = "vonmises")
Long_billed_curlew$s = as.numeric(Long_billed_curlew$s)
fit_LBC = lm.circular(Long_billed_curlew$theta , Long_billed_curlew$s , init = 0.0 , type = "c-l")
fit_LBC

for (i in 1:161) {
  Pacific_loon$s[i] = Pacific_loon$G_h[i+1] - Pacific_loon$G_h[i] 
  Pacific_loon$theta[i] = get_initial_bearing(Pacific_loon$longitude[i] , 
                                              Pacific_loon$latitude[i],
                                              Pacific_loon$longitude[i+1], 
                                              Pacific_loon$latitude[i+1] )
}
tail(Pacific_loon)
watson.test(Pacific_loon$theta , alpha = 0.05 , dist = "vonmises")
Pacific_loon$s = as.numeric(Pacific_loon$s)
fit_PL = lm.circular(Pacific_loon$theta , Pacific_loon$s , init = 0.0 , type = "c-l")
fit_PL


for (i in 1:167) {
  Swainsons_hawk$s[i] = Swainsons_hawk$G_h[i+1] - Swainsons_hawk$G_h[i] 
  Swainsons_hawk$theta[i] = get_initial_bearing(Swainsons_hawk$longitude[i] , 
                                                Swainsons_hawk$latitude[i],
                                                Swainsons_hawk$longitude[i+1], 
                                                Swainsons_hawk$latitude[i+1] )
}
sum(is.na(Swainsons_hawk$s))
tail(Swainsons_hawk)
watson.test(Swainsons_hawk$theta , alpha = 0.05 , dist = "vonmises")
x = c(Swainsons_hawk$s)
Swainsons_hawk = na.omit(Swainsons_hawk)
Swainsons_hawk$s = as.numeric(Swainsons_hawk$s)
sum(is.na(Swainsons_hawk$s))
fit_Sw = lm.circular(Swainsons_hawk$theta , x , init = 0.0 , type = "c-l")
fit_Sw$p.values
fit_Sw
bird_partition = dff$bird
colours =c("blue","red","green","brown","yellow", "pink")
library(flexmix)
library ( rgl )
library ( rglwidget )
theta = dff$longitude
phi = dff$latitude
x = sin ( phi )* cos ( theta )
y = sin ( phi )* sin ( theta )
z = cos ( phi )

d = cbind (x ,y , z )
s = d[,c(4 , 5, 14)]
#colours =c(" blue ","red"," green "," brown ")
p = plot3d (d,  col = colours[bird_partition]  , size =8)
rglwidget ( x = scene3d ( p ) , elementId = " spkm ")

arrows(0 ,0 , cos(circ.mean (new_data$latitude)) ,sin(circ.mean(new_data$latitude)) ,col= "red")
watson.test(Swainsons_hawk$longitude , alpha = 0.05 , dist = "vonmises")
watson.test(Swainsons_hawk$latitude , alpha = 0.05 , dist = "vonmises")

d1 = na.omit(Black_bellied_plover)
d2 = na.omit(Black_crowned_night_heron)
d3 = na.omit(Swainsons_hawk)
d4 = na.omit(Brown_pelican)
d5 = na.omit(Pacific_loon)
d6 = na.omit(Long_billed_curlew)
d11 = rbind(d1 , d2 , d3 , d4 , d5 , d6)
head(d11)
tail(d11)


bird_data = data.frame(bird = d11$species ,  lon = d11$longitude , lat = d11$latitude , direction= d11$theta)
head(d6)




world_map <- map_data("world")
northern_map <- subset(world_map, long <= 0)
# Create a basic plot using ggplot and specify the northern_map as the data source
plot <- ggplot() +
  geom_polygon(data = northern_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
  coord_fixed() +
  geom_segment(data = bird_data,
               aes(x = lon, y = lat, xend = lon + sin(direction * pi/180),
                   yend = lat + cos(direction * pi/180),
                   color = bird),
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last")) +
  scale_color_manual(values = c("Black-bellied plover" = "red", 
                                "Black-crowned night heron" = "blue", 
                                "Swainson's hawk" = "black",
                                "Brown pelican" = "brown",
                                "Pacific loon" = "purple",
                                "Long-billed curlew" = "pink")) +
  theme_map() +
  labs(color = "Bird") +
  theme(legend.position = "bottom")
print(plot)



sub_df61$part = "part 1"
sub_df611$part = "part 2"
sub_df612$part = "part 3"
sub_df613$part = "part 4"
sub_df614$part = "part 5"
sub_df615$part = "part 6"
sub_df616$part = "part 7"
sub_df617$part = "part 8"
sub_df618$part = "part 9"
sub_df619$part = "part 10"
sub_df6110$part = "part 11"
sub_df6111$part = "part 12"
sub_df6112$part = "part 13"
sub_df6113$part = "part 14"
sub_df6114$part = "part 15"
sub_df6115$part = "part 16"
sub_df6116$part = "part 17"
sub_df6117$part = "part 18"
sub_df6118$part = "part 19"
sub_df6119$part = "part 20"
sub_df6120$part = "part 21"
sub_df6121 = data.frame(col1 = Swainsons_hawk$latitude , col2  = Swainsons_hawk$longitude , part = "part 22")

df_part = rbind(sub_df61 ,  sub_df611,sub_df612,sub_df613,sub_df614,
                sub_df615,sub_df616,sub_df617,sub_df618,sub_df619, sub_df6110 , sub_df6111 , sub_df6112 , sub_df6113,
                sub_df6114 , sub_df6115, sub_df6116 , sub_df6117 , sub_df6118 , sub_df6119,
                sub_df6120, sub_df6121)
head(df_part)
tail(df_part)


library(maps)
world_map <- map_data("world")
northern_map <- subset(world_map, long <= 0)
# Create a basic plot using ggplot and specify the northern_map as the data source
plot <- ggplot() +
  geom_polygon(data = northern_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
  coord_fixed() +
  geom_point(
    data = df_part,
    aes(df_part$col2 , df_part$col1, color = df_part$part),
    alpha = 1
  ) 
theme(legend.position="top") # Fix the aspect ratio
# Customize the plot with additional layers and labels
plot <- plot +
  labs(title = "American Region") +
  xlab("Longitude") +
  ylab("Latitude")

# Print the plot
print(plot)
