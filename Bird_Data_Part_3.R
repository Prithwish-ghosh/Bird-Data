new_data = read_csv("birdpath_rad_data.csv")
new_data = data.frame(new_data)
head(new_data)
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

##################### Julian Day ############################

calculate_julian_day <- function(year, month, day) {
  jd <- (1461 * (year + 4800 + (month - 14) / 12)) / 4 +
    (367 * (month - 2 - 12 * ((month - 14) / 12))) / 12 -
    (3 * ((year + 4900 + (month - 14) / 12) / 100)) / 4 + day - 32075
  return(jd)
}


# Load the lubridate package
library(lubridate)

# Given date
date_str <- new_data$date

# Convert the date string to a Date object using dmy() function (day-month-year format)
date <- dmy(date_str)

# Extract year, month, and day
new_data$year <- year(date)
new_data$month <- month(date)
new_data$day <- day(date)

# Apply the function to calculate Julian Day for each row in the data frame
new_data$julian_day <- calculate_julian_day(new_data$year, new_data$month, new_data$day)

# Print the resulting data frame with Julian Day column
head(new_data)
tail(new_data)

#################Sun Position Calculation ###########################
new_data$n = new_data$julian_day - 2451545
new_data$L = 280.466 + .9856474* new_data$n
new_data$g = 357.528 + 0.9856003 * new_data$n
new_data$lamda = new_data$L + 1.915 * sin(new_data$g) + 0.020 * sin(2* new_data$g)
new_data$epsilon = 23.440 - 0.0000004 * new_data$n
new_data$alpha = atan(cos(new_data$epsilon) *tan (new_data$lamda)) * 180/pi
new_data$Deltaa = asin(sin(new_data$epsilon) *  sin(new_data$lamda))* 180/pi
new_data$RES = 1.00014 - 0.01671 * cos(new_data$g) - 0.00014 * cos(2*new_data$g) 
new_data$Emin = (new_data$L - new_data$alpha) * 4
new_data$lamda_S = -15* (0 - 12 + new_data$Emin/60)
new_data$Sx = cos(new_data$Deltaa) * sin(new_data$lamda_S - new_data$longitude)
new_data$Sy = cos(new_data$latitude) * sin(new_data$Deltaa) - sin(new_data$latitude)* cos(new_data$Deltaa)* cos(new_data$lamda_S - new_data$longitude)
new_data$Sz = cos(new_data$latitude) * sin(new_data$Deltaa) - cos(new_data$latitude)* cos(new_data$Deltaa)* cos(new_data$lamda_S - new_data$longitude)
head(new_data)

################# Solar Zenith Angle and Azimuth angle ##################################
library(solarPos)
x = solarPos::solarPosition(new_data$julian_day , new_data$longitude , new_data$latitude)
new_data = data.frame(new_data , x)
head(new_data)
################## Bird Direction #####################################

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
fit1 = lm.circular(Black_crowned_night_heron$theta , Black_crowned_night_heron$zenith)
fit11 = lm.circular(Black_crowned_night_heron$theta , Black_crowned_night_heron$azimuth)
fit1$rho
fit11$rho
watson.test(Black_crowned_night_heron$zenith , alpha = 0.05 , dist = "vonmises")
watson.test(Black_crowned_night_heron$azimuth , alpha = 0.05 , dist = "vonmises")



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
watson.test(Black_bellied_plover$zenith , alpha = 0.01 , dist = "vonmises")
watson.test(Black_bellied_plover$azimuth , alpha = 0.05 , dist = "vonmises")
fit2 = lm.circular(Black_bellied_plover$theta , Black_bellied_plover$zenith)
fit22 = lm.circular(Black_bellied_plover$theta , Black_bellied_plover$azimuth)
fit2$rho
fit22$rho

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
watson.test(Brown_pelican$azimuth , alpha = 0.01 , dist = "vonmises")
watson.test(Brown_pelican$zenith , alpha = 0.05 , dist = "vonmises")
fit3 = lm.circular(Brown_pelican$theta , Brown_pelican$zenith)
fit33 = lm.circular(Brown_pelican$theta , Brown_pelican$azimuth)
fit3$rho
fit33$rho

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
watson.test(Long_billed_curlew$azimuth , alpha = 0.05 , dist = "vonmises")
watson.test(Long_billed_curlew$zenith , alpha = 0.05 , dist = "vonmises")
fit4 = lm.circular(Long_billed_curlew$theta , Long_billed_curlew$azimuth)
fit44 = lm.circular(Long_billed_curlew$theta , Long_billed_curlew$zenith)
fit4$rho
fit44$rho

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
watson.test(Pacific_loon$azimuth , alpha = 0.05 , dist = "vonmises")
watson.test(Pacific_loon$zenith , alpha = 0.05 , dist = "vonmises")
fit5 = lm.circular(Pacific_loon$theta , Pacific_loon$zenith)
fit55 = lm.circular(Pacific_loon$theta , Pacific_loon$azimuth)
fit55$rho
fit5$rho

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
watson.test(Swainsons_hawk$azimuth , alpha = 0.05  , dist = "vonmises")
watson.test(Swainsons_hawk$zenith , alpha = 0.05 , dist = "vonmises")
fit6 = lm.circular(Swainsons_hawk$theta , Swainsons_hawk$zenith)
fit66 = lm.circular(Swainsons_hawk$theta , Swainsons_hawk$azimuth)
fit6$rho
fit66$rho

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
