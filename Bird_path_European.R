library(readr)
library(readxl)
data = read_csv("bird_migration.csv")
data = data.frame(data)
library(flexmix)
library ( rgl )
library ( rglwidget )
library(circular)
library(maps)
library(movMF)
library(ggplot2)
head(data)
Eric = data[1:19795,]
Nicco = data[19796:40916,]
Sanne  = data[40917:61921,]
tail(Eric)
watson.test(Eric$direction , alpha = 0.05 , dist = "vonmises")
watson.test(Sanne$direction, alpha = 0.05, dist = "vonmises")
watson.test(Nicco$direction, alpha = 0.05 , dist = "vonmises")

Eric$col = "red"
Nicco$col = "blue"
Sanne$col = "green"

plot(circular(Nicco$longitude) , col = d11$col , xlim=c(-1,1), ylim=c(-1.5, 1.5) , cex = 0.1 , lty = 1)
points(circular(Eric$longitude) , col = d12$col , cex = 0.1, lty = 2)
points(circular(Sanne$longitude) , col = d13$col , cex = 0.1, lty = 3)
lines(density.circular(Nicco$longitude , bw = 10) , col = "blue", lty = 4)
lines(density.circular( rvonmises(n=1000, mu=mean(Nicco$longitude), kappa=est.kappa(Nicco$longitude)), bw = 10) , col = "black" , lwd = 0.5, lty = 5)
legend("topleft", legend=c("Eric", "Nicco" ,"Sanne" , "Von mises Density"),
       col=c("red", "blue" , "green", "black"), lty=c(1:5),lwd=c(2,2), cex=0.59,
       box.lty=0)


plot(circular(Nicco$latitude) , col = d11$col , xlim=c(-1,1), ylim=c(-1.1, 1.5) , cex = 0.1 , lty = 1)
points(circular(Eric$latitude) , col = d12$col , cex = 0.1, lty = 2)
points(circular(Sanne$latitude) , col = d13$col , cex = 0.1, lty = 3)
lines(density.circular(Nicco$latitude , bw = 10) , col = "blue", lty = 4)
lines(density.circular( rvonmises(n=1000, mu=mean(Nicco$latitude), kappa=est.kappa(Nicco$latitude)), bw = 10) , col = "black" , lwd = 0.5, lty = 5)
legend("topleft", legend=c("Eric", "Nicco" ,"Sanne" , "Von mises Density"),
       col=c("red", "blue" , "green", "black"), lty=c(1:5),lwd=c(2,2), cex=0.59,
       box.lty=0)


world_map <- map_data("world")
library(maps)
world_map <- map_data("world")
northern_map <- subset(world_map, long <= 10)
northern_map1 <- subset(northern_map , long >= -25)
northern_map2 <- subset(northern_map1 , lat >= 10)
northern_map3 <- subset(northern_map2 , lat <= 55)
# Create a basic plot using ggplot and specify the northern_map as the data source
plot <- ggplot() +
  geom_polygon(data = northern_map3, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
  coord_fixed() +
  geom_segment(data = data,
               aes(x = data$longitude, y = data$latitude, xend = data$longitude + sin(data$direction * pi/180),
                   yend = data$latitude + cos(data$direction * pi/180),
                   color = data$bird_name),
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last")) +
  scale_color_manual(values = c("Eric" = "red", 
                                "Nicco" = "green", 
                                "Sanne" = "black"
  )) +
  theme_map() +
  labs(color = "Bird") +
  theme(legend.position = "bottom")
print(plot)


new_data = data

# Create a basic plot using ggplot and specify the northern_map as the data source
plot <- ggplot() +
  geom_polygon(data = northern_map3, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
  coord_fixed() +
  geom_point(
    data = new_data,
    aes(new_data$longitude , new_data$latitude, color = new_data$bird_name,),
    alpha = 1
  ) 
theme(legend.position="top") # Fix the aspect ratio
# Customize the plot with additional layers and labels
plot <- plot +
  labs(title = "European Region") +
  xlab("Longitude") +
  ylab("Latitude")

head(data)

# Assuming your timestamp is stored as a character string
timestamp <- data$date_time

# Convert the timestamp to a POSIXct object
posix_timestamp <- as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S")

# Extract the date and time components
data$date <- format(posix_timestamp, "%Y-%m-%d")
data$time <- format(posix_timestamp, "%H:%M:%S")

library(lubridate)

# Assuming you have the date in the format "YYYY-MM-DD"
date_string <- data$date

# Convert the date string to a date object
date <- ymd(date_string)
head(data)
# Get the number of the day in a year
data$dayOfYear <- yday(date)
new_data = data[,-c(2)]
head(new_data)
new_data$delta = 23.45*sin(0.986*(new_data$dayOfYear + 284))
new_data$h = asin(sin(new_data$latitude)*sin(new_data$delta) + cos(new_data$latitude)*cos(new_data$delta))
new_data$The_turbidity_factor = 0.796 - 0.01*sin(0.986*(new_data$dayOfYear + 284))       
new_data$C_t = 1 + 0.034*cos(new_data$dayOfYear - 2)
new_data$I_h = 1367*new_data$C_t*new_data$The_turbidity_factor*exp(-0.13/sin(new_data$h))*sin(new_data$h)
head(new_data)
new_data$D_h = 120*new_data$The_turbidity_factor*exp(-1/(0.4511+ sin(new_data$h)))
new_data$G_h = new_data$I_h + new_data$D_h
head(new_data)
filename <- "birdpath_rad_data_part2.csv"  # Specify the filename and path as desired
write.csv(new_data, file = filename, row.names = FALSE)
summary(new_data)
head(Eric)
Eric = new_data[1:19795,]
Nicco = new_data[19796:40916,]
Sanne  = new_data[40917:61921,]
sum(is.infinite(new_data$G_h))

F_Eric <- na.omit(Eric[is.finite(Eric$G_h), ])
F_Eric$G_h = log(F_Eric$G_h)
F_Eric = na.omit(F_Eric)
summary(F_Eric)

F_Nicco <- na.omit(Nicco[is.finite(Nicco$G_h), ])
F_Nicco$G_h = log(F_Nicco$G_h)
F_Nicco = na.omit(F_Nicco)
summary(F_Nicco)

F_Sanne <- na.omit(Sanne[is.finite(Sanne$G_h), ])
F_Sanne$G_h = log(F_Sanne$G_h)
F_Sanne = na.omit(F_Sanne)
summary(F_Sanne)
dim(F_Nicco)
#head(F_Eric)
#sum(is.na(F_Sanne))

length(F_Eric$direction)
length(F_Eric$G_h)
head(new_data)

library(solarPos)
library(suncalc)
library(lubridate)
date_str <- new_data$date

# Convert the date string to a Date object using dmy() function (day-month-year format)
date <- ymd(date_str)

# Extract year, month, and day
data$year <- year(date)
data$month <- month(date)
data$day <- day(date)

time_string <- data$time

# Convert the time string to a proper time object
time_obj <- hms(time_string)

# Extract the hour, minutes, and seconds
data$hour <- hour(time_obj)
data$minutes <- minute(time_obj)
data$seconds <- second(time_obj)

head(data)
data$JD = solarPos::julianDay(data$year , data$month , data$day, data$hour , data$minutes , data$seconds )
head(data)
x = solarPos::solarPosition(data$JD , data$longitude , data$latitude , data$altitude)
head(x)
data = data.frame(data , x)
head(data)
watson.test(data$azimuth , alpha = 0.05 , dist = "vonmises")
watson.test(data$zenith , alpha = 0.05 , dist = "vonmises")
filename <- "European Bird Path for Solar position.csv"  # Specify the filename and path as desired
write.csv(data, file = filename, row.names = FALSE)

Eric = data[1:19795,]
head(Eric)
Nicco = data[19796:40916,]
Sanne  = data[40917:61921,]

watson.test(Eric$azimuth , alpha = 0.05 , dist = "vonmises")
watson.test(Eric$zenith , alpha = 0.05 , dist = "vonmises")
watson.test(Nicco$azimuth , alpha = 0.05 , dist = "vonmises")
watson.test(Nicco$zenith , alpha = 0.05 , dist = "vonmises")
watson.test(Sanne$azimuth , alpha = 0.05 , dist = "vonmises")
watson.test(Sanne$zenith , alpha = 0.05 , dist = "vonmises")
head(data)
library(suncalc)
df1 = data.frame(date = Eric$date , lat = Eric$latitude , lon = Eric$longitude)
df2 = data.frame(date = Sanne$date, lat = Sanne$latitude , lon = Sanne$longitude)
df3 = data.frame(date = Nicco$date , lat = Nicco$latitude , lon = Nicco$longitude)
Eric_m = suncalc::getMoonPosition(data = df1)
Nicco_m = suncalc::getMoonPosition(data = df3)
Sanne_m = na.omit(suncalc::getMoonPosition(data = df2))
sum(is.na(Sanne_m))


filename <- "Eric bird Path for Moon position.csv"  # Specify the filename and path as desired
write.csv(Eric_m, file = filename, row.names = FALSE)
filename <- "Nicco bird Path for Moon position.csv"  # Specify the filename and path as desired
write.csv(Nicco_m, file = filename, row.names = FALSE)
filename <- "Sanne bird Path for Moon position.csv"  # Specify the filename and path as desired
write.csv(Sanne_m, file = filename, row.names = FALSE)

head(Eric)
f1 = lm.circular(Nicco$direction , Nicco_m$azimuth)
f2 = lm.circular(Sanne$direction , Sanne_m$azimuth)
f3 = lm.circular(Eric$direction , Eric_m$azimuth)
fit1 = lm.circular(Eric$direction , Eric$azimuth)
fit11 = lm.circular(Eric$direction , Eric$zenith)
fit2 = lm.circular(Sanne$direction , Sanne$azimuth)
fit22 = lm.circular(Sanne$direction , Sanne$zenith)
fit3 = lm.circular(Nicco$direction , Nicco$zenith)
fit33 = lm.circular( Nicco$direction,Nicco$azimuth)

fit1$rho
fit2$rho
fit3$rho
fit11$rho
fit22$rho
fit33$rho
f1$rho
f2$rho
f3$rho

e = cbind(data$latitude , data$longitude)
set.seed(2022)
EvMFs <- 
  function(K){
    movMF(e, k = K, control= list(nruns = 20))
  }

Esd = lapply(1:15, EvMFs)
gt = sapply(Esd, BIC)
gt
min(gt)


Esd[[1]]$details # For K value 1
Esd[[2]]$details # For K value 2
Esd[[3]]$details # For K value 3
Esd[[4]]$details # For K value 4
Esd[[5]]$details # For K value 5
Esd[[6]]$details # For K value 6
Esd[[7]]$details # For K value 7
Esd[[8]]$details # For K value 8
Esd[[9]]$details # For K value 9
Esd[[10]]$details # For K value 10
Esd[[11]]$details # For K value 11
Esd[[12]]$details # For K value 12
Esd[[13]]$details # For K value 13
Esd[[14]]$details # For K value 14
Esd[[15]]$details # For K value 15