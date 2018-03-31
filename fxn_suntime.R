SunTime<-function(ClockTime,day,month,lat,long,timeZone,RS) {
  
  lo<-long*12/180
  d2r<-pi/180
  m<-c(31,28,31,30,31,30,31,31,30,31,30,31)
  j<-rep(8,length(day))
  for (i in 1:length(day)){
    j[i]<-sum(m[0:(month[i]-1)])+day[i]
  }
  
  # mean anomaly
  Mo<-357.5291*d2r
  M1<-0.9856*d2r
  M<-Mo+M1*j
  
  # ellipse contribution
  e<-0.01671
  C<-(2*e-e^3/4)*sin(M)+5/4*e^2*sin(2*M)+13/12*e^3*sin(3*M)
  
  # ecliptic longitude
  lambda<-280.47*d2r+M1*j+C
  
  # earth's tilt contribution
  epsilon<-23.45*d2r
  R<-(-epsilon^2/4-epsilon^4/24-17/2880*epsilon^6)*sin(2*lambda)+(epsilon^4/32+epsilon^6/96)*sin(4*lambda)-epsilon^6*sin(6*lambda)/192
  
  # equation of Time
  Dt<-4*(C+R)
  
  # declination
  Dc<-asin(sin(epsilon)*sin(lambda))
  
  # sun height
  r<-34
  d<-32
  b<-0
  hs<- -r-d/2+b
  
  # hour angle
  cosHa<-(sin(hs*d2r/60)-sin(Dc)*sin(lat*d2r))/cos(Dc)/cos(lat*d2r)
  Ha<-acos(cosHa)/15/d2r
  
  # Time of sunrise
  Hrise<-12+Dt-Ha+lo+timeZone
  Hset<-12+Dt+Ha+lo+timeZone
  
  # sun Time
  nRise<-(RS==1)
  nSet<-(RS==0)
  SunTime<-rep(0,length(RS))
  SunTime[nRise]<-ClockTime[nRise]-Hrise[nRise]
  SunTime[nSet]<-ClockTime[nSet]-Hset[nSet]
  
  # Output
  Output<-cbind(SunTime,ClockTime,Hrise,Hset,day,month,lat,long,timeZone,RS)
  
  return(Output) 
}










####################################
#Function to calculate sunset time#
##################################
sunset.calc <- function (lat, long, date, timezone = "UTC") 
{
  lat.long <- matrix(c(long, lat), nrow = length(long))
  day <- as.POSIXct(date, tz = timezone)
  sunset <- maptools::sunriset(lat.long, day, direction = "sunset", 
                     POSIXct = TRUE)
  ss <- data.frame(sunset)
  ss <- ss[, -c(1, 3)]
  return(ss)
}


sunrise.calc <- function (lat, long, date, timezone = "UTC") 
{
  lat.long <- matrix(c(long, lat), nrow = length(long))
  day <- as.POSIXct(date, tz = timezone)
  sunset <- maptools::sunriset(lat.long, day, direction = "sunrise", 
                     POSIXct = TRUE)
  sr <- data.frame(sunset)
  sr <- sr[, -c(1, 3)]
  
  return(sr)
}
