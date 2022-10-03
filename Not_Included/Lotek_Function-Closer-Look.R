
  list.of.packages <- c("chron", "geosphere","proj4","rgdal","sp","maptools")
  
# check if any of the required packages are not installed on your computer 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# install any missing packages
# TODO missing conditional logic?
if(length(new.packages)) install.packages(new.packages)

# attach required packages  
  library(chron)
  library(geosphere)
  library(proj4)
  library(rgdal)
  library(sp)

# TODO deprecation warning for `maptools` package  
library(maptools)

##load in fix data file and check to make sure data looks ok
Fix_file <- here::here("Supplemental", "Fix_example.txt")

dataF=read.table(Fix_file,
                 sep=",",
                 header=T, 
                 col.names = c("Fix",
                               "Fix.Date.Time", 
                               "Latitude" , 
                               "Longitude",
                               "Altitude",
                               "Time" ,
                               "Temp",
                               "Fix.Status",
                               "Sats",
                               "DOP" ))
  
## remove rows with NA values
dataF=na.omit(dataF)
  
##load in sensor data file and check to make sure data looks ok

Sensor_file <- here::here("Supplemental", "Sensor_example.txt")

dataS = read.table(Sensor_file,
                   sep=",",
                   header=TRUE,
                   col.names = c("Record", 
                                 "Date.Time", 
                                 "X.Act", 
                                 "Y.Act", 
                                 "X..Head.Down", 
                                 "Temp"))
  
##convert lotek fix/date/time info into date column
# converts a double into an integer
date=as.integer(dataF$Fix.Date.T)

dataF$Date=chron::chron(dates=date,
                 out.format="m/d/y",
                 origin=c(month=12,day=30,year=1899))

##convert fix/date/time info into time column
  Tm=dataF$Fix.Date.T-date
  dd=Tm*24
  hour=as.integer(dd)
  m=dd-hour
  min=as.integer(m*60)
  s=(m*60)-min
  sec=as.integer(s*60)
  Time=paste(hour,":",min,":",sec)
  dataF$time=times(Time)
  min=floor(min/5)
  min=min*5
  Time2=paste(hour,":",min,":",00)
  TimeS=times(Time2)
  
  
  ##combine date and time into one vector to nearest five minute interval
  dataF$Date_Time=paste(dataF$Date,TimeS)
  
  ##convert lotek sensor file date/time into date column
  dateS=as.integer(dataS$Date.Time)
  dataS$Date=chron(dates=dateS,out.format="m/d/y",origin=c(month=12,day=30,year=1899))
  ##convert sensor date/time info into time column
  Tm2=dataS$Date.Time-dateS
  dataS$Time=chron(times=Tm2)
  
  ##combine date and time into one vector
  dataS$Date_Time=paste(dataS$Date,dataS$Time)
  
  
  ##merge fix data frame and sensor data frame based on identical date time values
  data_comb=merge(dataF,dataS,by.x="Date_Time",by.y="Date_Time")
  
  data_comb$Animal_ID=Animal_ID
  
  
  ## create points object with 2 variables longitude and latitude
  pts <- data_comb[c("Longitude", "Latitude")]  
  ## calculate distance between consecutive points
  dists <- distHaversine(p1 = pts[-nrow(data_comb),], p2 = pts[-1,])
  ## add zero value for first data point
  dists=c(0,dists)
  ## add distance values to table
  data_comb$distance=dists
  ## add in directional bearing in degrees
  direction <- bearing(p1 = pts[-nrow(data_comb),], p2 = pts[-1,])
  direction=c(0,direction)
  data_comb$bearing=direction
  ##Duration between points
  date2=as.integer(data_comb$Fix.Date.Time)
  Tm3=data_comb$Fix.Date.T-date2
  duration=diff(Tm3)*24*60
  duration=c(0,duration)
  data_comb$duration=duration
  
  data_comb$Time=NULL
  data_comb$Fix.Date.Time=NULL
  data_comb$Date.Time=NULL
  data_comb$Sats=NULL
  #data_comb$Fix.Status=NULL
  data_comb$Record=NULL
  data_comb$Temp.y=NULL
  data_comb$Time.x=NULL
  
  
  
  #if Graze chosen
  #colnames(data_comb)=c("Date_Time","Fix","Latitude","Longitude","Altitude","Temp","Fix.Status","DOP","Fix_Date","Fix_Time",
  #"X_act","Y_act","Head_Down","Sensor_date","Sensor_time","Animal_ID","distanceM","bearing","duration","X_Y_Sum","Graze")
  
  
  
  
  
  
  
  #if graze not chosen
  
  colnames(data_comb)=c("Date_Time","Fix","Latitude","Longitude","Altitude","Temp","Fix.Status","DOP","Fix_Date","Fix_Time",
                        "X_act","Y_act","Head_Down","Sensor_date","Sensor_time","Animal_ID","distanceM","bearing","duration")
  
  if (Graze=="Yes"|Graze=="Y" | Graze=="yes" | Graze=="YES"| Graze=="y" ){
    
    names(data_comb)
    data_comb$X_Y_Sum=data_comb$X_act+data_comb$Y_act
    hd=data_comb$Head_Down
    x=data_comb$X_act
    y=data_comb$Y_act
    xy_sum=data_comb$X_Y_Sum
    dis=data_comb$distance
    dur=data_comb$duration     
    ## Selects graze points based on distance traveled and sensor variables 
    select1=(hd>=99&dis>=6.6&dis<127.6&dur<9)|
      (hd>=94.7&dis>=6.6&dis<127.6&hd<99&dis>=21.1&dur<9)|
      (hd>=94.7&dis>=6.6&dis<127.6&hd<99&dis<21.1&x>=16&dur<9)|
      (hd>=94.7&dis>=6.6&dis>=127.6&dis<183.3&xy_sum>=63&dur<9)|
      (hd>=94.7&dis<6.6&x>=76)
    
    ## Create data column in data frame where G=Graze points and NG= non graze points
    data_comb$Graze=select1
    data_comb$Graze[data_comb$Graze=="TRUE"]="G"
    data_comb$Graze[data_comb$Graze=="FALSE"]="NG" 
    
    ########change classification to NC or not classified for points that duration is greater than 9 minutes
    data_comb[data_comb$duration >9, "Graze"] <- "NC"
  }
  
  
  
  
  ##convert date/time data to character strings for shapefile output
  data_comb$Sensor_time=as.character(data_comb$Sensor_time)
  data_comb$Sensor_date=as.character(data_comb$Sensor_date)
  data_comb$Fix_Date=as.character(data_comb$Fix_Date)
  data_comb$Fix_Time=as.character(data_comb$Fix_Time)
  #class(data_comb$Date_Time)
  
  ## create points in spatial data frame format
  points=SpatialPointsDataFrame(coords=pts,proj4string=CRS("+init=epsg:4326"),data=data_comb)
  ## bring in shapefile you will likely need to change epsg number to accomodate your coordinate system working in
  
  
  #shp <- readShapeSpatial(shp_file, proj4string=CRS("+init=epsg:4269"))
  ## overlay points that fall within prairie dog colony
  #Overlay_pts= over(points,shp)
  
  
  ######change Overlay_pts$Site to name of column you want to extract data from
  #data_comb$Site=as.character(Overlay_pts$Site)
  
  Out=paste(substr(Fix_file, 1, nchar(Fix_file)-4),"_Processed.csv",sep="")
  write.csv(data_comb,file=Out)
  output=SpatialPointsDataFrame(coords=pts,proj4string=CRS("+init=epsg:4326"),data=data_comb)
  
  writeOGR(obj=output,dsn=getwd(),layer= substr(Fix_file, 1, nchar(Fix_file)-4) ,driver="ESRI Shapefile")
  
