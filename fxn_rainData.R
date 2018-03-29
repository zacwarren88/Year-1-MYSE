# Get Rain data from NOAA -------------------------------------------------

##inputs
###startDT (endDT) - Start and end date and time for desired data
####posixct or string in format="%Y%m%d %H:%M:%S" 
####if string, assumes R session time zone if not specified
###SaveLocation - string of file location data should be saved to
#### do not include ending slash in file path

f.getRain <- function(startDT, 
                      endDT,
                      saveLocation
){
  
  if(is.na(file.info(saveLocation)$isdir)) stop("Save Directory does not exist, Please create and try again")
  
  
  # build seq
  require(lubridate)
  startDT=with_tz(as.POSIXct(startDT, format="%Y%m%d %H:%M:%S"), "GMT")
  endDT=with_tz(as.POSIXct(endDT, format="%Y%m%d %H:%M:%S"), "GMT")
  
  times = seq(startDT, endDT, by="1 hour")
  
  # build filenames
  ## http://www.srh.noaa.gov/data/ridge2/Precip/qpehourlyshape/YYYY/YYYYMM/YYYYMMDD
  
  baseURL= "http://www.srh.noaa.gov/data/ridge2/Precip/qpehourlyshape"
  folder = format(times,"/%Y/%Y%m/%Y%m%d/")
  filename =  paste0(saveLocation, format(times, "\\nws_precip_%Y%m%d%H.tar.gz"))
  URL=paste0(baseURL, format(times,"/%Y/%Y%m/%Y%m%d/nws_precip_%Y%m%d%H.tar.gz"))
  path = format(times,"%Y/%Y%m/%Y%m%d/nws_precip_%Y%m%d%H.dbf")
  rainFiles <- data.frame(times=times, folder= folder, URL = URL, destfile=filename, path=path)  
  
  
  # Download weather
  
  apply(rainFiles, 1, function(x) download.file(url=x["URL"], destfile=x["destfile"]))
  
  # Read dbf
  apply(rainFiles, 1, function(x) untar(x["destfile"], files=x["path"], exdir = saveLocation))
  l.data <- apply(rainFiles, 1, function(x) foreign::read.dbf(paste0(saveLocation,"\\", x["path"])))
  
  # combine dbf and return
  names(l.data) <- rainFiles$times
  data <- plyr::ldply(l.data, .id="Time")
  
  save(data, file= paste0(saveLocation, "\\Rain_",format(startDT, "%Y%m%d%H"), "-", format(endDT, "%Y%m%d%H"), ".Rdata"))
  return(data)
  
}


############################################################
###Interpret rain data downloaded from the f.getRain fxn###
##########################################################

f.interpRain <- function(rain.df,
                         points.df,
                         ID = NULL,
                         cLon = "POINT_X",
                         cLat = "POINT_Y",
                         cStart = "START_DATE",
                         cEnd = "END_DATE",
                         prj = "+init=epsg:4269")
{
  require(sp)
  require(maptools)
  require(dplyr)
  require(spatstat)
  
  ###Load lists of all HRAP prediction points
  
  ##Create a df of all HRAP points
  temp <- tempfile() #create a temp location
  temp2 <- tempfile()
  download.file("https://water.weather.gov/precip/archive/nws_precip_allpoint.tar.gz",temp) #download zip drive to temp loc
  untar(temp, exdir = temp2, files = "nws_precip_allpoint.dbf") #unzip dbf file with rain points
  rainsites.df <- foreign::read.dbf(paste0(temp2,"\\nws_precip_allpoint.dbf")) #read in dbf as a dataframe
  unlink(temp)#delete tarball
  unlink(temp2)#delete dbf file
  
  
  points.df %>% 
    rename_("X" = cLon,
            "Y" = cLat,
            "START" = cStart,
            "END" = cEnd) %>% 
    ungroup() %>% 
    mutate(X = as.numeric(X),
           Y = as.numeric(Y),
           START = lubridate::with_tz(as.POSIXct(START, format = "%Y%m%d"), "GMT"),
           END =  lubridate::with_tz(as.POSIXct(END, format = "%Y%m%d"), "GMT")) -> points.df
  
  if (is.null(ID) == TRUE) {
    # Create an ID column from the X and Y if ID is not provided
    points.df %>% 
      mutate(ID = paste0(X,Y)) -> points.df #concatonate the x and y column
    
  }else {
    #If ID is provided then rename the column ID
    points.df %>% 
      rename_("ID" = ID) -> points.df 
  }
  
  ##create bounding box to limit possibilities
  xmin = min(points.df$X)
  xmax = max(points.df$X)
  ymin = min(points.df$Y)
  ymax = max(points.df$Y)
  
  #subset the rain data to speed processing time
  rainsites.df %>% 
    filter (LON > xmin - 0.5 & LON < xmax + 0.5) %>% #filter by bounding box
    filter (LAT > ymin - 0.5 & LAT < ymax + 0.5) %>%
    select(-ID) %>%  #remove old ID col
    tidyr::unite (ID, HRAPX, HRAPY, sep="") %>% #Combine HRAPX with HRAPY to make a new col
    mutate(ID = as.numeric(ID)) %>% #convert ID to numeric
    select (ID, LAT, LON) -> rainsites.df
  
  #find bounding box to create a ppp object
  rain.xmin = min(rainsites.df$LON)
  rain.xmax = max(rainsites.df$LON)
  rain.ymin = min(rainsites.df$LAT)
  rain.ymax = max(rainsites.df$LAT)
  
  #projection of HRAP data
  hrap.crs = CRS('+proj=longlat +a=6371200 +b=6371200 +no_defs')
  
  #Reproject user data
  data.sp <- as.data.frame(points.df) #convert from tbl to dataframe                 
  coordinates(data.sp) <- ~ X + Y #set coords
  proj4string(data.sp) <- CRS(prj) #define projection
  data.sp <- spTransform(data.sp, hrap.crs) #transform to HRAP projection
  
  #convert from sp object to ppp object for USER data
  data.ppp <- as(data.sp["ID"], "ppp")
  
  #Create a ppp object of HRAP data
  rain.ppp <- ppp(x = rainsites.df$LON, 
                  y = rainsites.df$LAT, 
                  window = owin(xrange = c(rain.xmin, rain.xmax), 
                                yrange = c(rain.ymin, rain.ymax)), 
                  marks = rainsites.df$ID)
  
  #calculate nearest neighbor
  nearsite.df <- nncross(data.ppp, rain.ppp)
  class(rain.ppp)
  #retrieve user id values
  nearsite.df$user.id <- data.ppp$marks
  
  #Create a df that connects row num with unique ID
  rain.join.df <- data.frame(ID = rain.ppp$marks) #create a dataframe where ID is marks from ppp
  rain.join.df$which.id <- 1:nrow(rain.join.df) #create a which.id col that is = to rownum
  
  nearsite.df %>% #take near output
    left_join(rain.join.df, by = c("which" = "which.id")) %>% #join which by which id for rain data
    left_join(points.df, by = c("user.id" = "ID")) %>% #bring in the user data
    # rename(user.lat = Lat, #uncomment for bug checking
    #        user.long = Long) %>% 
    left_join(rainsites.df, by = "ID") %>% #join in 
    select(user.id, ID, X, Y, START, END) -> nearsite.df
  
  
  ###############################################
  ###Join user data with downloaded rain data###
  #############################################
  
  rain.df %>% 
    select(-Id) %>%  #remove old ID col
    tidyr::unite (ID, Hrapx, Hrapy, sep="") %>% #Combine HRAPX with HRAPY to make a new col
    mutate(ID = as.numeric(ID)) %>% #convert ID col to numeric
    left_join (nearsite.df, by = "ID") %>% #join user data to rain data
    filter(!is.na(user.id)) %>% #remove rain data that isn't associated with a user defined point
    mutate (Date = lubridate::as_date(Time), #convert rain's Time col to date format and extract date
            Time = lubridate::as_datetime(Time)) %>% #convert rains time col to date/time format and replace
    filter(END >= Date & Date >= START) %>% #remove rows where rain data doesn't fall between start and end day
    mutate(POINT = paste0(X,Y)) %>% #create an point col that concatenates x and y
    select(user.id, Time, Globvalue, POINT) %>% #select only neede cols
    rename(ID = user.id, #rename to consistent format
           TIME = Time, 
           PRECIP = Globvalue) -> rain.df
}
