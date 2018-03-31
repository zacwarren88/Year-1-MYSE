#########################################################################################################
###Aggregate summary files within a folder structure into a single long data frame with count and MLE###
#######################################################################################################
#Updated for 4.1.0

f.aggregateIDSUMMARYlong <- function(folder.IDfiles, pattern.summaryfiles = "idsummary\\.csv", multifolder = T) {
  
    require(tidyverse)
    require(zoo)
    
    #list all files within folder structure
    files.IDSUMMARY <-
      list.files( #list files
        folder.IDfiles, #folder path designated above
        pattern = pattern.summaryfiles, #pattern, default is idsummary
        full.names = T, 
        recursive = multifolder #look down into folder structure
      )
    
    
    
    #remove potential .bak1 files
    files.IDSUMMARY <- files.IDSUMMARY[lapply(files.IDSUMMARY, function(x) length(grep("bak1", x, value = FALSE))) == 0]
    
    ## Read all summary files into a list
    l.MLE.csv <- lapply(files.IDSUMMARY, function(fn)
      {
      
      
      ############
      ##MLE data#
      ##########
      t <- suppressMessages(suppressWarnings(read_csv(fn, skip = 1))) %>% #read in file
        select(matches("X[0-9]*"), matches("^[A-Z]{6}\\_1$")) %>% #select columns with either SPECIES name or X as header (files or MLE)
        select(which(colMeans(is.na(.))<1)) #remove blank columns
      
      
      
      ###Fill in sites and deployments
      columnstofill <- grep("X[0-9]*", names(t)) #find columns with X in header
      t[, columnstofill]<- sapply(t[, columnstofill],FUN = function(x) na.locf(x)) #fill NA with most recent non-NA
      t <- t[!apply(t, 1, function(r) any(r %in% c("*"))),] #remove rows with * in either site or deployment
      
      #Convert dump 6 digit codes to 4 digit codes
      
      names(t)[matches("EPTFUS_1", vars = names(t))] <- "EPFU"
      names(t)[matches("LASNOC_1", vars = names(t))] <- "LANO"
      names(t)[matches("LASCIN_1", vars = names(t))] <- "LACI"
      names(t)[matches("NYCHUM_1", vars = names(t))] <- "NYHU"
      names(t)[matches("LASBOR_1", vars = names(t))] <- "LABO"
      names(t)[matches("MYOSEP_1", vars = names(t))] <- "MYSE"
      names(t)[matches("MYOLUC_1", vars = names(t))] <- "MYLU"
      names(t)[matches("PERSUB_1", vars = names(t))] <- "PESU"
      names(t)[matches("MYOCIL_1", vars = names(t))] <- "MYCI"
      names(t)[matches("MYOVOL_1", vars = names(t))] <- "MYVO"
      names(t)[matches("MYOTHY_1", vars = names(t))] <- "MYTH"
      
      
      #convert from wide to long
      t <- t %>% 
        gather("SPECIES", "MLE", matches("^[A-Z]{4}$"))  #convert from wide to long
      
      ##change last (in order) unnamed column to "NIGHT"
      names(t)[max( matches("X[0-9]*", vars = names(t)) )] <- "NIGHT"
      
      ###CHANGE DEPENDING ON PROJECT###
      ##change first unnamed column to "SITE"
      names(t)[min( matches("X[0-9]*", vars = names(t)) )] <- "SITE"
      
      ##change remaining unnamed column to "deployment"
      names(t)[matches("X[0-9]*", vars = names(t))] <- "DEPLOYMENT"
      
      ##############
      #Count data##
      ############
      #Download data
      c <- suppressMessages(suppressWarnings(read_csv(fn, skip = 1))) 
      
      #df with only site, depl, and night
      x <- c %>% 
        select(matches("X[0-9]*")) %>% 
        select_if(colSums(!is.na(.)) > 0)
      
      #df with only species
      s <- c %>% 
        select(matches("^[A-Z]{6}$"))
      
      #combine species with night, depl, and site
      c <- cbind(x,s)
      
      
      
      ###Fill in sites and deployments
      columnstofill <- grep("X[0-9]*", names(c)) #find columns with X in header
      c[, columnstofill]<- sapply(c[, columnstofill],FUN = function(x) na.locf(x)) #fill NA with most recent non-NA
      c <- c[!apply(c, 1, function(r) any(r %in% c("*"))),] #remove rows with * in either site or deployment
      
      
      #Convert dump 6 digit codes to 4 digit codes
      
      names(c)[matches("EPTFUS", vars = names(c))] <- "EPFU"
      names(c)[matches("LASNOC", vars = names(c))] <- "LANO"
      names(c)[matches("LASCIN", vars = names(c))] <- "LACI"
      names(c)[matches("NYCHUM", vars = names(c))] <- "NYHU"
      names(c)[matches("LASBOR", vars = names(c))] <- "LABO"
      names(c)[matches("MYOSEP", vars = names(c))] <- "MYSE"
      names(c)[matches("MYOLUC", vars = names(c))] <- "MYLU"
      names(c)[matches("PERSUB", vars = names(c))] <- "PESU"
      names(c)[matches("MYOCIL", vars = names(c))] <- "MYCI"
      names(c)[matches("MYOVOL", vars = names(c))] <- "MYVO"
      names(c)[matches("MYOTHY", vars = names(c))] <- "MYTH"
      
      
      c <- c %>% 
        gather("SPECIES", "COUNT", matches("^[A-Z]{4}$")) %>%
        replace_na(list(COUNT = 0)) %>% 
        mutate(COUNT = as.numeric(COUNT))
      
      ##change last (in order) unnamed column to "NIGHT"
      names(c)[max( matches("X[0-9]*", vars = names(c)) )] <- "NIGHT"
      
      ###CHANGE DEPENDING ON PROJECT###
      ##change first unnamed column to "SITE"
      names(c)[min( matches("X[0-9]*", vars = names(c)) )] <- "SITE" 
      
      ##change remaining unnamed column to "deployment"
      names(c)[matches("X[0-9]*", vars = names(c))] <- "DEPLOYMENT"
      
      #################
      #Merge the data#
      ###############
      
      tc <- base::merge(c, t)
      
      tc
      
    })
    
    ##make one df from list 
    d.MLE <- plyr::ldply(l.MLE.csv)
    
    d.MLE$NIGHT <- as.Date(d.MLE$NIGHT, "%Y%m%d") #convert date column to date
    
    
    # fn.save_MLE.R = paste(folder.IDfiles,
    #                       "//ID_MLE_",
    #                       format(Sys.time(), "%Y%m%d%H%M"),
    #                       ".RDATA",
    #                       sep = "")
    # 
    # fn.save_MLE.csv = paste(folder.IDfiles,
    #                         "//ID_MLE_",
    #                         format(Sys.time(), "%Y%m%d%H%M"),
    #                         ".csv",
    #                         sep = "")
    # 
    # save(d.MLE, file = fn.save_MLE.R)
    # write_csv(d.MLE, path = fn.save_MLE.csv)
    
    return(d.MLE)
  }





###############################################
#Create a folder structure from a list########
#############################################

f.build.folder.structure <- function(sink.dir, dir.list) {
  
  setwd(sink.dir) #set working directory
  
  for (i in dir.list) {
    #create directory of deployment name
    dir.create(i)
  }}


##############################
#Copy files in a list########
############################

f.CopyDirOrFile<-function(sourcedir, sinkdir, sourcepath, sinkpath){ 
  DirSource<-paste(sourcepath,sourcedir,sep="") #create source path
  DirEnd<-paste(sinkpath,sinkdir,sep="") #create end path
  file.copy (from = DirSource, to = DirEnd, recursive = TRUE) #copy files
  return(file.exists(paste(sinkpath, sinkdir,sep=""))) #check for directory existance
}




#########################################################
#Obtain a data frame of nearest weather stations########
#######################################################

# dang <- list(
#   point_name = weather.test[2,1]
#   lat = weather.test[2,2]
#   long = weather.test[2,3]
#   api_key = "b656d42f641cc832"
# 
# 
# f.get.weath.stats <- function (point_name, lat, long, api_key) {
#   
#   require(rwunderground)
#   require(tidyverse)
# 
# df <- apply(point_name, lat, long, api_key, FUN = function(point_name, lat, long, api_key) {
# 
#   lat_long.i = as.character(paste(lat, long, sep = ",")) #set lat long
#   key.i = as.character(api_key) #set api key
#   stations <- geolookup(set_location(lat_long = lat_long.i), key = key.i) #look up nearest stations and store in df
#   
#   fuzz <- stations %>% 
#     mutate (site = as.character(point_name[,1]))
#   
#   return(fuzz) 
# })
#   
#   Sys.sleep(10)
#   return(df.all)
#   
# })
#   
#   
#   
# 
# 
# dog <- data.frame(number = seq(1,4,1))
#   
#   
#   
# df <- dog %>% 
#   mutate (site = point_name)
  

##########################################################################
###Calculate VIF and perform a stepwise timming of correlated variables##
########################################################################
  
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  ifelse(class(in_frame) != 'data.frame',in_frame<-data.frame(in_frame), in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-vector('list', length = ncol(in_frame))
  names(vif_init) <- names(in_frame)
  var_names <- names(in_frame)
  
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in<-formula(paste(val,' ~ .'))
    vif_init[[val]]<-VIF(lm(form_in,data=in_frame,...))
  }
  vif_max<-max(unlist(vif_init))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(names(in_frame))
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-vector('list', length = ncol(in_dat))
      names(vif_vals) <- names(in_dat)
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in<-formula(paste(val,' ~ .'))
        vif_add<-VIF(lm(form_in,data=in_dat,...))
        vif_vals[[val]]<-vif_add
      }
      
      max_row<-which.max(vif_vals)[1]
      
      vif_max<-vif_vals[max_row]
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        vif_vals <- do.call('rbind', vif_vals)
        vif_vals
        prmatrix(vif_vals,collab='vif',rowlab=row.names(vif_vals),quote=F)
        cat('\n')
        cat('removed: ', names(vif_max),unlist(vif_max),'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% names(vif_max)]
      
    }
    
    return(names(in_dat))
    
  }
  
}
  

########################################################################################
###Function to summarize number of noise files from a folder tree of summary results###
######################################################################################
#Updated to Kpro 4.1

f.sumNoise <- 
  function(folder.IDfiles,
           pattern.summaryfiles = 'idsummary\\.csv',
           multifolder = T)
  {
    require(tidyverse)
    require(zoo)
    
    #list all files within folder structure
    files.IDSUMMARY <-
      list.files( #list files
        folder.IDfiles, #folder path designated above
        pattern = pattern.summaryfiles, #pattern, default is idsummary
        full.names = T, 
        recursive = multifolder #look down into folder structure
      )
    
    
    
    #remove potential .bak1 files
    files.IDSUMMARY <- files.IDSUMMARY[lapply(files.IDSUMMARY, function(x) length(grep("bak1", x, value = FALSE))) == 0]
    
    ## Read all summary files into a list
    l.MLE.csv <- lapply(files.IDSUMMARY, function(fn){
      
      
      ############
      ##MLE data#
      ##########
      t <- suppressMessages(suppressWarnings(read_csv(fn, skip = 1))) %>% #read in file
        select(matches("X[0-9]*"), NOISE) %>% #select columns with either SPECIES name or X as header (files or MLE)
        select(which(colMeans(is.na(.))<1)) #remove blank columns
      
      
      
      ###Fill in sites and deployments
      columnstofill <- grep("X[0-9]*", names(t)) #find columns with X in header
      t[, columnstofill]<- sapply(t[, columnstofill],FUN = function(x) na.locf(x)) #fill NA with most recent non-NA
      t <- t[!apply(t, 1, function(r) any(r %in% c("*"))),] #remove rows with * in either site or deployment
      
      
      ##change last (in order) unnamed column to "NIGHT"
      names(t)[max( matches("X[0-9]*", vars = names(t)) )] <- "NIGHT"
      
      ###CHANGE DEPENDING ON PROJECT###
      ##change first unnamed column to "SITE"
      names(t)[min( matches("X[0-9]*", vars = names(t)) )] <- "SITE"
      
      ##change remaining unnamed column to "deployment"
      names(t)[matches("X[0-9]*", vars = names(t))] <- "DEPLOYMENT"
      
      t
      
    })
    
    ##make one df from list 
    d.MLE <- plyr::ldply(l.MLE.csv)
    
    d.MLE$NIGHT <- as.Date(d.MLE$NIGHT, "%Y%m%d") #convert date column to date
    
    
    return(d.MLE)
  }


############################################################################
###Function to graph differences in random vs roost tree characteristics###
##########################################################################

character.graph <- function(df, min.count = 4, path = "FIGURES/Looped Figures/", na.rm = TRUE, ...){
  
  #create a list of unique variables to loop over
  
  vars <- df %>% 
    count(variable) %>% 
    filter(n >= min.count)
  
  var_list <- unique(vars$variable)
  
  for (i in seq_along(var_list)) {
    
    df.plot <- subset(df, df$variable == var_list[i])
    df.plot <- df.plot %>% arrange((rst_val - ran_val)) %>% mutate(y.val = 1:n())
    xlim.min.rst <- min((df.plot$rst_val - df.plot$ran_val) - (1.96 * (df.plot$rst_se)))
    xlim.max.rst <- max((df.plot$rst_val - df.plot$ran_val) + (1.96 * (df.plot$rst_se)))
    xlim.min.ran <- min(0 - (1.96 * df.plot$ran_se))
    xlim.max.ran <- max(0 + (1.96 * df.plot$ran_se))
    xlim.min <- min(c(xlim.min.rst, xlim.min.ran))
    xlim.max <- max(c(xlim.max.rst, xlim.max.ran))
    
    save.list <- substr(var_list,1,regexpr("-",var_list)-1)
    
    plot <- 
      
      ggplot(data = df.plot) +
      theme_classic() +
      
      #middle line segment
      geom_segment(aes(x = 0, xend = 0, y=0, yend = max(y.val)+1)) +
      
      #95% CI of random value centered over 0
      geom_segment(aes(x = 0 - (1.96 * ran_se), xend = 0 + (1.96 * ran_se), y = y.val, yend = y.val),
                   size = 4, color = "grey70") +
      
      #95 CI of ROOST value
      geom_segment(aes(x = (rst_val - ran_val) - (1.96 * (rst_se)),
                       xend = (rst_val - ran_val) + (1.96 * (rst_se)),
                       y = y.val, yend = y.val),
                   size = 2, color = "red") +
      
      #Add mean ROOST value as a point
      geom_point(aes(x = rst_val - ran_val, y = y.val), fill = "black", shape = 23, size = 2) +
      
      #add study labels
      geom_text(aes(x = xlim.min + (xlim.min/10), y = y.val, 
                    label = paste0(Label, "-",population, ifelse(is.na(sub_pop), "", paste0("-",sub_pop)))), 
                hjust = 1, size = 2) +
      
      #settings
      coord_cartesian(xlim = c(xlim.min - (abs(xlim.min)*2), xlim.max)) +
      
      scale_y_discrete() +
      labs(x = df.plot$variable[1], y = NULL)
    
    ggsave(plot, file = paste0(path,save.list[i],".png"), height = (11/30)*max(df.plot$y.val), width = 8.5, units = "in")
    
    #print(plot)
    
  }
}

###############################################################################
###Function to obtain GSOD stations from study points within a given radius###
#############################################################################
###INPUTS###
#r = radius (km) from location to search for stations
#ID = id col in input data, unique for each study
#cLat = latitude col from input data
#cLon = longtidude col from input data
#cEnd/start = start col from input data
#full.data = if true then output is only ID, usaf, wban, year




f.getStations.GSOD <- function(data,
                               r = 100,
                               ID = "ID",
                               cLat = "POINT_Y",
                               cLon = "POINT_X",
                               cStart = "Start_date",
                               cEnd = "End_date",
                               full.data = TRUE) 
{
  require(tidyverse)
  require(rnoaa)
  require(lubridate)
  points <- data %>% 
    rename_("X" = cLon, #rename columns for consitancy in the functions below
            "Y" = cLat,
            "start_date" = cStart,
            "end_date" = cEnd,
            "ID" = ID) %>% 
    mutate(X = as.numeric(X), #ensure col is numeric
           Y = as.numeric(Y), #ensure col is numeric
           START = with_tz(as.POSIXct(start_date, format="%Y%m%d"), "GMT"), #add tz if needed
           END =  with_tz(as.POSIXct(end_date, format="%Y%m%d"), "GMT"))
  
  
  #Refresh cache of isd stations
  isd_stations(refresh=T)
  
  #function to retrieve stations from multiple points
  stations <- apply(points, 1, function(pts){ 
    isd_stations_search(lon=as.numeric(pts["X"]), lat=as.numeric(pts["Y"]), radius = r)
  })
  
  names(stations) <- points$ID #extract names from df
  detectorstations <- suppressWarnings(plyr::ldply(stations, .id="ID") %>%  #convert list to df
                                         left_join(points[,c("ID", "start_date", "end_date")], by = "ID") %>% 
                                         mutate(begin = ymd(begin), end = ymd(end)) %>% 
                                         filter(end >= end_date,
                                                begin <= start_date) %>% 
                                         mutate(year = as.numeric(year(end_date))))
  
  
  if(full.data==FALSE ){ 
    df.returndata <- detectorstations %>% select(ID, usaf, wban, year)
  }
  
  df.returndata = detectorstations
  
  return(df.returndata)
  
}

###############################################################################
###Function to obtain GSOD stations from study points within a given radius###
#############################################################################
###INPUTS###
#ID = id col in input data, unique for each study
#years in which data is requested
#usaf = usaf ID of station
#wban = wban ID of station
#end/start = start col from input data

###OUTPUT###
#ID = id of study
#mean.study.temp = mean (c) temp for period of the study


f.study.temp.GSOD <- function(data,
                              ID = "ID",
                              years = "year",
                              usaf = "usaf",
                              wban = "wban",
                              start = "start_date",
                              end = "end_date")
{
  
  require(GSODR)
  
  points <- data %>% 
    rename_("years" = years,
            "usaf" = usaf,
            "wban" = wban,
            "start_date" = start,
            "end_date" = end,
            "ID" = ID) %>% 
    mutate(station = paste0(usaf,"-",wban)) %>% 
    mutate(request_ID = paste0(ID,"-",station))
  
  stations.vector <- unique(points$station)
  
  daily.data <- apply(points, 1, function(pts){
    get_GSOD(years = as.numeric(pts["years"]), station = pts["station"])
  })
  
  names(daily.data) <- points$request_ID #extract names from df and apply to list
  
  
  daily.data.df <- suppressWarnings(plyr::ldply(daily.data, .id = "request_ID") %>% 
                                      filter(STNID %in% stations.vector) %>% 
                                      select(request_ID, STNID, YEARMODA, YEAR, MONTH, DAY, YDAY, TEMP) %>% 
                                      mutate(YEARMODA = ymd(YEARMODA)) %>% 
                                      rename("station" = STNID,
                                             "years" = YEAR,
                                             "month" = MONTH,
                                             "station" = STNID,
                                             "temp" = TEMP) %>% 
                                      mutate(years = as.numeric(years)) %>% 
                                      left_join(points, by = c("request_ID", "station", "years")) %>% 
                                      filter(!is.na(ID)) %>% 
                                      filter(YEARMODA >= start_date,
                                             YEARMODA <= end_date) %>% 
                                      group_by(request_ID, ID, station, years) %>% 
                                      summarise(mean.stat.temp = mean(temp)) %>% 
                                      group_by(ID) %>% 
                                      summarise(mean.study.temp = mean(mean.stat.temp)))
  
  return(daily.data.df)
  
}


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
