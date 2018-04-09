# Fxn to return data frame with monitoring time and possible errors
# an error of -1 indicates multiple distinct ZCA files for a given day. All other field will be NA
# If there are multiple starts and stops within a log file, seperate lines will be created
# If a log file immediately stops writing and never lists and end time, 
# the error col will = 1 and the btryDead time will be the time of this last log file

f.getON_log_temp <- function(file) {
  
  print(file)
  require(readr)
  require(dplyr)
  require(lubridate)
  
  log <- suppressMessages(suppressWarnings(read_csv(file, 
                  col_names=c("TIME", "TYPE", "Msg1", "Msg2"),
                  col_types = cols(.default = "c"))))
  
  thisFile<- log %>% filter(TYPE=="FILE") %>% distinct(Msg1) %>% as.character()
  if(thisFile == "character(0)")thisFile = NULL
  if(length(thisFile)>1){
    data=data_frame(file,
                    "ZCA"="ERROR: More Than 1", 
                    thisDate=NA, 
                    start_Time=NA, 
                    end_Time=NA, 
                    errors=-1, 
                    btryDead=NA, 
                    warnings=NA, 
                    buffer=NA,
                    first_btry=NA,
                    last_btry=NA,
                    "mean_temp" = NA,
                    "min_temp" = NA,
                    "mid_n_temp" = NA,
                    "last_temp" = NA)
    return(data)
  }
  
  if(length(thisFile)==0){
    return(NULL)
  }else{

    thisDate <- stringr::str_extract( thisFile, "201[0-9]-[0-9]{2}-[0-9]{2}") 
    
    starts <- log %>% 
      filter(TYPE == "LOG", Msg1=="Starting sampling") 
    
    ends <- log %>%
      filter(TYPE == "LOG", Msg1=="Closing data file")
    
    if(nrow(ends)==0){
      end.time <- log %>%
        filter(TYPE =="BATT") %>% 
        .[[nrow(.),"TIME"]]
      
      ends <- data.frame(TIME = end.time,
                         TYPE = NA,
                         Msg1 = NA,
                         Msg2 = NA)
      
      log.add <- data.frame(TIME = as.character(end.time),
                            TYPE = as.character(c("ERROR","WARN")),
                            Msg1 = "Battery dead",
                            Msg2 = NA)
      
      log <- suppressWarnings(bind_rows(log, log.add))
    }
    
    all_indexs=which(starts[,"Msg2"]=="night")
    
    data=data.frame()
    
    for(index in all_indexs){
      
      
      start_Time=as.character(starts[[index, "TIME"]])
      end_Time=as.character(ends[[index, "TIME"]])
      
      mn.temp <- log %>% 
        filter(TYPE == "TEMP") %>% 
        filter((hms(TIME) >= hms(start_Time) & hms(TIME) <= hms("23:59:59")) | (hms(TIME) <= hms(end_Time)))
      
      last.temp <- round(last(as.numeric(mn.temp$Msg1)),0)
      mean.temp <- round(mean(as.numeric(mn.temp$Msg1), na.rm = T),0)
      min.temp <- round(min(as.numeric(mn.temp$Msg1), na.rm = T),0)
      mid.night.temp <- mn.temp %>% 
            filter((hms(TIME) >= hms("23:50:00") & hms(TIME) <= hms("23:59:59")) | (hms(TIME) <= hms("00:10:00"))) %>% 
            summarise(mid.night.temp = round(mean(as.numeric(Msg1), na.rm = T),0))
      
      errors <- log %>%
        filter(TYPE =="ERROR") %>% 
        nrow()
      
      warnings <- log %>%
        filter(TYPE =="WARN") %>% 
        nrow()
      
      buffer <- log %>%
        filter(TYPE =="WARN", Msg1=="Buffer overflow") %>% 
        nrow()
      ##get time of battery dead
      btryDead <- log %>%
        filter(TYPE =="ERROR", Msg1=="Battery dead") %>% 
        .[["TIME"]] %>% 
        as.character()
      ##if no battery dead, set to NA
      if( length(btryDead)==0) btryDead=NA
      
      #Get first Battery time
      first_btry <- log %>%
        filter(TYPE =="BATT") %>% 
        .[[1,"TIME"]]
      
      #Get last Battery time
      last_btry <- log %>%
        filter(TYPE =="BATT") %>% 
        .[[nrow(.),"TIME"]]
      
      new_data=data_frame(file,
                          "ZCA"=thisFile, 
                          thisDate, 
                          start_Time, 
                          end_Time, 
                          errors, 
                          btryDead, 
                          warnings, 
                          buffer,
                          first_btry,
                          last_btry,
                          "mean_temp" = mean.temp,
                          "min_temp" = min.temp,
                          "mid_n_temp" = as.numeric(mid.night.temp[1,1]),
                          "last_temp" = last.temp)
      data = bind_rows(data,new_data)
    }
    
    return(data)
  }
}


