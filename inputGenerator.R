###############################################################################################################
#This is a program for the creating input for the reliability project
##############################################################################################################
location <- "S:\\Projects\\Outlier O-D Estimation\\Reliability\\Scripts\\"
# Reading the avl data 
library("RODBC", lib.loc="~/R/win-library/3.3")
conn <- odbcConnect("MT_ODS_ALine") #TO connect the Database
avl <- sqlQuery(conn, "SELECT * FROM [dbo].[t_apc_poc] apc
inner join [dbo].[site] st on
                  apc.site_id = st.site_id 
                  where apc.calendar_id = '120170206'")

# This is the query for the GTFS data. Please note that this is Green Line GTFS data for eastbound direction
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
# Connecting to the server 
con <- dbConnect(drv, dbname = "xxx",
                 host = "xxxx", port = 0000,
                 user = "xxxx", password = "xxx")
#Querying the serviceID within your time period
serviceId <- dbGetQuery(con, "select service_id from gtfs_feb_2017.calendar where start_date < '20170206' and end_date>'20170206'")
#Filtering for typical weekday service
serviceId <- serviceId[grepl('Weekday-01', serviceId$service_id) == TRUE & grepl('01-', serviceId$service_id) == FALSE, ]

# Querying and combining the gtfs data for different serviceIds (e.g. bus, light rail, sub, etc.)
gtfs <- data.frame()
for (sId in serviceId){
  gtfs <- rbind(gtfs, dbGetQuery(con, paste0("select start_date, end_date, routes.route_id, route_type, route_long_name, trips.trip_id, trips.direction_id, trip_headsign, arrival_time, departure_time, stops.stop_id, stop_name, stop_sequence, stop_desc, stop_lat, stop_lon from gtfs.gtfs_feb_2017.calendar inner join gtfs.gtfs_feb_2017.trips ON (calendar.service_id = trips.service_id and (calendar.service_id = ", "'", sId, "'", ")) join gtfs.gtfs_feb_2017.stop_times ON (stop_times.trip_id = trips.trip_id) join gtfs.gtfs_feb_2017.routes ON (routes.route_id = trips.route_id) join gtfs.gtfs_feb_2017.stops ON (stops.stop_id = stop_times.stop_id)")))
}
gtfs$route_short_id <- substr(gtfs$route_id, 1, nchar(gtfs$route_id)-3)

# This function converts seconds into hh:mm:ss (Created by Pramesh)
ConvertToTime <- function(X)
{
  sapply(X, function(Y) paste0(sprintf("%02d", as.integer(Y/3600)), ":", as.integer((Y %% 3600 )/60), ":", (Y %% 3600 ) %% 60))
}

# A function to convert the time into seconds
ConvertToSeconds <- function(X)
{
  X <- strsplit(X, ":")
  sapply(X, function(Y) sum(as.numeric(Y) * c(3600, 60, 1)))
}

# Adding a column in the GTFS data to convert the time into seconds
gtfs$arrival_time_sec <- ConvertToSeconds(gtfs$arrival_time)
gtfs$departure_time_sec <- ConvertToSeconds(gtfs$departure_time)

# Matching the gtfs data with avl data. We are gonna add a column to the GTFS data with actual arrival and departure time of the bus
gtfs$act_arr_tp <- 9999999
gtfs$act_dep_tp <- 9999999


avl <- avl[is.na(avl$sched_time) == FALSE, ] # Removing the null sched_time values from AVL

for(i in seq(from = 1, to = nrow(avl))){
    ind <- which(gtfs$route_short_id == as.character(avl$line_id[i]) & gtfs$stop_id == avl$site_id[i] & gtfs$departure_time_sec == as.numeric(avl$sched_time[i]))
    if (length(ind) == 1){
      gtfs[ind, ]$act_arr_tp <- avl$act_arr_at_tp[i]
      gtfs[ind, ]$act_dep_tp <- avl$act_dep_at_tp[i]
    }
}


#Adding a column if the current stop_sequence is the first stop or last stop or not
gtfs$firstStop <- 0
gtfs$lastStop <- 0
gtfs[gtfs$stop_sequence == 1, ]$firstStop <- 1
for(i in seq(from = 2, to = nrow(gtfs))){
  if(gtfs$stop_sequence[i] == 1){
    gtfs$lastStop[i-1] <- 1
  }
}




# Removing the non-time availability
gtfs <- gtfs[gtfs$act_arr_tp != 9999999, ]
gtfs <- gtfs[gtfs$act_dep_tp != 9999999, ]
gtfs <- gtfs[is.na(gtfs$act_dep_tp) == FALSE, ]
gtfs <- gtfs[is.na(gtfs$act_arr_tp) == FALSE, ]
gtfs <- unique(gtfs)

library(dplyr)



# Let's start creating the transfers
library(geosphere) # Required for calculating the distance between the stop coordinates
createTransferLinks <- function(i, toStop){
  fromStop <- toStop[i, ]
  if(fromStop$firstStop != 1){ # First stop of from node cannot be a transfer 
    toStop$dist <- 0.000621371*distHaversine(c(as.numeric(fromStop$stop_lon), as.numeric(fromStop$stop_lat)), toStop[c('stop_lon', 'stop_lat')])
    toStop$walkTime <- toStop$dist*60/3 # Considering walking speed of 3 miles an hour
    toStop <- toStop[toStop$dist <= 0.25, ]
    toStop <- toStop[toStop$lastStop != 1, ] # Last stop cannot be a transfer to another route
    toAdd1 <- toStop[toStop$dist > 0 & toStop$departure_time_sec - toStop$walkTime  > fromStop$arrival_time_sec & toStop$departure_time_sec - toStop$walkTime < fromStop$arrival_time_sec + 600, ]
    toAdd2 <- toStop[toStop$dist == 0 & toStop$departure_time_sec > fromStop$arrival_time_sec & toStop$departure_time_sec < fromStop$arrival_time_sec + 600, ]
    toStop <- rbind(toAdd2, toAdd1)

    if (nrow(toStop) != 0){
      toStop$routeFrom <- fromStop$route_short_id
      toStop$stopFromName <- fromStop$stop_name
      toStop$stopFromId <- fromStop$stop_id
      toStop$stopFromLat <- fromStop$stop_lat
      toStop$stopFromLon <- fromStop$stop_lon
      toStop$stopFromTripId <- fromStop$trip_id
      toStop$stopFromDir <- fromStop$direction_id
      toStop$stopFromStopSeq <- fromStop$stop_sequence
      toStop$stopFromArrTime <- fromStop$act_arr_tp
      toStop$stopFromDepTime <- fromStop$act_dep_tp
      toStop$stopFromSchedArrTime <- fromStop$arrival_time_sec
      toStop$stopFromSchedDeptTime <- fromStop$departure_time_sec
      toStop$FromNodeId <- rownames(fromStop)
      toStop$toNodeId <- rownames(toStop)
      # Let's reduce the transfers. not every entry here is a transfer
      toStop <- toStop[toStop$FromNodeId != toStop$toNodeId, ] # Not the same entry can be a transfer
      toStop <- toStop[toStop$route_short_id != toStop$routeFrom, ]
      toStop <- na.omit(toStop)
      print(i)
      return(toStop)
    }
    
  }
  
}

transfers_links <- do.call(rbind,lapply(1:nrow(gtfs),function(x) createTransferLinks(x, gtfs)))
transferData$sched_wait_time <- transferData$departure_time_sec - transferData$stopFromSchedArrTime
transferData$act_wait_time <- transferData$act_dep_tp - transferData$stopFromArrTime
# This step is to create a column which records the next bus arrival in case if the passenger missed the bus
for (i in seq(from = 1, to = nrow(transferData))){
  p <- gtfs[gtfs$route_id == transferData$route_id[i] & gtfs$stop_id == transferData$stop_id[i], ]
  j <- which(p$trip_id == transferData$trip_id[i])
  if (j != nrow(p) & nrow(p) != 0){
    transferData$secondTripId[i] <- p$trip_id[j+1]
    transferData$secondArrTime[i] <- p$act_arr_tp[j+1]
    transferData$secondDepTime[i] <- p$act_dep_tp[j+1]
    transferData$secondSchedArrTime[i] <- p$arrival_time_sec[j+1]
    transferData$secondSchedDeptTime[i] <- p$departure_time_sec[j+1]
    transferData$secondFromNodeId[i] <- rownames(p[j+1, ])
  }
  if (i %% 100 == 0){
    print(i)    
  }
  
}

transferData <- transferData[transferData$secondArrTime != 9999999, ]
transferData <- transferData[transferData$secondDepTime != 9999999, ]
transferData <- transferData[transferData$secondSchedArrTime != 9999999, ]
transferData <- transferData[transferData$secondSchedDeptTime != 9999999, ]



# Generating the successful transfer made
transferData$transfer <- transferData$act_dep_tp > transferData$stopFromArrTime
transferData$act_wait_time <- transferData$act_dep_tp - transferData$stopFromArrTime
transferData$sched_wait_time <- transferData$departure_time_sec - transferData$stopFromSchedArrTime
transferData$act_wait_time_failed_transfer <- NA
transferData[transferData$transfer == FALSE, ]$act_wait_time_failed_transfer <- transferData[transferData$transfer == FALSE, ]$secondDepTime - transferData[transferData$transfer == FALSE, ]$stopFromArrTime

write.csv(transferData, paste0(location, "testData.csv"))

transferData <- read.csv(paste0(location, "testData.csv"))


# Testing codes for Shiny App

trans <- as.data.frame(table(transferData$transfer))
colnames(trans) <- c('group', 'Freq')
levels(trans$group) <- c("Failed Transfers", "Successful Transfers")


ggplot(trans, aes(x = 1, y = Freq, fill = group))+
  geom_col(position = 'stack', 
           show.legend = F) +
  geom_text(aes(label = paste(group, ': ', Freq)), 
            position = position_stack(vjust = .5)) +
  coord_polar(theta = "y") +
  theme_void()



ggplot(transferData[transferData$transfer == TRUE, ]) + 
  geom_histogram(aes(sched_wait_time), fill = "red", alpha = 0.2, binwidth = 15) +
  geom_histogram(aes(act_wait_time), fill = "blue", alpha = 0.2, binwidth = 15) + xlim(c(0, 800)) +  scale_fill_manual(values = c(red = "red", blue = "blue"))



linecolors <- c("#714C02", "#01587A", "#024E37")
fillcolors <- c("#9D6C06", "#077DAA", "#026D4E")


ggplot(transferData[transferData$transfer == TRUE, ], aes(sched_wait_time, act_wait_time, colour = transfer, fill = transfer)) +
  geom_point(position=position_jitter(h=0.1, w=0.1),
             shape = 21, alpha = 0.5, size = 3) +
  scale_color_manual(values=linecolors) +
  scale_fill_manual(values=fillcolors) + xlim(0, 6000)
  theme_bw()


# Box plot
  ggplot(transferData[transferData$transfer == TRUE, ], aes(x = sched_wait_time/60, y = 
      act_wait_time/60, group = sched_wait_time, colour = as.factor(sched_wait_time/60))) +
   geom_boxplot(outlier.shape = NA)+ theme_minimal() + xlab("Scheduled wait time (min)") + ylab("Actual wait time (min)") +
    scale_x_continuous(breaks = round(seq(0, 10, by = 0.5))) + scale_y_continuous(breaks = round(seq(0, 30, by = 1)), limits = c(0, 30))+
    labs(color='Scheduled wait time (min)') + ggtitle("Box plot of wait time of successful transfers") + theme_bw()
  
  
# Box plot
ggplot(transferData[transferData$transfer == FALSE & transferData$act_wait_time_failed_transfer > 0 , ], aes(x = sched_wait_time, y = 
                                                             act_wait_time_failed_transfer, group = sched_wait_time, colour = as.character(sched_wait_time))) +
  geom_boxplot()+ theme_minimal() + xlab("Scheduled wait time (sec)") + ylab("Actual wait time (sec)") +
  labs(color='Scheduled wait time') + ggtitle("Box plot of wait time of failed transfers")

# Heat
h <- aggregate(X~stopFromName+stop_name, transferData, length)
ggplot(h, aes(stopFromName, stop_name)) + 
  geom_tile(aes(fill = X), colour = "white") 


# For map 
output$mymap <- renderLeaflet({
  leaflet() %>%
    addTiles() %>% 
    setView(lng = -93.227729, lat = 44.973990, zoom = 13) %>%
    addMarkers(lat = as.numeric(selectedData()$stop_lat), 
               lng = as.numeric(selectedData()$stop_lon)
               
               
    )
})




