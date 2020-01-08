library(ggplot2)
library(MonetDB.R)
library(DBI)
library(dplyr)
library(lubridate)
library(mgcv)


  conn <- DBI::dbConnect(MonetDB.R(), host="localhost", dbname='database', user='monetdb', password='password')
  # get patients
  patients <- dbGetQuery(conn, ' select \"NAME\", \"ID\" from raw.patient')
  
  #select patient X009
  patient.id <- patients$ID[which(patients$NAME=='X009')]
  
  # get all ams entries
  ams.times <- dbGetQuery(conn, paste0("select AMSconfig.\"ID\" as ID, \"START_LIMB\" as limb, \"START_TIME\" as starttime, \"END_TIME\" as endtime from AMSconfig inner join raw.protocol rp on rp.\"ID\"=AMSconfig.\"PROTOCOL$ID\" where \"PATIENT$ID\"=",patient.id)) %>%
    dplyr::mutate(starttime = as.POSIXct(starttime), endtime=as.POSIXct(endtime))

  #plot regions
  ggplot(ams.times) + 
    geom_rect(aes(xmin=starttime, xmax=endtime), ymin=0,ymax=1, color='black', fill='lightgrey') + 
    facet_wrap(~limb, nrow=2) + 
    scale_x_datetime(date_breaks = "1 day", date_labels = '%m/%d') + 
    theme(axis.text.x = element_text(angle = 30))
  
  # somehow select some of them, we take the first 5 of hand
  ams.times %>%
    dplyr::filter(limb=='hand') %>%
    dplyr::arrange(starttime ) %>%
    dplyr::filter(row_number() <= 5) ->
    selected.ids

  query <- paste0("select \"AMSconfig$ID\" as id, AMS.\"START\" as time, AMS.\"VALUE\" as value, \"NAME\" as patient, \"START_LIMB\" as limb,
                  \"START_SIDE\" as side from AMSconfig ac inner join raw.protocol rp on rp.\"ID\"=ac.\"PROTOCOL$ID\" 
                  inner join raw.patient rpa on rpa.\"ID\"=rp.\"PATIENT$ID\" inner join AMS_tmp AMS on
                  ac.\"ID\"=AMS.\"AMSconfig$ID\" where AMS.\"AMSconfig$ID\" in (", paste0(selected.ids$id, collapse = ','), ")")
  
  ams <- DBI::dbGetQuery(conn, query) %>%
    dplyr::mutate(time=as.POSIXct(time))
 #plot ams sequence
  ggplot(ams, aes(time, value)) + 
    geom_line(aes(group=id))

  ggplot(ams, aes(time, log(value))) + 
    geom_line(aes(group=id))
  
  # join ams for all days
  
 
  ams.all <- ams
  day(ams.all$time) = min(day(ams.all$time))
  month(ams.all$time) <- min(month(ams.all$time))
  
  ams.all %>%
    dplyr::group_by(sec10=as.POSIXct( round(as.double(ams.all$time)/10)*10, origin=as.POSIXct('1970-01-01 01:00:00'))) %>%
    summarise(m=mean(value), sd=sd(value), n=n()) ->
    ams.summary
  
  ggplot(ams.summary, aes(sec10, m)) + 
    geom_ribbon(aes(ymin=m-sd, ymax=m+sd), alpha=.2) +    
    geom_line(aes(color=n))  +
    geom_smooth(method='loess', se = F)

  
  dbDisconnect(conn)
  

load.ams <- function(dbname='database', patients=NULL, start.hour = 0, end.hour=24, sample.frac=.1){
  #first get necessary protocols for this selection
  query <- "select \"NAME\", a.\"ID\" from AMSconfig a inner join raw.protocol p on p.\"ID\"=a.\"PROTOCOL$ID\" inner join raw.patient pa on pa.\"ID\"=p.\"PATIENT$ID\""

  if (!is.null(patients)){
    query <- paste0(query, " where \"NAME\" in (\'",paste0(patients, collapse = "\',\'"),"\')")
  }
  
  conn <- DBI::dbConnect(MonetDB.R(), host="localhost", dbname=dbname, user='monetdb', password='password')
  a <- dbGetQuery(conn, query)
  loc <- dbGetQuery(conn, "Select AMSconfig.\"ID\" as \"AMSconfig$ID\" ,\"START\", \"NAME\", location  from location inner join raw.protocol p on p.\"PATIENT$ID\"=location.\"PATIENT$ID\" inner join AMSconfig on AMSconfig.\"PROTOCOL$ID\"=p.\"ID\"") %>%
    dplyr::mutate(day=yday(START), week = week(START)) %>%
    dplyr::select(-START)
  
  
  
  # then get ams data based on protocols ans selection
  ams.query <- paste("select * from ams_tmp where \"AMSconfig$ID\" in (", paste0(a$ID,collapse = ','), ")")
  
  if (start.hour < end.hour){
    compare <- c(">=", "<=")
  }else{
    compare <- c("<=",">=")
  }
  
  ams.query <- paste0(ams.query, " and extract(hour from \"START\") ", compare[1], start.hour)
  ams.query <- paste0(ams.query, " and extract(hour from \"START\") ", compare[2], end.hour)
  if (sample.frac<1 & sample.frac > 0){
    ams.query <- paste(ams.query, "sample" ,sample.frac)
  }
  
  ams <- dbGetQuery(conn, ams.query) %>%
    dplyr::mutate(START=as.POSIXct(START)) %>%
    dplyr::inner_join(loc, by=c("AMSconfig$ID"="AMSconfig$ID")) %>%
    dplyr::mutate(weekday=wday(START,label = TRUE), day=yday(START)-day, week=week(START)-week, NAME=factor(NAME))
}

filter.ams <- function(ams, patients=NULL, days=NULL, weeks=NULL, weekdays=NULL, time=NULL, locations=NULL){
  if (!is.null(patients)){
    ams %<>%
      dplyr::filter(NAME %in% patients)  
  }
  if (!is.null(days)){
    ams %<>%
      dplyr::filter(day %in% days)
  }
  
  if (!is.null(weekdays)){
    ams %<>%
      dplyr::filter(weekday %in% weekdays)
  }
  
  if (!is.null(time)){
    ams %<>%
      dplyr::filter(hour(START) %in% time)
  }
  
  if(!is.null(weeks)){
    ams %<>% 
      dplyr::filter(week %in% weeks)
  }
  
  if (!is.null(location)){
    ams %<>% 
      dplyr::filter(location %in% locations)
  }
  ams
}

  
plot.aggregate.ams <- function(ams, aggregate.by='NAME', relative.scale=FALSE, sample=.1){
  # ten second smoothing
  
  stopifnot(all(aggregate.by %in% names(ams)))
  ams$time <- as.POSIXct( (round(as.double(ams$START)/1)*1) %% (3600*24), origin=as.POSIXct('1970-01-01 01:00:00'))

  l <- split(ams,ams[,aggregate.by])
  l <- l[sapply(l, nrow) > 0]
  
  sm <- do.call("rbind", mclapply(l, function(dd){
    gmeans <- gam(dd$VALUE ~ s(as.numeric(dd$time),bs="cs"))
    bla <- supsmu(dd$time,gmeans$fitted.values)
    baz <- supsmu(dd$time,sqrt(gmeans$residuals^2))
    rownames(dd) <- NULL
    df <- data.frame(smo=bla, sd=baz, dd[1,aggregate.by])
    if (length(aggregate.by)==1){
      names(df)[ncol(df)] <- aggregate.by
    }
    df
  })) 
  
  sm %<>%
    dplyr::sample_frac(.1)

  
  ams %>%
    dplyr::sample_frac(0.1) %>%
    ggplot(.) + 
    #geom_point(aes(x=START, y=VALUE), alpha=.7, color='grey75') +
    geom_line(aes(x=time, y=VALUE), alpha=.7, color='grey75') +
    geom_line(data=sm, aes(x=smo.x, y=smo.y),color='orange') +
    scale_x_datetime(date_labels= "%H:%M") +
    geom_line(data=sm, aes(x=smo.x, y=pmax(0,smo.y-sd.y)),color='orange') +
    geom_line(data=sm, aes(x=smo.x, y=smo.y+sd.y),color='orange') +
    geom_ribbon(data=sm, aes(x=smo.x, ymin=pmax(0,smo.y-sd.y),ymax=smo.y+sd.y), fill='orange', alpha=.5) ->
    p
  
  if (relative.scale){
    p<- p + facet_wrap(as.formula(paste0('~', paste0(aggregate.by, collapse='+'))), scales='free_y')
  } else{
    p<- p + facet_wrap(as.formula(paste0('~', paste0(aggregate.by, collapse='+'))))
  }
  p
}


# plotting example


ams <- load.ams(dbname = 'database', start.hour = 11, end.hour = 14, sample.frac = 1)

ams %<>% filter.ams(ams = ., weeks=1:2, time=11:13)

plot.aggregate.ams(ams, aggregate.by = 'NAME', relative.scale = T, sample=.1)
plot.aggregate.ams(ams, aggregate.by = 'week')
plot.aggregate.ams(ams, aggregate.by = 'location', sample = .1)

plot.aggregate.ams(ams, aggregate.by = c('location','week'), sample = .1)
