library(MonetDB.R)
library(DBI)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate)

conn <- dbConnect(MonetDB.R(), host='localhost', dbname='database', username='monetdb', password='password')

df <- dbGetQuery(conn, "select \"START\",\"NAME\",\"CATEGORY\",\"OBSERVER\" from raw.annotation a inner join raw.patient p on p.\"ID\"=a.\"PATIENT$ID\" where \"TYPE\"='DZNE';")
dbDisconnect(conn)

df %<>%
  dplyr::mutate(START=as.POSIXct(START), 
                NAME=factor(NAME), 
                OBSERVER=factor(OBSERVER), 
                CATEGORY=factor(ifelse(is.na(CATEGORY), 'None',CATEGORY))) %>%
  dplyr::mutate(day=date(START))

show.per.observer <- function(data, show.lines=FALSE){
  data %>%
    group_by(START, OBSERVER) %>%
    arrange(CATEGORY) %>%
    mutate(i=row_number(), n=n()) ->
    df.rank
  
  size = ifelse(show.lines, .1,0)

  ggplot(df.rank, aes(START, as.numeric(factor(OBSERVER)))) + 
    geom_rect(color='black', size=size, aes(fill=CATEGORY, xmin=START, xmax=START + minutes(5), ymin=((i-1)/n) + as.numeric(factor(OBSERVER)), ymax=(i/n) + as.numeric(factor(OBSERVER)))) + 
    scale_y_continuous('OBSERVER', breaks=as.numeric(unique(factor(df.rank$OBSERVER)))+.5, labels=unique(factor(df.rank$OBSERVER))) +
    #scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:00") + 
    scale_fill_brewer(palette='Paired') + 
    theme(legend.position = 'top')
}

show.per.patient <- function(data, show.lines=FALSE){
  data %>%
    group_by(START, NAME) %>%
    arrange(CATEGORY) %>%
    mutate(i=row_number(), n=n()) ->
    df.rank
  
  size = ifelse(show.lines, .1,0)
  
  ggplot(df.rank, aes(START, as.numeric(factor(NAME)))) + 
    geom_rect(color='black', size=size,aes(fill=CATEGORY, xmin=START, xmax=START + minutes(5), ymin=((i-1)/n) + as.numeric(factor(NAME)), ymax=(i/n) + as.numeric(factor(NAME)))) + 
    scale_y_continuous('Patient', breaks=as.numeric(unique(factor(df.rank$NAME)))+.5, labels=unique(factor(df.rank$NAME))) +
    #scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:00") + 
    scale_fill_brewer(palette='Paired') + 
    theme(legend.position = 'top')
}


show.all <- function(data, show.lines=FALSE){
  df %>%
    group_by(START) %>%
    arrange(CATEGORY) %>%
    mutate(i=row_number(), n=n()) ->
    df.rank
  
  size = ifelse(show.lines, .1,0)
  
  ggplot(df.rank, aes(START, 1)) + 
    geom_rect(color='black', size=size,aes(fill=CATEGORY, xmin=START, xmax=START + minutes(5), ymin=((i-1)/n), ymax=(i/n))) + 
    scale_y_continuous('Patient', breaks=.5, labels='patients') +
    #scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:00") + 
    scale_fill_brewer(palette='Paired') + 
    theme(legend.position = 'top')
}


show.per.observer(df)
show.per.patient(df)

show.per.observer(dplyr::filter(df, date(START)<=mean(date(START))))
show.per.observer(dplyr::filter(df, date(START)<=min(date(START))), show.lines = TRUE)

show.per.patient(dplyr::filter(df, date(START)==min(date(START))), show.lines = TRUE)


