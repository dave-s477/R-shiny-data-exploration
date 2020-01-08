library(MonetDB.R)
library(DBI)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(reshape2)


get.data <- function(db){
  conn <- dbConnect(MonetDB.R(), host='localhost', dbname=db, username='monetdb', password='password')
  
  acc_range <- dbGetQuery(conn, 'select * from acc_range') %>%
    dplyr::mutate(NAME=factor(NAME), LIMB=factor(LIMB), START=as.POSIXct(START), END=as.POSIXct(END)) %>%
    dplyr::mutate(interval=interval(START, END))
  
  annotation_range <- dbGetQuery(conn, 'select * from annotation_range') %>%
    dplyr::mutate(NAME=factor(NAME), TYPE=factor(TYPE), START=as.POSIXct(START), END=as.POSIXct(END)) %>%
    dplyr::mutate(interval=interval(START, END))
  
  dbDisconnect(conn)
  list(acc_range=acc_range, annotation_range=annotation_range)
}
# number of annotations per day

dfo <- get.data('location1')
dfl <- get.data('location2')
annotation_range <- rbind(dfo$annotation_range, dfl$annotation_range)
acc_range <- rbind(dfo$acc_range, dfl$acc_range)

annotation_range$diff <- difftime(annotation_range$END,annotation_range$START, units='mins')
acc_range$diff <- difftime(acc_range$END,acc_range$START, units='mins')


# plot number of intervals available with fixed interval
plot.interval.fixed <- function(acc, annotation, start=14, end=18){
  # get joined intervals
  acc.anno <- join.by.interval(acc, annotation)
  
  aa_starttime <- ymd_h(paste0(lubridate::date(int_start(acc.anno$interval))," ", start))
  acc_starttime <- ymd_h(paste0(lubridate::date(acc$START)," ", start))
  anno_starttime <- ymd_h(paste0(lubridate::date(annotation$START)," ", start))
  anno_endtime <- ymd_h(paste0(lubridate::date(annotation$START)," ", end))
  
  if (end <= start){
    acc_endtime <- ymd_h(paste0(lubridate::date(acc$START)," ", end)) + days()
    aa_endtime <- ymd_h(paste0(lubridate::date(int_start(acc.anno$interval))," ", end)) + days()
  }else{
    acc_endtime <- ymd_h(paste0(lubridate::date(acc$START)," ", end))
    aa_endtime <- ymd_h(paste0(lubridate::date(int_start(acc.anno$interval))," ", end))
  }
  
  acc$in.interval <- ((acc_starttime %within% acc$interval) & (acc_endtime %within% acc$interval))
  annotation$in.interval <- ((anno_starttime %within% annotation$interval) & (anno_endtime %within% annotation$interval))
  acc.anno$in.interval <- ((aa_starttime %within% acc.anno$interval) & (aa_endtime %within% acc.anno$interval))
  
  dplyr::group_by(acc, NAME, LIMB) %>%
    dplyr::summarise(day=sum(in.interval))->
    amount
  
  dplyr::group_by(acc.anno, NAME, LIMB) %>%
    dplyr::summarise(day=sum(in.interval))->
    aa.amount
  
  dplyr::group_by(annotation, NAME) %>%
    dplyr::summarise(day=sum(in.interval))->
    anno.amount
  
  ggplot(amount, aes(NAME, day)) + 
    geom_bar(aes(fill=LIMB),stat='identity', position = 'dodge') + 
    ggtitle(paste0("Amount of days where data is avaliable between ", start, " and ", end)) + 
    scale_fill_brewer(palette='Paired') + 
    geom_text(aes(label=day,group=LIMB), position=position_dodge(width=.8))
  
  ggplot(anno.amount, aes(NAME, day)) + 
    geom_bar(aes(fill='Annotation'), stat='identity', position = 'dodge') + 
    ggtitle(paste0("Amount of days where annotation is avaliable between ", start, " and ", end)) + 
    scale_fill_brewer(palette='Paired') + 
    geom_text(aes(label=day), position=position_dodge(width=.8))
  
  p <- ggplot(aa.amount, aes(NAME, day)) + 
    geom_bar(aes(fill=LIMB),stat='identity', position = 'dodge') + 
    ggtitle(paste0("Amount of days where data is avaliable between ", start, " and ", end)) + 
    scale_fill_brewer(palette='Paired') + 
    geom_text(aes(label=day,group=LIMB), position=position_dodge(width=.8))
  p
}

# this function basically immitates an inner join on the intervals
join.by.interval <- function(acc, annotation){
  # get combined indices (full join)
  ci <- expand.grid(1:nrow(acc), 1:nrow(annotation))
  # filter by name (full join on NAME)
  ci.n <- ci[acc$NAME[ci$Var1]==annotation$NAME[ci$Var2],]
  # get indices of overlaps 
  ci.o <- ci.n[int_overlaps(acc$interval[ci.n$Var1], annotation$interval[ci.n$Var2]),]
  
  df <- data.frame(NAME=acc$NAME[ci.o$Var1], LIMB=acc$LIMB[ci.o$Var1], interval=interval(start=pmax(int_start(acc$interval[ci.o$Var1]), int_start(annotation$interval[ci.o$Var2])),
                                                                                         end=pmin(int_end(acc$interval[ci.o$Var1]),int_end(annotation$interval[ci.o$Var2]))))
}

plot.interval.floating.progress <- function(acc, annotation, ranges=seq(.5,5,by=.5)){
  # get joined intervals
  acc.anno <- join.by.interval(acc, annotation)
  
  df <- dplyr::select(acc.anno, NAME, LIMB, interval)
  
  df$length <- int_length(df$interval)
  
  r <- do.call('cbind', lapply(ranges, function(r){r*3600 <= df$length}))
  colnames(r) <- ranges
  
  dfm <- data.frame(NAME=df$NAME,LIMB=df$LIMB, r) %>%
    dplyr::group_by(NAME, LIMB) %>%
    dplyr::summarise_all(.funs = sum) %>%
    melt(., id.vars = c('NAME', 'LIMB')) %>%
    dplyr::mutate(hours=as.numeric(gsub('^[X]([0-9]+\\.*[0-9]*)','\\1',variable))) 
  
  dfm %>%
    dplyr::group_by(LIMB, hours) %>%
    dplyr::summarise(m=mean(value), sd=sd(value)) ->
    dfm.plot
  
  p <- ggplot(dfm.plot, aes(hours, m)) + 
    geom_ribbon(aes(fill=LIMB, ymax=m+sd, ymin=pmax(0,m-sd)), alpha=.2) + 
    geom_line(aes(color=LIMB)) + 
    scale_fill_brewer(palette = 'Paired') + 
    scale_color_brewer(palette = 'Paired') + 
    theme(legend.position='top') + 
    ylab("Mean amount of intervals") + 
    xlab("Length of interval in hours")
  print(p)
  
  dfm
}

# takes output from plot.interval.floating.progress
plot.interval.floating <- function(dfm, select.hours=1, fix.scale=TRUE){
  dplyr::filter(dfm, hours==select.hours) %>%
    ggplot(., aes(NAME, value)) + 
    geom_bar(aes(fill=LIMB), stat='identity', position='dodge') + 
    scale_fill_brewer(palette = 'Paired') + 
    geom_text(aes(label=value, group=LIMB), position=position_dodge(width=.8)) + 
    ylab('Amount of intervals per subject') + 
    xlab('Subject') + 
    ggtitle(paste0('Amount of intervals with minimum length of ', select.hours, ' hours.')) ->
    p
  
  if (fix.scale){
    p <- p + ylim(0, max(dfm$value))
  }
  p
}

