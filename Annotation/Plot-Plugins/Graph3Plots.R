########################################Plot 1########################################
#Aggregation Plot
aggregationPlot <- function(plot_data, plot_data.min, start, end){
  colorScheme <- colorSchemeGraph3(plot_data)
  p <- ggplot(plot_data, aes(NAME, n)) + 
    geom_bar(aes(fill=START_LIMB), stat='identity', position='dodge') + 
    scale_fill_manual(values = colorScheme) +
    theme(legend.position = 'top') +
    facet_grid(.~week)
  return(p)
}

########################################Plot 1########################################
plot.interval.fixed <- function (acc, annotation, start, end) {
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
    theme(legend.position = 'top') +
    geom_text(aes(label=day,group=LIMB), position=position_dodge(width=.8))
  
  ggplot(anno.amount, aes(NAME, day)) + 
    geom_bar(aes(fill='Annotation'), stat='identity', position = 'dodge') + 
    ggtitle(paste0("Amount of days where annotation is avaliable between ", start, " and ", end)) + 
    scale_fill_brewer(palette='Paired') + 
    theme(legend.position = 'top') +
    geom_text(aes(label=day), position=position_dodge(width=.8))
  
  ggplot(aa.amount, aes(NAME, day)) + 
    geom_bar(aes(fill=LIMB),stat='identity', position = 'dodge') + 
    ggtitle(paste0("Amount of days where data is avaliable between ", start, " and ", end)) + 
    scale_fill_brewer(palette='Paired') + 
    theme(legend.position = 'top') +
    geom_text(aes(label=day,group=LIMB), position=position_dodge(width=.8))
}

singleplot1 <- memoise(plot.interval.fixed)


########################################Plot 2########################################

plot.interval.floating.progress <- function (acc, annotation, ranges=seq(.5,5,by=.1)) {
  dfm <- interval.floating.data(acc, annotation, ranges)
  dfm %>%
    dplyr::group_by(LIMB, hours) %>%
    dplyr::summarise(m=mean(value), sd=sd(value)) ->
    dfm.plot
  
  p <- ggplot(dfm.plot, aes(hours, m)) + 
    geom_ribbon(aes(fill=LIMB, ymax=m+sd, ymin=pmax(0,m-sd)), alpha=.2) + 
    geom_line(aes(color=LIMB)) + 
    ggtitle(paste0("Amount of Intervals compared to their length")) + 
    scale_fill_brewer(palette = 'Paired') + 
    scale_color_brewer(palette = 'Paired') + 
    theme(legend.position='top') + 
    ylab("Mean amount of intervals") + 
    xlab("Length of interval in hours")
  p
}

singleplot2 <- memoise(plot.interval.floating.progress)


########################################Plot 3########################################

plot.interval.floating <- function (acc, annotation, select.hours=1, ranges=seq(.5,5,by=.1), fix.scale=TRUE) {
  dfm <- interval.floating.data(acc, annotation, ranges)
  dplyr::filter(dfm, hours==select.hours) %>%
    ggplot(., aes(NAME, value)) + 
    geom_bar(aes(fill=LIMB), stat='identity', position='dodge') + 
    scale_fill_brewer(palette = 'Paired') + 
    geom_text(aes(label=value, group=LIMB), position=position_dodge(width=.8)) + 
    theme(legend.position = 'top') +
    ylab('Amount of intervals per subject') + 
    xlab('Subject') + 
    ggtitle(paste0('Amount of intervals with minimum length of ', select.hours, ' hours.')) ->
    p
  
  if (fix.scale){
    p <- p + ylim(0, max(dfm$value))
  }
  p
}

singleplot3 <- memoise(plot.interval.floating)

########################################Data Functions########################################
#Data for Plot 2 and 3

interval.floating.dataU <- function(acc, annotation, ranges=seq(0,5,by=.1)){
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
  
  dfm
}

interval.floating.data <- memoise(interval.floating.dataU)

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