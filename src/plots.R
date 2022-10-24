pa.data <- function() {
  # Download purpleair data from dropbox
  url <- 'https://www.dropbox.com/s/hdi43rjded4ojtf/pa_lv_7day.csv?dl=1'
  
  cols <- cols(
    time_stamp = col_double(),
    datetime = col_datetime(format = ""),
    sensor_index = col_double(),
    humidity = col_double(),
    temperature = col_double(),
    pressure = col_double(),
    pm2.5 = col_double(),
    pm2.5_corr = col_double(),
    aqi = col_double(),
    aqi_corr = col_double())
  
  pa7 <- read_csv(url, col_types=cols)
  
  return(pa7)
}

pa.plot1 <- function(df) {
  df <- df %>% filter(sensor_index==20703)
  
  green  <- csscolor(rgb(0, 128, 0, 200, maxColorValue=255))
  yellow <- csscolor(rgb(255, 255, 0, 200, maxColorValue=255))
  orange <- csscolor(rgb(255, 140, 0, 200, maxColorValue=255))
  red    <- csscolor(rgb(255, 0, 0, 200, maxColorValue=255))
  purple <- csscolor(rgb(128, 0, 128, 200, maxColorValue=255))
  brown  <- csscolor(rgb(128, 0, 0, 200, maxColorValue=255))
  
  name <- case_when(
    df$sensor_index == 20703 ~ 'Edgeboro Annex',
    df$sensor_index == 82657 ~ 'BET5049',
    df$sensor_index == 101980 ~ 'NorthBeth22',
    df$sensor_index == 108582 ~ 'MineoSS',
    df$sensor_index == 108584 ~ 'FB1',
    df$sensor_index == 108706 ~ 'FB2',
    df$sensor_index == 108716 ~ 'FB3',
    df$sensor_index == 148727 ~ 'HanTwp-1',
    TRUE ~ 'Han-Twp-2'
  )
  
  date1 <- min(df$datetime)
  date2 <- max(df$datetime)
  date3 <- df %>%
    select(datetime) %>%
    mutate(datetime=as.Date(datetime)) %>%
    slice_tail(n=1) %>%
    pull()
  date3 <- format(date3, "%B %d, %Y")
  
  df %>%
    ggplot() +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=0,ymax=50,
             fill='darkgreen',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=50,ymax=100,
             fill='yellow',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=100,ymax=150,
             fill='orange',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=150,ymax=200,
             fill='red',alpha=0.4) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,200)) +
    geom_point(aes(datetime,aqi),size=0.5,alpha=0.3,color='gray',shape=20) +
    geom_line(size=0.25,aes(datetime,rollapplyr(aqi,30,mean,partial=TRUE))) +
    labs(y='Air Quality Index (AQI)',
         x='Date',
         title=paste0("Plot of 7-day AQI data for PurpleAir sensor at ",name)) +
    theme(plot.title=element_text(size=8),
          axis.title=element_text(size=8),
          axis.text=element_text(size=7),
          axis.ticks=element_line(size=0.25))
}

pa.plot2 <- function(df) {
  df <- df %>% filter(sensor_index==82657)
  
  green  <- csscolor(rgb(0, 128, 0, 200, maxColorValue=255))
  yellow <- csscolor(rgb(255, 255, 0, 200, maxColorValue=255))
  orange <- csscolor(rgb(255, 140, 0, 200, maxColorValue=255))
  red    <- csscolor(rgb(255, 0, 0, 200, maxColorValue=255))
  purple <- csscolor(rgb(128, 0, 128, 200, maxColorValue=255))
  brown  <- csscolor(rgb(128, 0, 0, 200, maxColorValue=255))
  
  name <- case_when(
    df$sensor_index == 20703 ~ 'Edgeboro Annex',
    df$sensor_index == 82657 ~ 'BET5049',
    df$sensor_index == 101980 ~ 'NorthBeth22',
    df$sensor_index == 108582 ~ 'MineoSS',
    df$sensor_index == 108584 ~ 'FB1',
    df$sensor_index == 108706 ~ 'FB2',
    df$sensor_index == 108716 ~ 'FB3',
    df$sensor_index == 148727 ~ 'HanTwp-1',
    TRUE ~ 'Han-Twp-2'
  )
  
  date1 <- min(df$datetime)
  date2 <- max(df$datetime)
  date3 <- df %>%
    select(datetime) %>%
    mutate(datetime=as.Date(datetime)) %>%
    slice_tail(n=1) %>%
    pull()
  date3 <- format(date3, "%B %d, %Y")
  
  df %>%
    ggplot() +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=0,ymax=50,
             fill='darkgreen',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=50,ymax=100,
             fill='yellow',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=100,ymax=150,
             fill='orange',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=150,ymax=200,
             fill='red',alpha=0.4) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,200)) +
    geom_point(aes(datetime,aqi),size=0.5,alpha=0.3,color='gray',shape=20) +
    geom_line(size=0.25,aes(datetime,rollapplyr(aqi,30,mean,partial=TRUE))) +
    labs(y='Air Quality Index (AQI)',
         x='Date',
         title=paste0("Plot of 7-day AQI data for PurpleAir sensor at ",name)) +
    theme(plot.title=element_text(size=8),
          axis.title=element_text(size=8),
          axis.text=element_text(size=7),
          axis.ticks=element_line(size=0.25))
}

pa.plot3 <- function(df) {
  df <- df %>% filter(sensor_index==101980)
  
  green  <- csscolor(rgb(0, 128, 0, 200, maxColorValue=255))
  yellow <- csscolor(rgb(255, 255, 0, 200, maxColorValue=255))
  orange <- csscolor(rgb(255, 140, 0, 200, maxColorValue=255))
  red    <- csscolor(rgb(255, 0, 0, 200, maxColorValue=255))
  purple <- csscolor(rgb(128, 0, 128, 200, maxColorValue=255))
  brown  <- csscolor(rgb(128, 0, 0, 200, maxColorValue=255))
  
  name <- case_when(
    df$sensor_index == 20703 ~ 'Edgeboro Annex',
    df$sensor_index == 82657 ~ 'BET5049',
    df$sensor_index == 101980 ~ 'NorthBeth22',
    df$sensor_index == 108582 ~ 'MineoSS',
    df$sensor_index == 108584 ~ 'FB1',
    df$sensor_index == 108706 ~ 'FB2',
    df$sensor_index == 108716 ~ 'FB3',
    df$sensor_index == 148727 ~ 'HanTwp-1',
    TRUE ~ 'Han-Twp-2'
  )
  
  date1 <- min(df$datetime)
  date2 <- max(df$datetime)
  date3 <- df %>%
    select(datetime) %>%
    mutate(datetime=as.Date(datetime)) %>%
    slice_tail(n=1) %>%
    pull()
  date3 <- format(date3, "%B %d, %Y")
  
  df %>%
    ggplot() +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=0,ymax=50,
             fill='darkgreen',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=50,ymax=100,
             fill='yellow',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=100,ymax=150,
             fill='orange',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=150,ymax=200,
             fill='red',alpha=0.4) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,200)) +
    geom_point(aes(datetime,aqi),size=0.5,alpha=0.3,color='gray',shape=20) +
    geom_line(size=0.25,aes(datetime,rollapplyr(aqi,30,mean,partial=TRUE))) +
    labs(y='Air Quality Index (AQI)',
         x='Date',
         title=paste0("Plot of 7-day AQI data for PurpleAir sensor at ",name)) +
    theme(plot.title=element_text(size=8),
          axis.title=element_text(size=8),
          axis.text=element_text(size=7),
          axis.ticks=element_line(size=0.25))
}

pa.plot4 <- function(df) {
  df <- df %>% filter(sensor_index==108582)
  
  green  <- csscolor(rgb(0, 128, 0, 200, maxColorValue=255))
  yellow <- csscolor(rgb(255, 255, 0, 200, maxColorValue=255))
  orange <- csscolor(rgb(255, 140, 0, 200, maxColorValue=255))
  red    <- csscolor(rgb(255, 0, 0, 200, maxColorValue=255))
  purple <- csscolor(rgb(128, 0, 128, 200, maxColorValue=255))
  brown  <- csscolor(rgb(128, 0, 0, 200, maxColorValue=255))
  
  name <- case_when(
    df$sensor_index == 20703 ~ 'Edgeboro Annex',
    df$sensor_index == 82657 ~ 'BET5049',
    df$sensor_index == 101980 ~ 'NorthBeth22',
    df$sensor_index == 108582 ~ 'MineoSS',
    df$sensor_index == 108584 ~ 'FB1',
    df$sensor_index == 108706 ~ 'FB2',
    df$sensor_index == 108716 ~ 'FB3',
    df$sensor_index == 148727 ~ 'HanTwp-1',
    TRUE ~ 'Han-Twp-2'
  )
  
  date1 <- min(df$datetime)
  date2 <- max(df$datetime)
  date3 <- df %>%
    select(datetime) %>%
    mutate(datetime=as.Date(datetime)) %>%
    slice_tail(n=1) %>%
    pull()
  date3 <- format(date3, "%B %d, %Y")
  
  df %>%
    ggplot() +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=0,ymax=50,
             fill='darkgreen',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=50,ymax=100,
             fill='yellow',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=100,ymax=150,
             fill='orange',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=150,ymax=200,
             fill='red',alpha=0.4) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,200)) +
    geom_point(aes(datetime,aqi),size=0.5,alpha=0.3,color='gray',shape=20) +
    geom_line(size=0.25,aes(datetime,rollapplyr(aqi,30,mean,partial=TRUE))) +
    labs(y='Air Quality Index (AQI)',
         x='Date',
         title=paste0("Plot of 7-day AQI data for PurpleAir sensor at ",name)) +
    theme(plot.title=element_text(size=8),
          axis.title=element_text(size=8),
          axis.text=element_text(size=7),
          axis.ticks=element_line(size=0.25))
}

pa.plot5 <- function(df) {
  df <- df %>% filter(sensor_index==108584)
  
  green  <- csscolor(rgb(0, 128, 0, 200, maxColorValue=255))
  yellow <- csscolor(rgb(255, 255, 0, 200, maxColorValue=255))
  orange <- csscolor(rgb(255, 140, 0, 200, maxColorValue=255))
  red    <- csscolor(rgb(255, 0, 0, 200, maxColorValue=255))
  purple <- csscolor(rgb(128, 0, 128, 200, maxColorValue=255))
  brown  <- csscolor(rgb(128, 0, 0, 200, maxColorValue=255))
  
  name <- case_when(
    df$sensor_index == 20703 ~ 'Edgeboro Annex',
    df$sensor_index == 82657 ~ 'BET5049',
    df$sensor_index == 101980 ~ 'NorthBeth22',
    df$sensor_index == 108582 ~ 'MineoSS',
    df$sensor_index == 108584 ~ 'FB1',
    df$sensor_index == 108706 ~ 'FB2',
    df$sensor_index == 108716 ~ 'FB3',
    df$sensor_index == 148727 ~ 'HanTwp-1',
    TRUE ~ 'Han-Twp-2'
  )
  
  date1 <- min(df$datetime)
  date2 <- max(df$datetime)
  date3 <- df %>%
    select(datetime) %>%
    mutate(datetime=as.Date(datetime)) %>%
    slice_tail(n=1) %>%
    pull()
  date3 <- format(date3, "%B %d, %Y")
  
  df %>%
    ggplot() +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=0,ymax=50,
             fill='darkgreen',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=50,ymax=100,
             fill='yellow',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=100,ymax=150,
             fill='orange',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=150,ymax=200,
             fill='red',alpha=0.4) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,200)) +
    geom_point(aes(datetime,aqi),size=0.5,alpha=0.3,color='gray',shape=20) +
    geom_line(size=0.25,aes(datetime,rollapplyr(aqi,30,mean,partial=TRUE))) +
    labs(y='Air Quality Index (AQI)',
         x='Date',
         title=paste0("Plot of 7-day AQI data for PurpleAir sensor at ",name)) +
    theme(plot.title=element_text(size=8),
          axis.title=element_text(size=8),
          axis.text=element_text(size=7),
          axis.ticks=element_line(size=0.25))
}

pa.plot6 <- function(df) {
  df <- df %>% filter(sensor_index==108706)
  
  green  <- csscolor(rgb(0, 128, 0, 200, maxColorValue=255))
  yellow <- csscolor(rgb(255, 255, 0, 200, maxColorValue=255))
  orange <- csscolor(rgb(255, 140, 0, 200, maxColorValue=255))
  red    <- csscolor(rgb(255, 0, 0, 200, maxColorValue=255))
  purple <- csscolor(rgb(128, 0, 128, 200, maxColorValue=255))
  brown  <- csscolor(rgb(128, 0, 0, 200, maxColorValue=255))
  
  name <- case_when(
    df$sensor_index == 20703 ~ 'Edgeboro Annex',
    df$sensor_index == 82657 ~ 'BET5049',
    df$sensor_index == 101980 ~ 'NorthBeth22',
    df$sensor_index == 108582 ~ 'MineoSS',
    df$sensor_index == 108584 ~ 'FB1',
    df$sensor_index == 108706 ~ 'FB2',
    df$sensor_index == 108716 ~ 'FB3',
    df$sensor_index == 148727 ~ 'HanTwp-1',
    TRUE ~ 'Han-Twp-2'
  )
  
  date1 <- min(df$datetime)
  date2 <- max(df$datetime)
  date3 <- df %>%
    select(datetime) %>%
    mutate(datetime=as.Date(datetime)) %>%
    slice_tail(n=1) %>%
    pull()
  date3 <- format(date3, "%B %d, %Y")
  
  df %>%
    ggplot() +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=0,ymax=50,
             fill='darkgreen',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=50,ymax=100,
             fill='yellow',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=100,ymax=150,
             fill='orange',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=150,ymax=200,
             fill='red',alpha=0.4) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,200)) +
    geom_point(aes(datetime,aqi),size=0.5,alpha=0.3,color='gray',shape=20) +
    geom_line(size=0.25,aes(datetime,rollapplyr(aqi,30,mean,partial=TRUE))) +
    labs(y='Air Quality Index (AQI)',
         x='Date',
         title=paste0("Plot of 7-day AQI data for PurpleAir sensor at ",name)) +
    theme(plot.title=element_text(size=8),
          axis.title=element_text(size=8),
          axis.text=element_text(size=7),
          axis.ticks=element_line(size=0.25))
}

pa.plot7 <- function(df) {
  df <- df %>% filter(sensor_index==108716)
  
  green  <- csscolor(rgb(0, 128, 0, 200, maxColorValue=255))
  yellow <- csscolor(rgb(255, 255, 0, 200, maxColorValue=255))
  orange <- csscolor(rgb(255, 140, 0, 200, maxColorValue=255))
  red    <- csscolor(rgb(255, 0, 0, 200, maxColorValue=255))
  purple <- csscolor(rgb(128, 0, 128, 200, maxColorValue=255))
  brown  <- csscolor(rgb(128, 0, 0, 200, maxColorValue=255))
  
  name <- case_when(
    df$sensor_index == 20703 ~ 'Edgeboro Annex',
    df$sensor_index == 82657 ~ 'BET5049',
    df$sensor_index == 101980 ~ 'NorthBeth22',
    df$sensor_index == 108582 ~ 'MineoSS',
    df$sensor_index == 108584 ~ 'FB1',
    df$sensor_index == 108706 ~ 'FB2',
    df$sensor_index == 108716 ~ 'FB3',
    df$sensor_index == 148727 ~ 'HanTwp-1',
    TRUE ~ 'Han-Twp-2'
  )
  
  date1 <- min(df$datetime)
  date2 <- max(df$datetime)
  date3 <- df %>%
    select(datetime) %>%
    mutate(datetime=as.Date(datetime)) %>%
    slice_tail(n=1) %>%
    pull()
  date3 <- format(date3, "%B %d, %Y")
  
  df %>%
    ggplot() +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=0,ymax=50,
             fill='darkgreen',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=50,ymax=100,
             fill='yellow',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=100,ymax=150,
             fill='orange',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=150,ymax=200,
             fill='red',alpha=0.4) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,200)) +
    geom_point(aes(datetime,aqi),size=0.5,alpha=0.3,color='gray',shape=20) +
    geom_line(size=0.25,aes(datetime,rollapplyr(aqi,30,mean,partial=TRUE))) +
    labs(y='Air Quality Index (AQI)',
         x='Date',
         title=paste0("Plot of 7-day AQI data for PurpleAir sensor at ",name)) +
    theme(plot.title=element_text(size=8),
          axis.title=element_text(size=8),
          axis.text=element_text(size=7),
          axis.ticks=element_line(size=0.25))
}

pa.plot8 <- function(df) {
  df <- df %>% filter(sensor_index==148727)
  
  green  <- csscolor(rgb(0, 128, 0, 200, maxColorValue=255))
  yellow <- csscolor(rgb(255, 255, 0, 200, maxColorValue=255))
  orange <- csscolor(rgb(255, 140, 0, 200, maxColorValue=255))
  red    <- csscolor(rgb(255, 0, 0, 200, maxColorValue=255))
  purple <- csscolor(rgb(128, 0, 128, 200, maxColorValue=255))
  brown  <- csscolor(rgb(128, 0, 0, 200, maxColorValue=255))
  
  name <- case_when(
    df$sensor_index == 20703 ~ 'Edgeboro Annex',
    df$sensor_index == 82657 ~ 'BET5049',
    df$sensor_index == 101980 ~ 'NorthBeth22',
    df$sensor_index == 108582 ~ 'MineoSS',
    df$sensor_index == 108584 ~ 'FB1',
    df$sensor_index == 108706 ~ 'FB2',
    df$sensor_index == 108716 ~ 'FB3',
    df$sensor_index == 148727 ~ 'HanTwp-1',
    TRUE ~ 'Han-Twp-2'
  )
  
  date1 <- min(df$datetime)
  date2 <- max(df$datetime)
  date3 <- df %>%
    select(datetime) %>%
    mutate(datetime=as.Date(datetime)) %>%
    slice_tail(n=1) %>%
    pull()
  date3 <- format(date3, "%B %d, %Y")
  
  df %>%
    ggplot() +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=0,ymax=50,
             fill='darkgreen',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=50,ymax=100,
             fill='yellow',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=100,ymax=150,
             fill='orange',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=150,ymax=200,
             fill='red',alpha=0.4) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,200)) +
    geom_point(aes(datetime,aqi),size=0.5,alpha=0.3,color='gray',shape=20) +
    geom_line(size=0.25,aes(datetime,rollapplyr(aqi,30,mean,partial=TRUE))) +
    labs(y='Air Quality Index (AQI)',
         x='Date',
         title=paste0("Plot of 7-day AQI data for PurpleAir sensor at ",name)) +
    theme(plot.title=element_text(size=8),
          axis.title=element_text(size=8),
          axis.text=element_text(size=7),
          axis.ticks=element_line(size=0.25))
}

pa.plot9 <- function(df) {
  df <- df %>% filter(sensor_index==149118)
  
  green  <- csscolor(rgb(0, 128, 0, 200, maxColorValue=255))
  yellow <- csscolor(rgb(255, 255, 0, 200, maxColorValue=255))
  orange <- csscolor(rgb(255, 140, 0, 200, maxColorValue=255))
  red    <- csscolor(rgb(255, 0, 0, 200, maxColorValue=255))
  purple <- csscolor(rgb(128, 0, 128, 200, maxColorValue=255))
  brown  <- csscolor(rgb(128, 0, 0, 200, maxColorValue=255))
  
  name <- case_when(
    df$sensor_index == 20703 ~ 'Edgeboro Annex',
    df$sensor_index == 82657 ~ 'BET5049',
    df$sensor_index == 101980 ~ 'NorthBeth22',
    df$sensor_index == 108582 ~ 'MineoSS',
    df$sensor_index == 108584 ~ 'FB1',
    df$sensor_index == 108706 ~ 'FB2',
    df$sensor_index == 108716 ~ 'FB3',
    df$sensor_index == 148727 ~ 'HanTwp-1',
    TRUE ~ 'Han-Twp-2'
  )
  
  date1 <- min(df$datetime)
  date2 <- max(df$datetime)
  date3 <- df %>%
    select(datetime) %>%
    mutate(datetime=as.Date(datetime)) %>%
    slice_tail(n=1) %>%
    pull()
  date3 <- format(date3, "%B %d, %Y")
  
  df %>%
    ggplot() +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=0,ymax=50,
             fill='darkgreen',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=50,ymax=100,
             fill='yellow',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=100,ymax=150,
             fill='orange',alpha=0.4) +
    annotate('rect',
             xmin=date1,xmax=date2,
             ymin=150,ymax=200,
             fill='red',alpha=0.4) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,200)) +
    geom_point(aes(datetime,aqi),size=0.5,alpha=0.3,color='gray',shape=20) +
    geom_line(size=0.25,aes(datetime,rollapplyr(aqi,30,mean,partial=TRUE))) +
    labs(y='Air Quality Index (AQI)',
         x='Date',
         title=paste0("Plot of 7-day AQI data for PurpleAir sensor at ",name)) +
    theme(plot.title=element_text(size=8),
          axis.title=element_text(size=8),
          axis.text=element_text(size=7),
          axis.ticks=element_line(size=0.25))
}

plot1_out <- function(file_out){
  tar_load(pa_plot_20703)
  
  print(pa_plot_20703)
  
  ggsave(file_out,
         width=7,
         height=4,
         units='in',
         dpi=600)
}

plot2_out <- function(file_out){
  tar_load(pa_plot_82657)
  
  print(pa_plot_82657)
  
  ggsave(file_out,
         width=7,
         height=4,
         units='in',
         dpi=600)
}

plot3_out <- function(file_out){
  tar_load(pa_plot_101980)
  
  print(pa_plot_101980)
  
  ggsave(file_out,
         width=7,
         height=4,
         units='in',
         dpi=600)
}

plot4_out <- function(file_out){
  tar_load(pa_plot_108582)
  
  print(pa_plot_108582)
  
  ggsave(file_out,
         width=7,
         height=4,
         units='in',
         dpi=600)
}

plot5_out <- function(file_out){
  tar_load(pa_plot_108584)
  
  print(pa_plot_108584)
  
  ggsave(file_out,
         width=7,
         height=4,
         units='in',
         dpi=600)
}

plot6_out <- function(file_out){
  tar_load(pa_plot_108706)
  
  print(pa_plot_108706)
  
  ggsave(file_out,
         width=7,
         height=4,
         units='in',
         dpi=600)
}

plot7_out <- function(file_out){
  tar_load(pa_plot_108716)
  
  print(pa_plot_108716)
  
  ggsave(file_out,
         width=7,
         height=4,
         units='in',
         dpi=600)
}

plot8_out <- function(file_out){
  tar_load(pa_plot_148727)
  
  print(pa_plot_148727)
  
  ggsave(file_out,
         width=7,
         height=4,
         units='in',
         dpi=600)
}

plot9_out <- function(file_out){
  tar_load(pa_plot_149118)
  
  print(pa_plot_149118)
  
  ggsave(file_out,
         width=7,
         height=4,
         units='in',
         dpi=600)
}