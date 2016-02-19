library("dummies")

setup <- function(){
  rm(list=ls());
  Sys.setenv(LANG = "en_US.UTF-8");
  Sys.setlocale("LC_TIME", "C")
  cat("\014");
}

load_mm_data <- function(file) {
#   df = read.csv(file, sep=";");
  df = read.csv(file, sep=";", nrows = 5900);
  return(df)
}

format_mm_data <- function(df){
  #remove scientif notation
  options(scipen=999)
  
  #check type
  #sapply(df, class)
  
  #change type of day
  df["day"] <- as.POSIXct(df$day, tz = "Europe/Paris")
  
  #change linkids : not the safest way, but fast :)
  nlines <- dim(df)[1]
  ndays <- 59
  nlinks <- nlines/ndays
  df["link"] <- rep(1:nlinks, each=ndays)
  

  df
}

add_date_features <- function(df){
  #weekday
  df["wday"] <- sapply(df["day"], weekdays)
  df <- dummy.data.frame(df, names = "wday", sep="_")
  
  # holiday
  #...
  
  #public holiday
  #...
  
  df
}

add_curve_features <- function(df){
  header = colnames(df);
  idx_ts <- grep("X+", header, value=FALSE)
  
  basic_stats = c('sum', 'max', 'min', 'mean', 'sd', 'npos')
  df[,basic_stats] <-  
    t(apply(df[idx_ts], 1, FUN=function(x) c(
      sum(x), max(x), min(x), mean(x), sd(x), sum(x>0))))
  
  df
}

#retrieve timestamps
# library("chron")
# header = colnames(df);
# idx_ts = grep("X+", header, value=FALSE)
# timestamps= header[idx_ts]
#str_ts = substring(timestamps, 2, 9)
# test = strptime(timestamps, "X%H.%M.%S")
# as.times(test)
