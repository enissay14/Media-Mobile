
library("ggplot2");
library("RColorBrewer");

plot_timestamps <- function(df, ts) {
  header = colnames(df);
  idx_ts <- grep("X+", header, value=FALSE);
  sum_ <- sapply(df[idx_ts], sum);
  npos_ <- sapply(df[idx_ts], FUN=function(x) sum(x>0));
  
  df_p <- data.frame(ts = ts, sum = sum_, npos = npos_);
  
  title = "Attributes per timestamp";
  p = ggplot(data=df_p, aes(ts)) +
    ggtitle(title) +
    xlab("Time of Day (hours)") + ylab("Count") +
    geom_line(aes( y = sum, colour="Sum of values"), lwd=2) +
    geom_line(aes( y = npos, colour="Count of positive values"), lwd=2) + 
    theme(legend.title=element_blank());
  print(p)
  
  df_p
}


plot_links <- function(df, ndays, nbins) {
  sums = df$sum
  npos = df$npos
  avg_linksum = c() # sum of data
  avg_linknpos = c() # sum of timestamps with value > 0
  linksum = 0
  linknpos = 0
  
  for (i in 1:length(sums))
  {
    linksum = linksum + sums[i];
    linknpos = linknpos + npos[i]
    #store data for every link (every ndays rows)
    if (i %%ndays == 0)
    {
      avg_linksum = c(avg_linksum, linksum / ndays)
      avg_linknpos = c(avg_linknpos, linknpos / ndays)
      linksum = 0
      linknpos = 0
    }
  }
  
  
  df_p <- data.frame(avg_sum= avg_linksum, avg_npos = avg_linknpos);
  
  title = "Histogram of sum of data per link";
  p = ggplot(data=df_p, aes(avg_sum)) +
    ggtitle(title) +
    xlab("Sum of data") + ylab("Count") +
    geom_histogram(binwidth=10);
  print(p)
  
  title = "Histogram of sum of positive values per link";
  p = ggplot(data=df_p, aes(avg_npos)) +
    ggtitle(title) +
    xlab("Sum of data>0") + ylab("Count") +
    geom_histogram(binwidth=1);
  print(p)
  
  df_p
}

plot_days <- function(df, ndays) {
  sums = df$sum
  npos = df$npos
  daysum_ = rep(0, ndays)
  daynpos_ = rep(0, ndays)
  for (i in 1:length(sums))
  {
    idx = (i-1)%%ndays + 1; # select day index
    daysum_[idx] = daysum_[idx] + sums[i]
    daynpos_[idx] = daynpos_[idx] + npos[i]
  }
  
  df_p <- data.frame(days=1:ndays, day_sum= daysum_, day_npos = daynpos_);
  
  title = "Attributes per day";
  p = ggplot(data=df_p, aes(days)) +
    ggtitle(title) +
    xlab("Day (index)") + ylab("Total counts (all links)") +  
    geom_line(aes( y = day_sum, colour="Sum of values"), lwd=2) +
    geom_line(aes( y = day_npos, colour="Count of positive values"), lwd=2) + 
    theme(legend.title=element_blank());
  print(p)
  
  df_p
}

plot_all_timestamps <- function(df, ndays) {
  header = colnames(df);
  idx_ts <- grep("X+", header, value=FALSE)
  days = as.POSIXct(t(unique(df["day"])), tz = "Europe/Paris")
  sum_all_dt=c()
  npos_all_ts=c()
  dt = .POSIXct(character(0))
  
  for (day in days) {
    df_day = df[df$day == day,]
    sum_one_day = sapply(df_day[idx_ts], sum)
    sum_all_dt = c(sum_all_dt, sum_one_day)
    dt = c(dt, day + seq(0, 22*60*60 - 60*15, 60*15))
  }
  
  col_days = rep(days, each=88)
  col_ts = rep(0:87, ndays)
  df_p = data.frame(days= col_days, ts = col_ts, dt = dt,
                         val=sum_all_dt)
  
  title = "Sum of values per datetime";
  p = ggplot(data=df_p, aes(x = dt, y = val)) +
    ggtitle(title) +
    xlab("Datetime") + ylab("Sum of values") +  
    geom_line(aes(group=days));
  print(p)
  
  df_p
}

plot_link <- function(df, linkid, ndays) {
  begin = (linkid-1) * ndays + 1;
  end = linkid * ndays;
  header = colnames(df);
  
  #curves
  idx_ts <- grep("X+", header, value=FALSE);
  df_p <- df[begin:end,]
  
  x <- (0:87)/4;
  y <- t(df_p[,c(idx_ts)])
  col <- df_p$wday_Saturday > 0 | df_p$wday_Sunday > 0;
  
  matplot(x = x, y = y, col = col +1, type='l', lwd = 1, lty=1, main =paste0("Curves for link ", linkid),
          xlab = "hours", ylab = "counts")
  legend('topright', c("week", "weekend"), lty = 1, col = c(1,2))
  
  #basic stats
  basic_stats = c('sum', 'max', 'min', 'mean', 'sd', 'npos')
  df_p <- df[begin:end,]
  
  x <- 1:6
  y <- df_p[,basic_stats]
  y <- t(sapply(y, FUN=function(x) (x - mean(x))/max(1,sd(x)))) # normalize
  col <- df_p$wday_Saturday > 0 | df_p$wday_Sunday > 0;
  
  matplot(x = x, y = y, col = col +1, type='l', lwd = 1, lty=1, main =paste0("Curves for link ", linkid),
          xlab = "stats indicators", ylab = "normalized value", xaxt="n")
  legend('topright', c("week", "weekend"), lty = 1, col = c(1,2));
  axis(1, at=x, labels=basic_stats)
}

