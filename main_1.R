
dir <- "C:\Users\Hugo\Desktop\Big data";
setwd(dir);
source("load_data_1.R");
setup();

#load data
file <- "C:\\Users\\Hugo\\Desktop\\Big data\\counts_1.csv";
df <- load_mm_data(file)
df <- format_mm_data(df)

#augment df
df <- add_date_features(df)
df <- add_curve_features(df)

df <- df[!(as.character(df$day) == "2014-12-31" | as.character(df$day) == "2014-12-30"),]
df <- df[-(3:10)]
"on tronque le tableau ici"

#data exploration
source("C:\\Users\\Hugo\\Desktop\\Big data\\data_exploration_1.R")
hours = 0:87 / 4
ndays = 57
df_p <- plot_timestamps(df, hours)
df_p[df_p$npos == 0,] # display timestamps with no data

df_p <- plot_links(df, ndays, nbins)
sum(df_p$avg_npos < 24); # display links with less than 1/4 of data in avg
sum(df_p$avg_sum < 100); # display links with less than 100 counts in avg

df_p <- plot_days(df, ndays)

df_p = plot_all_timestamps(df, ndays)
df_p[df_p$val == 0,]$dt #display datetimes with no values

linkid = 15;
plot_link(df, linkid, ndays)
