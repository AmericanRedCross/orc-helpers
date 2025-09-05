## orc_explore

## purpose:  conduct preliminary explorations of OpenRiverCam data
## last update:  05 sep 2025


##  data source:  Hessel Winsemius' ORC dashboard (password protected; see Hessel's 12 Jul 2025 email)

##  data download:  
#     log in to LiveOpenRiverCam site
#     navigate to time series section (from menu on left)
#     select export button (upper right of screen)
#     select format = csv
#     select site = Hommerich
#     enter start date/time [see note 2 below]
#     enter end date/time
#     click submit
#     move downloaded file to working directory for analyses
#     note 1:  download may take 30++ seconds
#     note 2: site survey date is listed as 18 Jul 2024 and 
#             so selected start date should be after this date

##  tasks
#     preliminaries
#     import data
#     convert dates to usable format for analysis
#     explore median flows over a year
#     construct plots
#     export plots if desired by selecting desired option from export menu in plot window


# preliminaries

############### set working directory and load libraries:

library(tidyverse)
library(viridis)   
library(ggExtra) 
library(usethis)  

# import data

df <- read.csv("TimeSeries-2025-09-02.csv")   # file includes data from 1 sep 2024 to 2 sep 2025


# convert dates to usable format for analysis

df_i <- df |>
  mutate(ymd = ymd(str_sub(timestamp, 1, 10)))


# explore median flows over a year, using only estimates where the fraction_velocimetry is greater than 50 percent

df_ii <- df_i %>%
  filter(q_50 > 0 & fraction_velocimetry > 50) |>
  group_by(ymd) |>
  summarise(max_q50 = max(q_50),
            max_q95 = max(q_95)) |>
  ungroup() 


# construct plots of daily flow estimates

ggplot(df_ii, aes(x = ymd, y = max_q50)) + 
    geom_line(color = "gray") +
    geom_point() +  
    ggtitle("Median flow estimates, 2024-09-01 -- 2025-09-02, Hommerich, NL") +
    xlab("Date") +
    ylab("Flow Estimate (q_50, m3/sec)")


ggplot(df_ii, aes(x = ymd)) +
    geom_line( aes(y = max_q50), color = "gold2", linewidth = 0.75) + 
    geom_line( aes(y = max_q95), color = "darkblue", linewidth = 0.65) + 
    ggtitle("Daily Flow Estimates, 2024-09-01 -- 2025-09-02, Hommerich, NL
    Gold: Median Flow; Blue: Upper 95th Percentile Flow") +
    xlab("") +
    ylab("Flow Estimate (m3/sec)")
  
df_i %>%
  filter(fraction_velocimetry > 75 & q_95 > 0 & ymd > "2025-07-31") |>
  ggplot( aes(x = ymd, y = q_95)) +
  #geom_point(shape = 16, color = "black", fill = "blue", size = 2) +
  geom_point() +
  ggtitle("Upperbound flow estimates (q_95), 2025-08-01 -- 2025-09-02, Hommerich, NL") +
  xlab("Date") +
  ylab("Flow Estimate (q_95, m3/sec)")
  
  
# yet more plots :)


#  create plotting df for daily median flows

df_iii <- df_i %>% 
  filter(q_50 > 0, fraction_velocimetry > 50) |>
  mutate(year = year(ymd), 
         month = month(ymd, label=TRUE), 
         day = day(ymd))



df_iv <-df_iii %>% 
  select(q_50, year, month, day)


# plot daily median flows

p <-ggplot(df_iv,aes(month,day,fill=q_50))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="q_50 (m3/sec)",option ="D")
p <-p + facet_grid(year~month)
#p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df_iii$day))
#p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= "Daily Median Flows, Hommerich, 2024-2025", x="", y="Day")
p <-p + theme(legend.position = "top")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="lightblue"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  
  removeGrid()#ggExtra


p


# create plotting df for heat map

df_v <-df_iv %>% 
  filter(year > 2024) %>%
  select(q_50, year, month, day)


# plot heat map

p <-ggplot(df_iv,aes(month,day,fill=q_50))+
  geom_tile(color= "lightblue",size=0.1) + 
  scale_fill_viridis(name="Median Flows, m3/sec",option ="D") +
  ggtitle("Median Flows, Hommerich, Jan 2025 - Aug 2025") +
  xlab("Month") +
  ylab("Day")

p 



##  references
#         https://r-graph-gallery.com/283-the-hourly-heatmap.html
#         https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r


##  other notes
#        precip data ordered from NOAA https://www.ncei.noaa.gov/cdo-web/review


######   save script (cntl + s)

##  select commit tab, add commit message
##  make some more changes to file 



# exit

q()
