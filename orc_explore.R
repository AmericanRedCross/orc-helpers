## orc_explore

## purpose:  conduct preliminary explorations of OpenRiverCam data
## last update:  26 oct 2025


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
#     note 3: large downloads (e.g., > 1 yr of data) occasionally time out

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

df <- read.csv("TimeSeries-2025-10-25.csv")   # file includes data from 1 sep 2024 to 2 sep 2025


# convert dates to usable format for analysis

df_i <- df |>
  mutate(ymd = ymd(str_sub(timestamp, 1, 10)))


# explore median flows over a year, using only estimates where the fraction_velocimetry is greater than 50 percent

df_ii <- df_i %>%
  filter(q_50 > 0 & fraction_velocimetry > 50) |>
  group_by(ymd) |>
  summarise(median_q50 = median(q_50),
            max_q95 = max(q_95)) |>
  ungroup() 


# construct plots of daily flow estimates

ggplot(df_ii, aes(x = ymd, y = median_q50)) + 
    geom_line(color = "gray") +
    geom_point() +  
    ggtitle("Median flow estimates, 2024-10-25 -- 2025-10-25, Hommerich, NL") +
    xlab("Date") +
    ylab("Flow Estimate (q_50, m3/sec)")


ggplot(df_ii, aes(x = ymd)) +
    geom_line( aes(y = median_q50), color = "gold2", linewidth = 0.75) + 
    geom_line( aes(y = max_q95), color = "darkblue", linewidth = 0.65) + 
    ggtitle("Daily Flow Estimates, 2024-10-25 -- 2025-10-25, Hommerich, NL
    Gold: Median Flow; Blue: Upper 95th Percentile Flow") +
    xlab("") +
    ylab("Flow Estimate (m3/sec)")
  
df_i %>%
  filter(fraction_velocimetry > 75 & q_95 > 0 & ymd > "2025-07-31") |>
  ggplot( aes(x = ymd, y = q_95)) +
  #geom_point(shape = 16, color = "black", fill = "blue", size = 2) +
  geom_point() +
  ggtitle("Upperbound flow estimates (q_95), 2025-08-01 -- 2025-10-25, Hommerich, NL") +
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

p <-ggplot(df_v,aes(month,day,fill=q_50))+
  geom_tile(color= "lightblue",size=0.1) + 
  scale_fill_viridis(name="Median Flows, m3/sec",option ="D") +
  ggtitle("Median Flows, Hommerich, Jan 2025 - Aug 2025") +
  xlab("Month") +
  ylab("Day")

p 


# alternative heatmap:  create "github contributions" format suggested by Dan
# https://restateinsight.com/posts/general-posts/2024-12-github-contributions-plot/

library(ggplot2)
library(lubridate)
library(import)

import::from(dplyr, mutate, if_else, summarise)
import::from(tidyr, expand_grid)
import::from(forcats, fct_rev)


flow_calendar <- df_ii |>
  filter(ymd > "2024-11-03") |>   # for this, choose Monday start date
  mutate(
  # Gets the number of the week in the year
  # Start with November 2024, end with October 2025
  n_week = if_else(week(ymd) > 43, week(ymd), (week(ymd) + 52)),
  
  # n_week = week(ymd) ,
  
  # Gets weekday number - Starts the week at sunday
  n_day = wday(ymd, week_start = 1  ),
  # Weekday labels for the plot
  weekday_label = wday(ymd, week_start = 1, label = TRUE, abbr = TRUE),
  weekday_label = fct_rev(weekday_label),
  # Month labels for the plot
  month = month(ymd, label = TRUE, abbr = TRUE) )

tab <- flow_calendar |> 
  summarise(nmin = min(n_week), .by = "month")

# simple version, no titles, no legend
ggplot(flow_calendar, aes(n_week, weekday_label)) +
  geom_tile(
    aes(fill = median_q50),
    color = "white",
#    radius = unit(2, "pt"),  # for geom_rtile
    width = 0.9,
    height = 0.9
  ) +
  # Highlight the months on the horizontal axis
  scale_x_continuous(
    breaks = tab$nmin,  # use tab df to set breaks and labels; this is prob clue to a non-jan start 
    labels = as.character(tab$month),
    position = "top",
    expand = c(0, 0)
  ) +
  # Highlight days of the week on the vertical axis
  scale_y_discrete(breaks = c("Mon", "Wed", "Fri" )) +
  # Adjust color palette
  scale_fill_distiller(
    palette = "Blues",  # replaced "Greens"
    direction = 1,
    na.value = "gray95") +
  # Removes x and y labels
  labs(x = NULL, y = NULL) +
  # Removes the color legend
  guides(fill = "none") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    text = element_text(color = "black")
  )

# title + legend, median flow
ggplot(flow_calendar, aes(n_week, weekday_label, fill = median_q50 )) +
  geom_tile(
    color  = "white",
    width  = 0.9,
    height = 0.9  ) +
  # Highlight the months on the horizontal axis
  scale_x_continuous(
    breaks = tab$nmin,  # use tab df to set breaks and labels; this is prob clue to a non-jan start 
    labels = as.character(tab$month),
    position = "top",
    expand = c(0, 0)
  ) +
  # Highlight days of the week on the vertical axis
  scale_y_discrete(breaks = c("Mon", "Wed", "Fri", "Sun")) +
  # Adjust color palette
  
  scale_fill_viridis(name="Median Flow,
m3/sec",option ="H", na.value = "gray95") +
  ggtitle("Daily Median Flows, Hommerich, NL, Nov 2024 - Oct 2025") +
  xlab("") +
  ylab("") +
  labs(caption = "Days with no data are shown in gray")


# title + legend, max flows
ggplot(flow_calendar, aes(n_week, weekday_label, fill = max_q95)) +
  geom_tile(
    color = "white",
    width = 0.9,
    height = 0.9  ) +
  # Highlight the months on the horizontal axis
  scale_x_continuous(
    breaks = tab$nmin,  # use tab df to set breaks and labels; this is prob clue to a non-jan start 
    labels = as.character(tab$month),
    position = "top",
    expand = c(0, 0)
  ) +
  # Highlight days of the week on the vertical axis
  scale_y_discrete(breaks = c("Mon", "Wed", "Fri", "Sun")) +
  # Adjust color palette
  
  scale_fill_viridis(name="Maximum Flow,
m3/sec",option ="H", na.value = "gray95") +
  ggtitle("Daily Maximum Flows, Hommerich, NL, Nov 2024 - Oct 2025") +
  xlab("") +
  ylab("") +
  labs(caption = "Days with no data are shown in gray") +
  theme(plot.title = element_text(face = "bold" ) ,
        axis.text = element_text(face = "bold"))







##  references
#         https://r-graph-gallery.com/283-the-hourly-heatmap.html
#         https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r


##  other notes
#        precip data ordered from NOAA https://www.ncei.noaa.gov/cdo-web/review

##  select commit tab, add commit message

# exit

######   save script (cntl + s)

q()
