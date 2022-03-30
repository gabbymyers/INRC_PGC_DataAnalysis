library(apsimx)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(plotly)

#Nashua Rainfall data for 30 years from Iowa Envrionmental Mesonet
nashua.iem <- get_iem_apsim_met(lonlat = c(-92.56990, 42.93709), 
                               dates = c("1991-01-01", "2021-12-31"))

#creating a column for cumulative rain by year and exporting the entire new thing as a dataframe 
cumulative <- nashua.iem %>%
  group_by(year) %>%
  mutate(cumulative_rain = cumsum(rain))

#dataframe with average cumulative rain for each day 
day_avg <- cumulative %>%
  group_by(day) %>%
  mutate(avg_cumulative_rain = mean(cumulative_rain))

#adding a column to make it easier to subset
day_avg$count <- 1:nrow(day_avg)

#subsetting the repeated values into one list of average daily cumulative rain
#and choosing a leap year because it contains 1-366, so 366-731 correspond to 1992
day_avg_small <-subset(day_avg,day_avg$count>365 & day_avg$count<732)

plot(day_avg_small$day, day_avg_small$avg_cumulative_rain)

day_avg_small <- day_avg_small[, c("day", "avg_cumulative_rain")]


fig_average<- plot_ly(day_avg_small, x = ~day, y = ~avg_cumulative_rain, type = 'scatter', mode = 'lines+markers')
fig_average
##looks good!

#dataframe of only 2021
year_2021 <- subset(cumulative,cumulative$year == 2021)

year_2021_cumul_only <- year_2021[, c("day", "cumulative_rain")]

#renaming column of 2021 subset so I can combine the dataframes
year_2021_cumul_only <- year_2021_cumul_only %>%
  rename(cumulative_rain_2021 = cumulative_rain)

#merging average data and 2021 data 
merged_data<- merge(year_2021_cumul_only,day_avg_small, by = "day")


#Interactive figure with a line for average and a line for 2021
fig <- plot_ly(merged_data, x = ~day, y = ~avg_cumulative_rain, name = '1991-2021 avg', type = 'scatter', mode = 'lines') 
fig <- fig %>% add_trace(y = ~cumulative_rain_2021, name = '2021', mode = 'lines') 


fig

fig <- fig%>%
  layout(title = 'Nashua Rainfall Summary', font=list(size = 18)) %>%
  layout(xaxis = list(title = 'Day of Year'), yaxis = list(title = 'Rainfall (mm)')) %>%
  layout( xaxis = list(titlefont = list(size = 15), tickfont = list(size = 12)),
          yaxis = list(titlefont = list(size = 15), tickfont = list(size = 12)) )%>%
  layout(legend = list( font = list(size = 12, color = "#000"), bgcolor = "white"))
fig

#save fig as interactive html file
htmlwidgets::saveWidget(as_widget(fig), "rainfallplot2021.html")




