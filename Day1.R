library(dyplyr)
library(ggplot2)
library(lubridate)

df <- read.csv("Divvy_Trips_2019_Q1.csv")


# Data Cleaning and Processing
df_fil <- select(df,-c(gender,birthyear))
# correct the data type of the date
df_fil$start_time <- dmy_hm(df_fil$start_time)
df_fil$end_time <- dmy_hm(df_fil$end_time)

# create new cols for hour and weekday 
df_fil$hour <- hour(df_fil$start_time)
df_fil$weekday <- weekdays(df_fil$start_time)

df_fil$end_hour <- hour(df_fil$end_time)
df_fil$end_weekday <- weekdays(df_fil$end_time)

# Create the ride_length in minutes
df_fil$ride_length <- as.numeric(difftime(df_fil$end_time, df_fil$start_time, units = "mins"))

# Data analytic
# Basic information
colnames(df_fil)
head(df_fil)
dim(df_fil) # records = 365069 
summary(df_fil)

# User distribution
user_distribution <- table(df_fil$usertype)
prop.table(user_distribution) * 100  

# Basic statistic for ride length
df_user_sum <- aggregate(ride_length ~ usertype, df_fil, summary)
# 先检查df_user_sum的结构
print(colnames(df_user_sum))
print(str(df_user_sum))

# 方法1: 直接提取数据画图
# 平均时长条形图
mean_data <- data.frame(
  usertype = df_user_sum$usertype,
  mean_time = df_user_sum$ride_length[,"Mean"]
)

ggplot(mean_data, aes(x = usertype, y = mean_time)) +
  geom_col(fill = c("lightblue", "lightgreen")) +
  labs(title = "平均骑行时长", x = "用户类型", y = "分钟")

# 均值和中位数比较
compare_data <- data.frame(
  usertype = rep(df_user_sum$usertype, 2),
  measure = rep(c("均值", "中位数"), each = nrow(df_user_sum)),
  time = c(df_user_sum$ride_length[,"Mean"], df_user_sum$ride_length[,"Median"])
)

ggplot(compare_data, aes(x = usertype, y = time, fill = measure)) +
  geom_col(position = "dodge") +
  labs(title = "骑行时长比较", x = "用户类型", y = "分钟")

# 分布直方图对比 (保留)
ggplot(df_fil, aes(x=ride_length, fill=usertype)) +
  geom_histogram(bins=50, alpha=0.7, position="identity") +
  xlim(0, 120) + # 聚焦常见时长
  labs(title="Trip Duration Distribution by User Type",
       x="Duration (minutes)", y="Count")

# 箱型图对比
ggplot(df_fil, aes(x=usertype, y=ride_length, fill=usertype)) +
  geom_boxplot() +
  ylim(0, 60) +
  labs(title="Trip Duration Comparison", 
       x="User Type", y="Duration (minutes)")

# User distribution dataframe
user_df <- data.frame(
  usertype = names(user_distribution),
  count = as.numeric(user_distribution)
)
user_df$percentage <- round(user_df$count/sum(user_df$count)*100, 1)

# Pie chart for user distribution
ggplot(user_df, aes(x="", y=count, fill=usertype)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label=paste0(percentage,"%")), 
            position=position_stack(vjust=0.5)) +
  labs(title = "User Type Distribution") +
  theme_void()

# Regardless the usertype, what are the frequency of bike riding for each day (start)
hourly_usage <- df_fil %>% 
  group_by(hour) %>% 
  summarise(count = n())
ggplot(data=hourly_usage) +geom_col((mapping = aes(x=hour, y=count)))

# Subscriber hourly usage (start)
sub_hourly_usage <- df_fil %>% 
  filter(usertype == "Subscriber") %>% 
  group_by(hour) %>% 
  summarise(count=n())
ggplot(data=sub_hourly_usage) +geom_col((mapping = aes(x=hour, y=count)))+
  labs(title = "Subscriber Start Riding Time Frequency")

# Customer hourly usage (start)
cus_hourly_usage <- df_fil %>% 
  filter(usertype == "Customer") %>% 
  group_by(hour) %>% 
  summarise(count=n())
ggplot(data=cus_hourly_usage) +geom_col((mapping = aes(x=hour, y=count)))+
  labs(title= "Customer Start Riding Time Frequency")

# Regardless the usertype, what are the frequency of bike riding for each day (end)
end_hourly_usage <- df_fil %>% 
  group_by(end_hour) %>% 
  summarise(count = n())
ggplot(data=end_hourly_usage) +geom_col((mapping = aes(x=end_hour, y=count)))

# Subscriber hourly usage (end)
end_sub_hourly_usage <- df_fil %>% 
  filter(usertype == "Subscriber") %>% 
  group_by(end_hour) %>% 
  summarise(count=n())
ggplot(data=end_sub_hourly_usage) +geom_col((mapping = aes(x=end_hour, y=count)))+
  labs(title = "Subscriber End Riding Time Frequency")

# Customer hourly usage (end)
end_cus_hourly_usage <- df_fil %>% 
  filter(usertype == "Customer") %>% 
  group_by(end_hour) %>% 
  summarise(count=n())
ggplot(data=end_cus_hourly_usage) +geom_col((mapping = aes(x=end_hour, y=count)))+
  labs(title= "Customer End Riding Time Frequency")

# Regardless usertype, what are the riding frequency for each day
daily_usage <-df_fil %>% 
  group_by(weekday) %>% 
  summarise(count=n())
ggplot(data=daily_usage) +geom_col((mapping = aes(x= weekday, y=count)))+
  scale_x_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))+
  labs(title= "Diary Riding Frequency")

#  what are the riding frequency for each day for subscriber
sub_daily_usage <-df_fil %>% 
  filter (usertype == "Subscriber") %>% 
  group_by(weekday) %>% 
  summarise(count=n())
ggplot(data=sub_daily_usage) +geom_col((mapping = aes(x= weekday, y=count)))+
  scale_x_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))+
  labs(title= "Subscribers Diary Riding Frequency")

#  what are the riding frequency for each day for customer
cus_daily_usage <-df_fil %>% 
  filter (usertype == "Customer") %>% 
  group_by(weekday) %>% 
  summarise(count=n())
ggplot(data=cus_daily_usage) +geom_col((mapping = aes(x= weekday, y=count)))+
  scale_x_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))+
  labs(title= "Customers Diary Riding Frequency")


### Station
# How many start-station? 
length(unique(df_fil$to_station_id))
length(unique(df_fil$to_station_name))
# How many stop-station?
length(unique(df_fil$from_station_name))
length(unique(df_fil$from_station_id))


# Regardless usertype, the popularity for each station (mine)
top20_station_popularity <- df_fil %>% 
  group_by(to_station_id) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% 
  slice_head(n=20)
ggplot(data=top20_station_popularity)+geom_col(mapping = aes(x=to_station_id, y=count))
print(top20_station_popularity,n=20)

#OP (Claude)
top20_station_popularity <- df_fil %>% 
  group_by(to_station_id, to_station_name) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  arrange(desc(count)) %>% 
  slice_head(n = 20)
ggplot(data = top20_station_popularity) + 
  geom_col(mapping = aes(x = reorder(to_station_name, count), y = count)) +
  coord_flip() +  # 水平条形图，方便阅读车站名称
  labs(title = "Top 20 Most Popular Stations", 
       x = "Station Name", 
       y = "Count") +
  theme_minimal()

# the top to-station (mine... pity...)
top_to_station <- df_fil %>% 
  filter(to_station_id == 91) %>% 
  select(to_station_name) %>% 
  distinct()
top_to_station

#OP (Claude)
sub_top20_station_popularity <- df_fil %>% 
  filter(usertype =="Subscriber") %>% 
  group_by(to_station_id, to_station_name) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  arrange(desc(count)) %>% 
  slice_head(n = 20)
ggplot(data = sub_top20_station_popularity) + 
  geom_col(mapping = aes(x = reorder(to_station_name, count), y = count)) +
  coord_flip() +  # 水平条形图，方便阅读车站名称
  labs(title = "Top 20 Most Popular Stations (Subscriber)", 
       x = "Station Name", 
       y = "Count") +
  theme_minimal()

#OP (Claude)
cus_top20_station_popularity <- df_fil %>% 
  filter(usertype =="Customer") %>% 
  group_by(to_station_id, to_station_name) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  arrange(desc(count)) %>% 
  slice_head(n = 20)
ggplot(data = cus_top20_station_popularity) + 
  geom_col(mapping = aes(x = reorder(to_station_name, count), y = count)) +
  coord_flip() +  # 水平条形图，方便阅读车站名称
  labs(title = "Top 20 Most Popular Stations (Customer)", 
       x = "Station Name", 
       y = "Count") +
  theme_minimal()


