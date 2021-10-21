library(lubridate)
library(ggplot2)
library(patchwork)

# names of the raw data files
filenames <- list.files("RawData/iNatData")
filenames <- grep(".csv", filenames, value = TRUE)

# a new list to store raw data
record <- vector("list", length = length(filenames))
names(record) <- gsub(".csv", "", filenames)

for (i in 1:length(filenames)) {
  record[[i]] <- read.csv(paste0("RawData/iNatData/", filenames[i]))
}

# 去除异常值
record$Yokohama <-
  record$Yokohama[which(record$Yokohama$user_login != "jeanvaljean"), ]

# 比较年度观察总数，观察者数量，人均观察数
# 统计观察总数
fun_yearly <- function(x) {
  x[, "year"] <- year(as.Date(x[, "observed_on"]))
  x_yearly <- aggregate(x[, "year"], by = list(x[, "year"]), FUN = "length")
  names(x_yearly) <- c("year", "observation")
  return(x_yearly)
}

record_yearly <- lapply(record, fun_yearly)
for (i in names(record_yearly)) {
  record_yearly[[i]][, "city"] <- i
}
record_yearly <- Reduce(rbind, record_yearly)

# 统计观察者数量
fun_user_yearly <- function(x) {
  x[, "year"] <- year(as.Date(x[, "observed_on"]))
  x_user_yearly <- unique(x[c("year", "user_id")])
  x_user_yearly <- aggregate(x_user_yearly[, "year"],
                             by = list(x_user_yearly[, "year"]), FUN = "length")
  names(x_user_yearly) <- c("year", "users")
  return(x_user_yearly)
}

record_user_yearly <- lapply(record, fun_user_yearly)
for (i in names(record_user_yearly)) {
  record_user_yearly[[i]][, "city"] <- i
}
record_user_yearly <- Reduce(rbind, record_user_yearly)

# 人均观察数
record_obsperuser_yearly <-
  merge(record_yearly, record_user_yearly, by = c("year", "city"))
record_obsperuser_yearly$obsperuser <-
  record_obsperuser_yearly$observation / record_obsperuser_yearly$users

# plots
p1 <- ggplot(record_yearly) + geom_line(aes(year, observation)) +
  facet_wrap(.~ city, nrow = 1, scales = "free_y")
p2 <- ggplot(record_user_yearly) + geom_line(aes(year, users)) +
  facet_wrap(.~ city, nrow = 1, scales = "free_y")
p3 <- ggplot(record_obsperuser_yearly) + geom_line(aes(year, obsperuser)) +
  facet_wrap(.~ city, nrow = 1, scales = "free_y")
p1 / p2 / p3

# 月度数据比较
# 每月观察总数
fun_monthly <- function(x) {
  x[, "year"] <- year(as.Date(x[, "observed_on"]))
  x[, "month"] <- month(as.Date(x[, "observed_on"]))
  x_monthly <- aggregate(x[, "month"], by = list(x[, "year"], x[, "month"]),
                         FUN = "length")
  names(x_monthly) <- c("year", "month", "observation")
  x_monthly[, "year"] <- as.factor(x_monthly[, "year"])
  return(x_monthly)
}

record_monthly <- lapply(record, fun_monthly)
for (i in names(record_monthly)) {
  record_monthly[[i]][, "city"] <- i
}

record_monthly <- Reduce(rbind, record_monthly)
# 限定为2018年之后的数据
record_monthly <-
  record_monthly[which(record_monthly$year %in% c("2018", "2019", "2020")), ]


# 每月观察者数量
fun_user_monthly <- function(x) {
  x[, "year"] <- year(as.Date(x[, "observed_on"]))
  x[, "month"] <- month(as.Date(x[, "observed_on"]))
  x_user_monthly <- unique(x[c("year", "month", "user_id")])
  x_user_monthly <-
    aggregate(x_user_monthly[, "month"],
              by = list(x_user_monthly[, "year"], x_user_monthly[, "month"]),
              FUN = "length")
  names(x_user_monthly) <- c("year", "month", "users")
  x_user_monthly[, "year"] <- as.factor(x_user_monthly[, "year"])
  return(x_user_monthly)
}

record_user_monthly <- lapply(record, fun_user_monthly)
for (i in names(record_user_monthly)) {
  record_user_monthly[[i]][, "city"] <- i
}

record_user_monthly <- Reduce(rbind, record_user_monthly)
record_user_monthly <-
  record_user_monthly[which(record_user_monthly$year %in% c("2018", "2019", "2020")), ]

# 人均观察数
record_obsperuser_monthly <-
  merge(record_monthly, record_user_monthly, by = c("year", "month", "city"))
record_obsperuser_monthly$obsperuser <-
  record_obsperuser_monthly$observation / record_obsperuser_monthly$users

p1 <- ggplot(record_monthly) +
  geom_line(aes(x = month, y = observation, color = year)) +
  facet_wrap(.~ city, nrow = 1, scales = "free_y")
p2 <- ggplot(record_user_monthly) +
  geom_line(aes(x = month, y = users, color = year)) +
  facet_wrap(.~ city, nrow = 1, scales = "free_y")
p3 <- ggplot(record_obsperuser_monthly) +
  geom_line(aes(x = month, y = obsperuser, color = year)) +
  facet_wrap(.~ city, nrow = 1, scales = "free_y")
p1 / p2 / p3

# 2020年相比2019年每月下降多少
# 选择目标城市：2019年年用户数量达到100以上
tar_city <- record_user_yearly[which(record_user_yearly$year == 2019 &
                                       record_user_yearly$users > 100), ]$city

fun_change_monthly <- function(x, name_var) {
  change_monthly <-
    merge(x[which(x$year == "2019"), ],
          x[which(x$year == "2020"), ],
          by = c("city", "month"))
  change_monthly[, "decrease_rate"] <-
    (change_monthly[, paste0(name_var, ".y")] -
       change_monthly[, paste0(name_var, ".x")]) /
    change_monthly[, paste0(name_var, ".x")]
  change_monthly <- change_monthly[which(change_monthly$city %in% tar_city), ]
  return(change_monthly)
}

change_monthly <- fun_change_monthly(record_monthly, "observation")

change_user_monthly <- fun_change_monthly(record_user_monthly, "users")


change_obsperuser_monthly <-
  fun_change_monthly(record_obsperuser_monthly, "obsperuser")

change_monthly$month <- as.numeric(change_monthly$month)

p1 <- ggplot(change_monthly) +
  geom_rect(aes(xmin = 3.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "light blue") +
  geom_col(aes(x = month, y = decrease_rate)) +
  facet_wrap(.~city, nrow = 1)
p2 <- ggplot(change_user_monthly) +
  geom_rect(aes(xmin = 3.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "light blue") +
  geom_col(aes(x = month, y = decrease_rate)) +
  facet_wrap(.~city, nrow = 1)
p3 <- ggplot(change_obsperuser_monthly) +
  geom_rect(aes(xmin = 3.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "light blue") +
  geom_col(aes(x = month, y = decrease_rate)) +
  facet_wrap(.~city, nrow = 1)



# 结合紧急事态和疫情情况分析
# 基本上从二月~四月开始受影响
# 结合十月份的数据，可见不仅受政策，也受到实际疫情数据的影响
ggplot(record_yearly) +
  geom_rect(aes(xmin = 2016, xmax = 2018, fill = "blue")) +
  geom_line(aes(year, observation, color = city))


covid <- read.csv("RawData/nhk_news_covid19_prefectures_daily_data.csv")
covid <- covid[c("日付", "都道府県名", "各地の感染者数_1日ごとの発表数")]
names(covid) <- c("date", "prefecture", "number")
covid <-
  covid[which(covid$prefecture %in% c("京都府", "大阪府", "東京都", "神奈川県")), ]
covid$prefecture[which(covid$prefecture == "京都府")] <- "Kyoto prefecture"
covid$prefecture[which(covid$prefecture == "大阪府")] <- "Osaka prefecture"
covid$prefecture[which(covid$prefecture == "東京都")] <- "Tokyo prefecture"
covid$prefecture[which(covid$prefecture == "神奈川県")] <- "Kanagawa prefecture"

covid$date <- as.Date(covid$date)
covid[, "year"] <- year(covid$date)
covid[, "month"] <- month(covid$date)
covid <- covid[which(covid$year == "2020"), ]

covid_monthly <- aggregate(covid$number, by = list(covid$prefecture, covid$month),
                           FUN = "sum")
names(covid_monthly) <- c("prefecture", "month", "number")
covid_monthly$prefecture <- factor(covid_monthly$prefecture,
                                   levels = c("Kyoto prefecture", "Osaka prefecture",
                                              "Tokyo prefecture", "Kanagawa prefecture"))

p4 <- ggplot(covid_monthly) + geom_col(aes(x = month, number)) +
  facet_wrap(.~prefecture, nrow = 1, scales = "free_y")
p1 / p2 / p3 / p4




