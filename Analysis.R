library(lubridate)
library(ggplot2)
library(patchwork)
library(reshape2)

# 读取数据 ----
# names of the raw data files
filenames <- list.files("RawData/iNatData")
filenames <- grep(".csv", filenames, value = TRUE)

# a new list to store raw data
record <- vector("list", length = length(filenames))
names(record) <- gsub(".csv", "", filenames)

for (i in 1:length(filenames)) {
  record[[i]] <- read.csv(paste0("RawData/iNatData/", filenames[i]))
}

# 年度分析 ----
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

# 月度分析 ----
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

# 微观分析：追踪连续参与用户的变化 ----
# 输出各用户每年的观测数宽表
fun_usertrack <- function(obs_user_year) {
  obs_user_year <- obs_user_year[c("user_login", "observed_on")]
  obs_user_year$observed_on <- year(as_date(obs_user_year$observed_on))
  obs_user_year <-
    aggregate(obs_user_year$user_login,
              by = list(obs_user_year$user_login, obs_user_year$observed_on),
              FUN = "length")
  names(obs_user_year) <- c("user", "year", "obs")
  # 转换成宽数据
  obs_user_year <- dcast(obs_user_year, user ~ year, value.var = "obs")
  # 输出数据
  return(obs_user_year)
}

fun_contuser <- function(obs_user_year) {
  # 输入用户每年观测数宽表
  # 以两年为窗口，循环计算两年之间的差值
  # 建立向量，以存储各个城市2016-2020年每年相比前一年，连续用户中观测数增长的比例
  usertrack_output <- vector("numeric")
  for (i in 2016:2020) {
    # 计算两年差值
    obs_user_year[paste0("chg", i)] <-
      obs_user_year[paste(i)] - obs_user_year[paste(i-1)]
    usertrack_output <-
      c(usertrack_output,
        # 接入连续用户数总数
        sum(is.na(obs_user_year[paste0("chg", i)]) == FALSE))
  }
  # 输出观测数增长的连续用户数占总连续用户数的比例
  return(usertrack_output)
}

fun_contusermoreprop <- function(obs_user_year) {
  # 输入用户每年观测数宽表
  # 以两年为窗口，循环计算两年之间的差值
  # 建立向量，以存储各个城市2016-2020年每年相比前一年，连续用户中观测数增长的比例
  usertrack_output <- vector("numeric")
  for (i in 2016:2020) {
    # 计算两年差值
    obs_user_year[paste0("chg", i)] <-
      obs_user_year[paste(i)] - obs_user_year[paste(i-1)]
    usertrack_output <-
      c(usertrack_output,
        # 接入观测数增长的连续用户数占总连续用户数的比例
        sum(obs_user_year[paste0("chg", i)] > 0, na.rm = TRUE) /
          sum(is.na(obs_user_year[paste0("chg", i)]) == FALSE))
  }
  # 输出观测数增长的连续用户数占总连续用户数的比例
  return(usertrack_output)
}

# 生成各城市每个用户每年的观测数
obs_user_year <- lapply(record, fun_usertrack)
# 虽然下载了2015-2020的数据，但有些城市并没有2015年的数据
# 权宜之计，对于这些城市，2015年数据补全为NA
obs_user_year$Kawasaki$"2015" <- NA
obs_user_year$Saitama$"2015" <- NA

# 生成各城市每年连续用户数数据框
contuser_city_year <- lapply(obs_user_year, fun_contuser)
contuser_city_year <- as.data.frame(Reduce(rbind, contuser_city_year))
contuser_city_year$city <- names(obs_user_year)
rownames(contuser_city_year) <- NULL
names(contuser_city_year) <- c(2016:2020, "city")
contuser_city_year <- contuser_city_year[c("city", 2016:2020)]
# 转化成长表
contuser_city_year <-
  melt(contuser_city_year, id = "city",
       variable.name = "year", value.name = "contuser")
# 权宜之计：对连续用户少于10人的年份不予分析
contuser_city_year <-
  contuser_city_year[which(contuser_city_year$contuser > 10), ]

# 生成各城市每年连续用户中观测增长者比例的数据框
contusermoreprop_city_year <- lapply(obs_user_year, fun_contusermoreprop)
contusermoreprop_city_year <- as.data.frame(Reduce(rbind, contusermoreprop_city_year))
contusermoreprop_city_year$city <- names(obs_user_year)
rownames(contusermoreprop_city_year) <- NULL
names(contusermoreprop_city_year) <- c(2016:2020, "city")
contusermoreprop_city_year <- contusermoreprop_city_year[c("city", 2016:2020)]
# 转化成长表
contusermoreprop_city_year <-
  melt(contusermoreprop_city_year, id = "city",
       variable.name = "year", value.name = "contusermoreprop")

# 保留和连续用户数据框一致的条目
contuser_all <-
  merge(contuser_city_year, contusermoreprop_city_year, by = c("city", "year"))
p1 <- ggplot(contuser_city_year) + geom_col(aes(year, contuser)) +
  facet_wrap(.~city, nrow = 1)
p2 <- ggplot(contuser_all) + geom_col(aes(year, contusermoreprop)) +
  facet_wrap(.~city, nrow = 1)
p1 / p2

# 暂时的结论：大约一半的连续用户在2020年观测数比2019年多，当然这个数据也要和2018-2019年的变化量进行对比，比如从连续用户总数和连续用户中观测数增长者的占比的对比可以看出，虽然2019-2020很多城市的连续用户数上升了，但是比例却下降了，可能也反映了新冠的影响，就是说有更多人在2020年持续上传观测，这可能是平台持续增长的体现，但是同时观测增加者的比例却下降了

# 老用户行为和其他用户对比
# 老用户的观测数是否显著高于其他人？
# 挑出老用户
longtime_user_plot <- vector("list", length(record))
names(longtime_user_plot) <- names(obs_user_year)

for (name_city in names(obs_user_year)) {
  # 2015-2020年间3年有观测记录的用户定义为老用户
  obs_user_year[[name_city]]$yr_obs <-
    rowSums(is.na(obs_user_year[[name_city]]) == FALSE) - 1
  obs_user_year[[name_city]]$longtime_user <- "shorttime"
  obs_user_year[[name_city]]$longtime_user[which(
    obs_user_year[[name_city]]$yr_obs >= 3)] <- "longtime"

  obs_user_year[[name_city]] <-
    melt(obs_user_year[[name_city]], id = c("user", "longtime_user"),
         variable.name = "year", value.name = "obs")

  summary(aov(obs ~ longtime_user, data = obs_user_year[[name_city]]))
  plot <- ggplot(obs_user_year[[name_city]][
    colnames(obs_user_year[[name_city]]) != c("yr_obs")]) +
    geom_boxplot(aes(longtime_user, obs)) +
    facet_wrap(.~year, nrow = 1, scales = "free_y")
  longtime_user_plot[[name_city]] <-
    plot
}

# 老用户在各年份的上传数视觉上判断比较高
Reduce("/", longtime_user_plot[1:3])
Reduce("/", longtime_user_plot[4:7])

# 那么老用户2020年的变化是否和其他用户不同呢？
for (name_city in names(obs_user_year)) {
  # 再将各用户每年观测数据转化为宽表
  obs_user_year[[name_city]] <-
    dcast(obs_user_year[[name_city]], user + longtime_user ~ year)
  # 计算2019-2020变化率
  obs_user_year[[name_city]]$chg <-
    (obs_user_year[[name_city]]$"2020" - obs_user_year[[name_city]]$"2019") /
    obs_user_year[[name_city]]$"2019"
}

# 对各个城市的变化率做盒形图
for (name_city in names(obs_user_year)) {
  obs_user_year[[name_city]] <-
    obs_user_year[[name_city]][c("user", "longtime_user", "chg")]
  obs_user_year[[name_city]] <-
    obs_user_year[[name_city]][which(is.na(obs_user_year[[name_city]]$chg) == FALSE), ]
  obs_user_year[[name_city]] <-
    obs_user_year[[name_city]][which(obs_user_year[[name_city]]$chg < 20), ]
  plot <- ggplot(obs_user_year[[name_city]]) +
    geom_boxplot(aes(longtime_user, chg)) +
    labs(title = name_city)
  print(plot)
  # 检测老用户和其他用户统计区别
  print(name_city)
  print(summary(aov(chg ~ longtime_user, data = obs_user_year[[name_city]])))
}
# 视觉上不易判断是否有显著差别，但是，在一些城市如东京、琦玉，短期观测者2020年观测数上升，而在另外一些城市，长期观测者的观测数上升；在限制变化率小于20的情况下，AOV检测两组无统计学差异

