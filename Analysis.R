library(lubridate)
library(ggplot2)
library(patchwork)
library(reshape2)
library(openxlsx)

# 读取数据 ----
# 读取目标县名和城市名对应文件
pre_city <- read.xlsx("RawData/Prefectures_cities.xlsx")
pre_city <- pre_city[is.na(pre_city$city_en) == FALSE, ]

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
tar_city <- names(record)

fun_mthchange_2019_2020 <- function(x, name_var) {
  change_monthly <-
    merge(x[which(x$year == "2019"), ],
          x[which(x$year == "2020"), ],
          by = c("city", "month"))
  # 计算2020年相对于2019年的变化率
  change_monthly[, "change_rate"] <-
    (change_monthly[, paste0(name_var, ".y")] -
       change_monthly[, paste0(name_var, ".x")]) /
    change_monthly[, paste0(name_var, ".x")]
  # 留下目标城市的数据
  change_monthly <- change_monthly[which(change_monthly$city %in% tar_city), ]
  return(change_monthly)
}

change_monthly <- fun_mthchange_2019_2020(record_monthly, "observation")

change_user_monthly <- fun_mthchange_2019_2020(record_user_monthly, "users")
# 按照从北到南对城市进行排序
change_user_monthly$city <-
  factor(change_user_monthly$city, levels = pre_city$city_en)

change_obsperuser_monthly <-
  fun_mthchange_2019_2020(record_obsperuser_monthly, "obsperuser")
# 按照从北到南对城市进行排序
change_obsperuser_monthly$city <-
  factor(change_obsperuser_monthly$city, levels = pre_city$city_en)

change_monthly$month <- as.numeric(change_monthly$month)
# 按照从北到南对城市进行排序
change_monthly$city <- factor(change_monthly$city, levels = pre_city$city_en)

p1 <- ggplot(change_monthly) +
  geom_rect(aes(xmin = 3.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "light blue") +
  geom_col(aes(x = month, y = decrease_rate)) +
  facet_wrap(.~city, nrow = 1, scales = "free_y")
p2 <- ggplot(change_user_monthly) +
  geom_rect(aes(xmin = 3.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "light blue") +
  geom_col(aes(x = month, y = change_rate)) +
  facet_wrap(.~city, nrow = 1)
p3 <- ggplot(change_obsperuser_monthly) +
  geom_rect(aes(xmin = 3.5, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "light blue") +
  geom_col(aes(x = month, y = decrease_rate)) +
  facet_wrap(.~city, nrow = 1)


# 紧急事态和疫情情况分析
# 基本上从二月~四月开始受影响
# 结合十月份的数据，可见不仅受政策，也受到实际疫情数据的影响
# 各县感染者数量
covid <- read.csv("RawData/nhk_news_covid19_prefectures_daily_data.csv")
covid <- covid[c("日付", "都道府県名", "各地の感染者数_1日ごとの発表数")]
names(covid) <- c("date", "prefecture", "number")

covid <-
  covid[which(covid$prefecture %in% pre_city$prefecture_jp), ]
covid <- merge(covid, pre_city, by.x = "prefecture", by.y = "prefecture_jp")

covid$date <- as.Date(covid$date)
covid[, "year"] <- year(covid$date)
covid[, "month"] <- month(covid$date)
covid <- covid[which(covid$year == "2020"), ]

covid_monthly <- aggregate(covid$number, by = list(covid$prefecture_en, covid$month),
                           FUN = "sum")
names(covid_monthly) <- c("prefecture", "month", "number")
# 权宜之计：按照从北到南对城市进行排序
covid_monthly$prefecture <-
  factor(covid_monthly$prefecture, levels = unique(pre_city$prefecture_en))

p4 <- ggplot(covid_monthly) + geom_col(aes(x = month, number)) +
  facet_wrap(.~prefecture, nrow = 1, scales = "free_y")
p1 / p2 / p3 / p4

## 环比变化率~新冠感染数 ----
# 2020年每个月环比相比于2019年对应环比的变化率和新冠感染数的关系？
fun_seqchg <- function(x) {
  # 更改输入数据的月份为数字格式后按照城市、年、月排序
  x$month <- as.numeric(x$month)
  x <- x[order(x$city, x$year, x$month), ]
  # 新建一列时间步长差一个月的数据并计算环比
  x$pre <- c(NA, x$user[1: length(x$year)-1])
  x$seqchg <- (x$user - x$pre) / x$pre
  return(x)
}

mthseqchg <- fun_seqchg(record_user_monthly)
ggplot(mthseqchg[which(mthseqchg$year %in% c(2019, 2020)), ]) +
  geom_line(aes(month, seqchg, color = year)) +
  facet_wrap(.~ city, scales = "free_y")
# 结论：视觉上看并无明显规律，但上半年似乎影响较大，本来环比增长较多的月份在2020年环比增长较少

# 再看看两年环比变化率和新冠感染数的关系
covid_monthly <-
  merge(covid_monthly, pre_city, by.x = "prefecture", by.y = "prefecture_en")

fun_seqchg_chg_2019_2020 <- function(x) {
  # 更改输入数据的月份为数字格式后按照城市、年、月排序
  x$month <- as.numeric(x$month)
  x <- x[order(x$city, x$year, x$month), ]
  # 新建一列时间步长差一个月的数据并计算环比
  x$pre <- c(NA, x$user[1: length(x$year)-1])
  x$seqchg <- (x$user - x$pre) / x$pre
  # 计算2019-2020环比变化率
  x_output <-
    merge(x[which(x$year == "2019"), ],
          x[which(x$year == "2020"), ],
          by = c("city", "month"))
  x_output$seqchg_chg <-
    (x_output$pre.y - x_output$pre.x) /
    x_output$pre.x
  return(x_output)
}
mthseqchg_chg <- fun_seqchg_chg_2019_2020(record_user_monthly)

# 合并环比变化率数据和新冠感染数据
mthseqchg_chg <-
  merge(mthseqchg_chg, covid_monthly,
        by.x = c("city", "month"), by.y = c("city_en", "month"))
# 区分上下半年
mthseqchg_chg$phase <- "before"
mthseqchg_chg$phase[which(mthseqchg_chg$month > 6)] <- "after"
ggplot(mthseqchg_chg) + geom_point(aes(seqchg_chg, number, color = phase), alpha = 0.5) +
  facet_wrap(.~ city, scales = "free")
# 查看相关性分析统计结果
for (i in tar_city) {
  fit <- cor.test(mthseqchg_chg[mthseqchg_chg$city == i, ]$number,
                  mthseqchg_chg[mthseqchg_chg$city == i, ]$users.y)
  cat(i, "\n",
      cor(mthseqchg_chg[mthseqchg_chg$city == i, ]$seqchg_chg,
          mthseqchg_chg[mthseqchg_chg$city == i, ]$number), "\n",
      fit$p.value, "\n")
}
# 结论：正负相关均有，且少有显著关系

# 微观分析：追踪连续参与用户的变化 ----
## Observation of cont users ----
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

## Active days ----
# 函数：生成年度活跃天数数据框
# 输入单个城市的数据，输出各用户每月的活跃天数列表
fun_usermth <- function(x) {
  # 从日期中提取年月数据
  x[, "observed_on"] <- as.Date(x[, "observed_on"])
  x[, "year"] <- year(x[, "observed_on"])
  x[, "month"] <- month(x[, "observed_on"])
  x[, "day"] <- day(x[, "observed_on"])
  # 活跃天数即每个月每个用户的活跃天数计数
  x_output <- unique(x[c("user_id", "year", "month", "day")])
  x_output <- aggregate(
    x_output[, "user_id"],
    by = list(x_output[, "user_id"], x_output[, "year"], x_output[, "month"]),
    FUN = "length")
  names(x_output) <- c("user_id", "year", "month", "act_days")
  return(x_output)
}

# 将多城市数据合并为一个数据框
fun_ls2df <- function(x) {
  # 给列表中每个元素数据框加上对应的元素名
  for (i in names(x)) {
    x[[i]][, "city"] <- i
  }
  x <- Reduce(rbind, x)
  colnames_x <- c("city", names(x)[!names(x) %in% "city"])
  x <- x[colnames_x]
  return(x)
}

usermth <- fun_ls2df(lapply(record, fun_usermth))
ggplot(usermth) + geom_line(aes(month, act_days, color = user_id)) +
  facet_grid(year ~ city, scales = "free")

# 挑出2019-2020连续用户
contuser <- usermth[which(usermth$year %in% c(2019, 2020)), ]
contuser <- unique(contuser[c("city", "user_id", "year")])
contuser$value <- 1
contuser <- dcast(contuser, city + user_id ~ year, value.var = "value")
contuser$cont_value <- contuser$`2019` + contuser$`2020`
contuser$contuser <- "non_cont"
contuser$contuser[which(contuser$cont_value == 2)] <- "cont"
comment(contuser) <- "2019-2020连续用户列表"

# 合并连续用户列表并保留2019和2020年数据
usermth <- merge(usermth, contuser[c("city", "user_id", "contuser")],
                 by = c("city", "user_id"))
usermth <- usermth[which(usermth$year %in% c(2019, 2020)), ]
usermth$month <- as.factor(usermth$month)
ggplot(usermth[which(usermth$year == 2020), ]) +
  geom_boxplot(aes(contuser, act_days, color = contuser)) +
  facet_wrap(.~ city, scales = "free_y")

for (i in tar_city) {
  print(i)
  print(summary(aov(act_days ~ contuser,
                    data = usermth[which(usermth$city == i & usermth$year == 2020), ])))
}

# 结论：实际上2019和2020年，连续用户的活跃天数都大于非连续用户数。要如何比较两年的两组差异呢？是否可以比较两者的连续用户平均值和非连续用户平均值？

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

# 换个思路：将2019-2020年上传增加的用户挑出来看有什么特点
# 重新构建每个用户每年上传记录列表
obs_user_year <- lapply(record, fun_usertrack)
# 虽然下载了2015-2020的数据，但有些城市并没有2015年的数据
# 权宜之计，对于这些城市，2015年数据补全为NA
obs_user_year$Kawasaki$"2015" <- NA
obs_user_year$Saitama$"2015" <- NA

# 挑出2019-2020增长的用户
fun_change_rate <- function(x, name_year_start = "2019", name_year_end = "2020") {
  x[, "chg"] <-
    (x[, name_year_end] - x[, name_year_start]) / x[, name_year_start]
  x <- x[is.na(x$chg) == FALSE, ]
  x[, "mean"] <- rowMeans(x[!colnames(x) %in% c("user", "chg")], na.rm = TRUE)
  return(x)
}
obs_user_year <- lapply(obs_user_year, fun_change_rate)

# 查看各个城市2019年上传数、2020年上传数、多年平均数和2019-2020变化率的关系
for (name_city in names(obs_user_year)) {
  plot(obs_user_year[[name_city]][c("2019", "2020", "chg", "mean")], main = name_city)
}
# 基本结论：没有关系

# 物种鉴别数据分析 ----
# 疫情期间鉴别率是否上升？
fun_id_rate <- function(x) {
  # 保留观测时间和鉴定者数数据
  x <- x[c("observed_on", "num_identification_agreements",
           "num_identification_disagreements")]
  x[, "year"] <- year(as_date(x[, "observed_on"]))
  x[, "num_id"] <- x[, "num_identification_agreements"] +
    x[, "num_identification_disagreements"]
  # 汇总计算每年的鉴定率
  # 每年总观测数
  x_agg <-
    aggregate(x[, "num_identification_agreements"],
              by = list(x[, "year"]), FUN = "length")
  # 其中鉴定数大于0的观测数
  x_agg[, "identified"] <-
    aggregate(x[which(x[, "num_id"] > 0), "num_identification_agreements"],
              by = list(x[which(x[, "num_id"] > 0), "year"]),
              FUN = "length")[, 2]
  names(x_agg) <- c("year", "obs_tot", "obs_id")
  x_agg[, "id_rate"] <- x_agg[, "obs_id"] / x_agg[, "obs_tot"]
  return(x_agg)
}

id_year <- lapply(record, fun_id_rate)

# 作图：各城市2015-2020年鉴定率变化
par(mfrow = c(3, 4))
for (name_city in names(id_year)) {
  plot(id_year[[name_city]]$year, id_year[[name_city]]$id_rate, type = "l",
       main = name_city, xlab = "", ylab = "id_rate")
}
# 结论：2020年鉴定率升高的城市只是少数

