library(lubridate)
library(ggplot2)
library(patchwork)
library(reshape2)
library(openxlsx)

# Read data ----
## Prefectures and cities ----
pre_city <- read.xlsx("RawData/Prefectures_cities.xlsx")
pre_city <- pre_city[is.na(pre_city$city_en) == FALSE, ]

## Raw iNaturalist data ----
filenames <- list.files("RawData/iNatData")
filenames <- grep(".csv", filenames, value = TRUE)

# a new list to store raw data
record <- vector("list", length = length(filenames))
names(record) <- gsub(".csv", "", filenames)

for (i in 1:length(filenames)) {
  record[[i]] <- read.csv(paste0("RawData/iNatData/", filenames[i]))
  # 生成年月日数据
  record[[i]][, "observed_on"] <- as.Date(record[[i]][, "observed_on"])
  record[[i]][, "year"] <- year(record[[i]][, "observed_on"])
  record[[i]][, "month"] <- month(record[[i]][, "observed_on"])
  record[[i]][, "day"] <- day(record[[i]][, "observed_on"])
  # 由于有些城市只有2016年及之后的数据，所以就保留共同数据
  record[[i]] <- subset(record[[i]], year > 2015)
}

## COVID-19 data ----
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
# 合并城市所属县的数据
covid_monthly <-
  merge(covid_monthly, pre_city, by.x = "prefecture", by.y = "prefecture_en")

# Annual comparison ----
# 函数：汇总计算记录数、活跃用户数、活跃天数等数据
# 输入数据框：日期、user_id等信息
fun_smrydata <- function(x, dur = "month") {
  # 从日期中提取年月数据
  x[, "observed_on"] <- as.Date(x[, "observed_on"])
  x[, "year"] <- year(x[, "observed_on"])
  x[, "month"] <- month(x[, "observed_on"])
  x[, "day"] <- day(x[, "observed_on"])

  x$year <- factor(x$year)
  x$day <- factor(x$day)

  if (dur == "month") {
    # 记录数即每个月的数据记录条数
    x_obs <- aggregate(
      x[, "observed_on"], by = list(x$year, x$month), FUN = "length"
    )
    names(x_obs) <- c("year", "month", "obs")
    # 用户数即每个月的不重复用户数
    x_users <- unique(x[c(c("year", "month", "user_id"))])
    x_users <- aggregate(
      x_users[, "user_id"],
      by = list(x_users$year, x_users$month), FUN = "length"
    )
    names(x_users) <- c("year", "month", "users")
    # 活跃天数即每个月的各用户的活跃天数之和
    x_actdays <- unique(x[c("user_id", "year", "month", "day")])
    x_actdays <- aggregate(
      x_actdays[, "user_id"],
      by = list(x_actdays$year, x_actdays$month), FUN = "length")
    names(x_actdays) <- c("year", "month", "act_days")
    # 输出结果：默认根据year和month进行merge
    x_output <- Reduce(merge, list(x_obs, x_users, x_actdays))

    # 计算人均记录数、人均活跃天数、人均日均记录数
    x_output$obs_per_user <- x_output$obs / x_output$users
    x_output$actdays_per_user <- x_output$act_days / x_output$users
    x_output$obs_pu_pd <- x_output$obs / x_output$users / x_output$act_days
  }

  # 按年度进行数据汇总计算
  if (dur == "year") {
    # 年度记录数
    x_obs <- aggregate(x[, "observed_on"], by = list(x$year), FUN = "length")
    names(x_obs) <- c("year", "obs")
    # 用户数即每个月的不重复用户数
    x_users <- unique(x[c(c("year", "user_id"))])
    x_users <- aggregate(
      x_users[, "user_id"], by = list(x_users$year), FUN = "length"
    )
    names(x_users) <- c("year", "users")
    # 活跃天数即每个月的各用户的活跃天数之和
    x_actdays <- unique(x[c("user_id", "year", "day")])
    x_actdays <- aggregate(
      x_actdays[, "user_id"],
      by = list(x_actdays$year), FUN = "length")
    names(x_actdays) <- c("year", "act_days")
    # 输出结果：默认根据year进行merge
    x_output <- Reduce(merge, list(x_obs, x_users, x_actdays))

    # 计算人均记录数、人均活跃天数、人均日均记录数
    x_output$obs_per_user <- x_output$obs / x_output$users
    x_output$actdays_per_user <- x_output$act_days / x_output$users
    x_output$obs_pu_pd <- x_output$obs / x_output$users / x_output$act_days
  }
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

# 函数：按照列名批量生成图片并存储在列表中
fun_plot <- function(x, var_ls, dur = "month") {
  # 构建存放图片的列表
  plot_ls <- vector("list", length(var_ls))
  names(plot_ls) <- var_ls

  # 循环作图并存储
  if (dur == "month") {
    for (i in var_ls) {
      plot_ls[[i]] <- ggplot(x) +
        geom_line(aes_string("month", i, color = "year")) +
        scale_color_manual(values = c("2019" = "#00AF64", "2020" = "#bf5930")) +
        facet_wrap(.~ city, nrow = 1, scales = "free")
    }
  } else if (dur == "year") {
    for (i in var_ls) {
      plot_ls[[i]] <- ggplot(x) +
        geom_line(aes_string("year", i)) +
        facet_wrap(.~ city, nrow = 1, scales = "free")
    }
  }
  return(plot_ls)
}

tot_yrdata <- fun_ls2df(lapply(record, fun_smrydata, dur = "year"))
tot_yrdata$year <- as.numeric(tot_yrdata$year)

plot_ls <- fun_plot(tot_yrdata,
                    var_ls = c("obs", "users", "act_days", "obs_per_user",
                               "actdays_per_user", "obs_pu_pd"), dur = "year")
Reduce("/", plot_ls) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# 报告分析
# 2020年出现下降的城市
rep_tot_yrdata <- subset(tot_yrdata, year > 3)
# 函数：对比各城市2019和2020年的数据
# 输入：各城市各年份各项指标数值数据框
# 输出：对于各指标2020低于2019的城市个数并可视化
fun_comp1920 <- function(x) {
  # 将数据分成2019和2020两部分并且按照城市排序
  x1 <- subset(x, year == 4)
  x1 <- x1[order(x1$city), ]
  x2 <- subset(x, year == 5)
  x2 <- x2[order(x2$city), ]

  # 计算各项数值的差异
  # 如果2020年比2019年高则判断为TRUE
  x <- cbind(city = x1$city,
             x2[names(x2)[!names(x2) %in% c("city", "year")]] /
               x1[names(x1)[!names(x1) %in% c("city", "year")]] > 1)
  x <- as.data.frame(x)

  # 输出各项指标2020低于2019的城市数
  print(apply(x[names(x)[!names(x) %in% c("city", "year")]], 2,
              function(y) {sum(y == FALSE)}))

  # 转化成长数据并作图
  x <- melt(x, id = "city")
  ggplot(x) + geom_tile(aes(x = city, y = variable, fill = value), alpha = 0.5)
}

fun_comp1920(tot_yrdata)

# Monthly comparison ----
## General comparison of 2019-2020 ----
# 构建各年份月度数据
tot_mthdata <- fun_ls2df(lapply(record, fun_smrydata, dur = "month"))

# 添加各城市2019-2020各月记录数、活跃用户数、活跃天数和人均指标变化并可视化
tot_mthdata_1920 <- subset(tot_mthdata, year %in% c(2019, 2020))
plot_ls <- fun_plot(tot_mthdata_1920,
                    var_ls = c("obs", "users", "act_days", "obs_per_user",
                               "actdays_per_user", "obs_pu_pd"))
Reduce("/", plot_ls) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

## Varieties of idx vs. covid data ----
# 2019-2020每月同比变化
# 函数：计算2020年每月相比上年相同月份的变化率
# 输入：各城市各年月的各项指标数据
# 输出：各城市各年月的各项指标相比去年同期的变化率
fun_mthchange_1920 <- function(x) {
  # 分别构建2019年和2020年的数据框
  x_2019 <- subset(x, year == 2019)
  x_2019 <- x_2019[, -grep("year", names(x_2019))]
  x_2019 <- x_2019[order(x_2019$city, x_2019$month), ]
  x_2020 <- subset(x, year == 2020)
  x_2020 <- x_2020[, -grep("year", names(x_2020))]
  x_2020 <- x_2020[order(x_2020$city, x_2020$month), ]

  # 计算2020年相对于2019年的变化率
  x_mthchg <- cbind(
    x_2019[c("city", "month")],
    (x_2020[names(x_2020)[!names(x_2020) %in% c("city", "month")]] -
       x_2019[names(x_2019)[!names(x_2019) %in% c("city", "month")]]) /
       x_2019[names(x_2019)[!names(x_2019) %in% c("city", "month")]]
  )

  # 输出数据
  return(x_mthchg)
}

# 函数：可视化各项指标2019-2020年差异的各月变化及与新冠的关系
# 输入：各城市各年月的各项指标2019-2020年变化率数据，新冠月度数据
# 输出：可视化各月数据变化，及与新冠相关性图
fun_plot_mthchg1920_covid <- function(x) {
  x_lng <- melt(x, id = c("city", "month"))
  # 可视化各月数据变化
  p1 <- ggplot(x_lng) +
    geom_line(aes(month, value)) +
    facet_grid(variable ~ city, scales = "free")
  print(p1)
  # 2019-2020各指标变化率与新冠相关性图
  x_lng <- merge(x_lng, covid_monthly,
                 by.x = c("city", "month"), by.y = c("city_en", "month"))
  p2 <- ggplot(x_lng) +
    geom_point(aes(number, value), alpha = 0.4) +
    facet_grid(variable ~ city, scales = "free")
  print(p2)
}

# 统计检验函数
# 函数：检验各城市2020年各月不同指标数据和对应新冠感染数的相关关系
# 输入：各城市2020年各月不同指标数据数据框，各城市2020各月新冠感染数数据框
# 输出：各城市2020年各月不同指标和新冠感染数相关关系的p值数据
fun_corcovid <- function(x, covid_df) {
  # 生成空结果列表用于存储计算结果
  x_output_ls <- vector("list", 11)
  names(x_output_ls) <- unique(x$city)

  tar_var <- names(x)[!names(x) %in% c("city", "year", "month")]
  for (i in names(record)) {
    x_output_ls[[i]] <-
      apply(subset(x, city == i)[tar_var], 2,
            function(y) {
              cor.test(y, subset(covid_df, city_en == i,
                                 select = "number")$number)$p.value})
  }
  # 将结果列表转化为数据框形式
  x_output <- Reduce(rbind, x_output_ls)
  x_output <- as.data.frame(x_output)
  rownames(x_output) <- NULL
  x_output$city <- names(x_output_ls)
  x_output <- x_output[c("city", tar_var)]
  # 转化为二元数据：判断p值是否小于0.05
  x_output <-
    cbind(city = x_output$city,
          subset(x_output, select = names(x_output)[
            !names(x_output) %in% "city"]) < 0.05)
  x_output <- as.data.frame(x_output)
  # 作图可视化各指标和新冠感染数相关的城市有几个
  ggplot(melt(x_output, id = c("city"))) +
    geom_tile(aes(city, variable, fill = value), alpha = 0.6)
}

# 每月各项变化
tot_mthdata_chg_1920 <- fun_mthchange_1920(tot_mthdata)
fun_plot_mthchg1920_covid(tot_mthdata_chg_1920)
# 问题：统计检验函数出问题
# fun_corcovid(x = tot_mthdata_chg_1920, covid_df = covid_monthly)

## 环比变化率~新冠感染数 ----
# 函数：2019-2020年各项指标环比数据
# 输入：各城市各年月的各项指标数据
# 输出：各城市各年月的各项指标相比去年同期的变化率
fun_seqchg_1920 <- function(x) {
  # 构建所需数据的完整城市、年、月数据框
  x_1920 <- data.frame(
    city = rep(unique(x$city), each = 3*12),
    year = rep(rep(c(2018:2020), each = 12), length(unique(x$city))),
    month = rep(rep(1:12, 3), length(unique(x$city)))
  )
  # 将输入数据合并到该数据框
  x_1920 <- merge(x_1920, x, by = c("city", "year", "month"), all.x = TRUE)

  # 构建延迟数据列
  funin_lag <- function(y) {
    c(NA, y[1: (length(y) - 1)])
  }
  x_1920_pre <- as.data.frame(
    apply(x_1920[names(x_1920)[!names(x_1920) %in% c("city", "year", "month")]],
          2, funin_lag))
  x_1920_pre <- cbind(
    city = x_1920$city,
    year = x_1920$year,
    month = x_1920$month,
    x_1920_pre
  )

  # 构建空输出数据框
  x_output <- data.frame(
    city = x_1920$city,
    year = x_1920$year,
    month = x_1920$month
  )
  x_output <- subset(x_output, year %in% c(2019, 2020))

  # 仅保留2019和2020年目标列的数据
  x_1920 <- subset(x_1920, year %in% c(2019, 2020),
                   select = names(x_1920)[!names(x_1920) %in%
                                            c("city", "year", "month")])
  x_1920_pre <- subset(x_1920_pre, year %in% c(2019, 2020),
                       select = names(x_1920_pre)[!names(x_1920_pre) %in%
                                                c("city", "year", "month")])

  # 计算环比
  x_output <- cbind(x_output, (x_1920 - x_1920_pre) / x_1920_pre)

  # 输出结果
  return(x_output)
}

# 2019-2020年各项指标环比月度变化作图
tot_mthdata_seqchg_1920 <- fun_seqchg_1920(tot_mthdata)
ggplot(melt(tot_mthdata_seqchg_1920, id = c("city", "year", "month"))) +
  geom_line(aes(month, value, color = year)) +
  facet_grid(variable ~ city, scales = "free")

# 2020年相比前一年环比的变化率和新冠的关系
tot_mthdata_seqchg_chg_1920 <- fun_mthchange_1920(tot_mthdata_seqchg_1920)
fun_plot_mthchg1920_covid(tot_mthdata_seqchg_chg_1920)
# 问题：未能统计分析该指标和新冠感染数的关系

# User behavior ----
# 参与者人均和日均观测数的分析已在前面展示过，此处对参与者进行分组分析，看活跃用户和其他用户在疫情期间的表现有何差异

## Observation of active users ----

# 尝试2：
# 定义活跃用户为2016-2020年间2年有上传记录的用户

# 函数：输出各用户各年份观测数、活跃天数及日均观测数
fun_smrydata <- function(x) {
  # 从日期中提取年月数据
  x[, "observed_on"] <- as.Date(x[, "observed_on"])
  x[, "year"] <- year(x[, "observed_on"])
  x[, "month"] <- month(x[, "observed_on"])
  x[, "day"] <- day(x[, "observed_on"])

  x$year <- factor(x$year)
  x$day <- factor(x$day)

  # 观测数即每年的记录条数
  x_obs <- aggregate(
    x[, "observed_on"], by = list(x$user_id, x$year), FUN = "length"
  )
  names(x_obs) <- c("user", "year", "obs")
  # 活跃天数即各用户的活跃天数之和
  x_actdays <- unique(x[c("user_id", "year", "month", "day")])
  x_actdays <- aggregate(
    x_actdays[, "user_id"],
    by = list(x_actdays$user_id, x_actdays$year), FUN = "length")
  names(x_actdays) <- c("user", "year", "act_days")

  # 默认根据user和year进行merge
  x_output <- Reduce(merge, list(x_obs, x_actdays))

  # 计算用户的日均观测数
  x_output$obs_pd <- x_output$obs / x_output$act_days

  return(x_output)
}

# 标注活跃参与者并比较不同组用户的每年行为差异
# 函数：给各城市用户分组，标注活跃用户
# 输入：各城市各用户各年份观测数等数据框
fun_actpa <- function(x) {
  # 生成各城市活跃参与者名单
  actpa <- aggregate(obs ~ city + user, x, FUN = "length")
  names(actpa) <- c("city", "user", "rec_yr")
  actpa$actpa <- actpa$rec_yr >= 2

  # 标注输入数据中的活跃参与者
  x <- merge(x, actpa, by = c("city", "user"), all.x = TRUE)

  return(x)
}

user_yrdata <- fun_ls2df(lapply(record, fun_smrydata))
user_yrdata <- fun_actpa(user_yrdata)

# 查看不同分组观测数或活跃天数的差异
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr), obs)) +
  facet_wrap(.~ city, scales = "free_y")
# 视觉上判断：对于其中6个城市来说，上传年数高的参与者观察数也多
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr), act_days)) +
  facet_wrap(.~ city, scales = "free_y")
# 视觉上判断：对于其中9个城市，上传年数高的参与者活跃天数也较高
ggplot(user_yrdata) + geom_boxplot(aes(x = actpa, obs)) +
  facet_wrap(.~ city, scales = "free")
# 视觉上判断：区分效果不如上传年数好，但是也有4个左右城市活跃用户的上传数显示稍高
ggplot(user_yrdata) + geom_boxplot(aes(x = actpa, act_days)) +
  facet_wrap(.~ city, scales = "free")
# 视觉上判断：区分效果不如上传年数好，但是也有5个左右城市活跃用户的活跃天数显示稍高

# 加入年份分组看看
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr), obs)) +
  facet_grid(year ~ city, scales = "free_y")
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr), act_days)) +
  facet_grid(year ~ city, scales = "free_y")
ggplot(user_yrdata) + geom_boxplot(aes(x = actpa, obs)) +
  facet_grid(year ~ city, scales = "free_y")
ggplot(user_yrdata) + geom_boxplot(aes(x = actpa, act_days)) +
  facet_grid(year ~ city, scales = "free_y")
# 视觉判断：似乎区分效果反而不如不加年份的好

# 考虑组内样本数不足的问题，分成3个组试试看：1年，2年，3年及以上
user_yrdata$rec_yr_grp <- user_yrdata$rec_yr
user_yrdata$rec_yr_grp[which(user_yrdata$rec_yr >= 3)] <- 3

ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp), obs)) +
  facet_grid(year ~ city, scales = "free_y")
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp), act_days)) +
  facet_grid(year ~ city, scales = "free_y")
# 问题：有没有可能在疫情下不同用户之间的差异反而变大了？
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp), obs)) +
  facet_wrap(.~ city, scales = "free_y")
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp), act_days)) +
  facet_wrap(.~ city, scales = "free_y")
# 视觉判断：相比观测数，不同分组用户的活跃天数差异更大

ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp), act_days)) +
  facet_grid(city ~ year, scales = "free_y")


# 尝试3：
# 新的分组方式：根据用户是第几年参与进行分组
user_yrdata <- fun_ls2df(lapply(record, fun_smrydata))
user_yrdata$rec_yr <-
  ave(as.numeric(user_yrdata$year),
      list(user_yrdata$user, user_yrdata$city), FUN = seq_along)

# 查看不同分组观测数或活跃天数的差异
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr), obs)) +
  facet_wrap(.~ city, scales = "free")
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr), act_days)) +
  facet_wrap(.~ city, scales = "free")
# 加入年份分组
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr), obs)) +
  facet_grid(year ~ city, scales = "free_y")
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr), act_days)) +
  facet_grid(year ~ city, scales = "free_y")
# 视觉判断：相比上一种尝试，区分效果差不多，结果图像略右偏，右边的样本量减少

## Active days by city ----
# 函数：生成年度和月度活跃天数数据框
# 输入单个城市的数据，输出各用户每月的活跃天数列表
fun_actdays <- function(x, dur = "month", tarfun = "sum") {
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
  if (dur == "year") {
    # 活跃天数即月度活跃天数的汇总加和
    x_output <- aggregate(
      x_output[, "act_days"],
      by = list(x_output[, "year"]), FUN = tarfun)
    names(x_output) <- c("year", "act_days")
  }
  return(x_output)
}

# 各城市年度总活跃天数变化
useryr <- fun_ls2df(lapply(record, fun_actdays, dur = "year"))
ggplot(useryr) + geom_line(aes(year, act_days)) +
  facet_wrap(.~ city, scales = "free_y")
# 结论：一半的城市在2020年均出现活跃天数下降的情况

# 各城市年度人均活跃天数变化
useryr <- fun_ls2df(lapply(record, fun_actdays, dur = "year", tarfun = "mean"))
ggplot(useryr) + geom_line(aes(year, act_days)) +
  facet_wrap(.~ city, scales = "free_y")
# 结论：只有两个城市在2020年出现下降，所以总活跃天数的下降可能是由于总活跃用户人数下降导致的

## Active days of cont user ----
usermth <- fun_ls2df(lapply(record, fun_actdays))
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


# 解决办法：比较2019年和2020年连续用户表现之间的差异，和非连续用户之间的差异
# 函数：挑选任意两年连续用户列表
# 输入包含年份的用户数据和目标年份，自动生成该年份和前一年都有上传记录的用户
# 用户数据应当包括：city, user_id, year
fun_contuser <- function(x, taryear) {
  contuser <- x[which(x$year %in% c(taryear-1, taryear)), ]
  contuser <- unique(contuser[c("city", "user_id", "year")])
  contuser$value <- 1
  contuser <- dcast(contuser, city + user_id ~ year, value.var = "value")
  contuser$cont_value <-
    contuser[, paste(taryear - 1)] + contuser[, paste(taryear)]
  contuser <- contuser$user_id[which(contuser$cont_value == 2)]
  return(contuser)
}

usermth <- fun_ls2df(lapply(record, fun_usermth))

usermth_cont2019 <- usermth
usermth_cont2019$contuser <- "non_cont2019"
usermth_cont2019$contuser[which(
  usermth_cont2019$user_id %in% fun_contuser(usermth_cont2019, 2019))] <-
  "cont2019"

usermth_cont2020 <- usermth
usermth_cont2020$contuser <- "non_cont2020"
usermth_cont2020$contuser[which(
  usermth_cont2020$user_id %in% fun_contuser(usermth_cont2020, 2020))] <-
  "cont2020"

# 对比2019和2020年连续用户活跃天数差异
usermth_cont_2019_2020 <-
  rbind(usermth_cont2019[which(usermth_cont2019$contuser == "cont2019"), ],
        usermth_cont2020[which(usermth_cont2020$contuser == "cont2020"), ])
ggplot(usermth_cont_2019_2020) +
  geom_boxplot(aes(contuser, act_days)) +
  facet_wrap(.~ city, scales = "free_y")
# 按城市分组做统计检验
for (i in tar_city) {
  print(i)
  print(summary(
    aov(act_days ~ contuser,
        data = usermth_cont_2019_2020[which(usermth_cont_2019_2020$city == i), ])))
}
# 结论：2019的连续用户和2020年的连续用户活跃天数并无差异

# 对比2019和2020年非连续用户活跃天数差异
usermth_noncont_2019_2020 <-
  rbind(usermth_cont2019[which(usermth_cont2019$contuser == "non_cont2019"), ],
        usermth_cont2020[which(usermth_cont2020$contuser == "non_cont2020"), ])
ggplot(usermth_noncont_2019_2020) +
  geom_boxplot(aes(contuser, act_days)) +
  facet_wrap(.~ city, scales = "free_y")
# 按城市分组做统计检验
for (i in tar_city) {
  print(i)
  print(summary(
    aov(act_days ~ contuser,
        data = usermth_noncont_2019_2020[which(usermth_noncont_2019_2020$city == i), ])))
}
# 结论：东京和横滨的非连续用户在2020年有显著下降


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

