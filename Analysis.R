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
  # 在原数据基础上加上鉴定者数列
  x$idpa <- x$num_identification_agreements +
    x$num_identification_disagreements

  # 月度和年度计算方法不同
  if (dur == "month") {
    # 记录数即每个月的数据记录条数
    x_obs <- aggregate(observed_on ~ year + month, data = x, FUN = "length")
    names(x_obs) <- c("year", "month", "obs")

    # 有鉴定的观测数：鉴定用户数大于0的观测数
    x_obsid <-
      aggregate(idpa ~ year + month, data = x,
                FUN = function(y) {sum(y > 0)})
    names(x_obsid) <- c("year", "month", "obs_id")

    # 历年参与鉴定的用户数
    x_idpa <- aggregate(idpa ~ year + month, data = x, FUN = "sum")

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

  } else if (dur == "year") {
    # 历年观测数
    x_obs <- aggregate(x[, "observed_on"], by = list(x$year), FUN = "length")
    names(x_obs) <- c("year", "obs")

    # 历年有鉴定的观测数
    # 判断鉴定用户数大于0的观测数
    x_obsid <-
      aggregate(idpa ~ year, data = x,
                FUN = function(y) {sum(y > 0)})
    names(x_obsid) <- c("year", "obs_id")

    # 历年参与鉴定的用户数
    x_idpa <- aggregate(idpa ~ year, data = x, FUN = "sum")

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
  }

  # 合并结果：默认根据year或者year + month进行merge
  x_output <- Reduce(merge, list(x_obs, x_obsid, x_idpa, x_users, x_actdays))

  # 计算人均记录数、人均活跃天数、人均日均记录数、观测鉴定率
  x_output$obs_per_user <- x_output$obs / x_output$users
  x_output$actdays_per_user <- x_output$act_days / x_output$users
  x_output$obs_pu_pd <- x_output$obs / x_output$users / x_output$act_days
  x_output$id_rate <- x_output$obs_id / x_output$obs

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
fun_plot <- function(x, var_ls, plotname, dur = "month") {
  # 构建存放图片的列表
  plot_ls <- vector("list", length(var_ls))
  names(plot_ls) <- var_ls

  # 循环作图并存储
  if (dur == "month") {
    for (i in 1:length(var_ls)) {
      # 将年份设置为因子以便分别赋予颜色
      x$year <- factor(x$year)
      plot_ls[[i]] <- ggplot(x) +
        geom_line(aes_string("month", var_ls[i], color = "year")) +
        scale_x_continuous(breaks = c(3, 6, 9, 12)) +
        scale_color_manual(values = c("2019" = "#00AF64", "2020" = "#bf5930")) +
        labs(title = plotname[i], y = NULL, x = "") +
        facet_wrap(.~ city, nrow = 1, scales = "free")
    }
  } else if (dur == "year") {
    for (i in 1:length(var_ls)) {
      plot_ls[[i]] <- ggplot(x) +
        geom_line(aes_string("yr_sht", var_ls[i])) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = plotname[i], y = NULL, x = "") +
        facet_wrap(.~ city, nrow = 1, scales = "free")
    }
  }
  return(plot_ls)
}

tot_yrdata <- fun_ls2df(lapply(record, fun_smrydata, dur = "year"))
# 生成年份缩写列用于作图
tot_yrdata$yr_sht <- tot_yrdata$year - 2000

# 作图：各城市各指标历年变化
plot_ls <-
  fun_plot(tot_yrdata,
           var_ls =
             c("obs", "users", "act_days",
               "obs_per_user",
               "actdays_per_user",
               "obs_pu_pd",
               "idpa", "id_rate"),
           plotname =
             c("(a) Observation", "(b) Participant", "(c) Active days",
               "(d) Observations per participant",
               "(e) Active days per participant",
               "(f) Observations per participant per active days",
               "(g) Identification participant", "(h) Identification rate"),
           dur = "year")
png(filename = "历年各项指标变化.png", res = 300,
    width = 3000, height = 4500)
Reduce("/", plot_ls) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()

# 报告分析
# 2020年出现下降的城市
rep_tot_yrdata <- subset(tot_yrdata, year > 3)
# 函数：对比各城市2019和2020年的数据
# 输入：各城市各年份各项指标数值数据框
# 输出：对于各指标2020低于2019的城市个数并可视化
fun_comp1920 <- function(x) {
  # 将数据分成2019和2020两部分并且按照城市排序
  x1 <- subset(x, year == 2019)
  x1 <- x1[order(x1$city), ]
  x2 <- subset(x, year == 2020)
  x2 <- x2[order(x2$city), ]

  # 计算各项数值的差异
  # 如果2020年比2019年高则判断为TRUE
  x <- cbind(city = x1$city,
             ifelse(x2[names(x2)[!names(x2) %in% c("city", "year")]] /
                      x1[names(x1)[!names(x1) %in% c("city", "year")]] > 1,
                    "20 > 19", "20 <= 19"))
  x <- as.data.frame(x)

  # 输出各项指标2020低于2019的城市数
  print(apply(x[names(x)[!names(x) %in% c("city", "year")]], 2,
              function(y) {sum(y == FALSE)}))

  # 转化成长数据并作图
  x <- melt(x, id = "city")
  ggplot(x) +
    geom_tile(aes(x = city, y = variable, fill = value), alpha = 0.5) +
    theme(axis.text.x = element_text(angle = 90))
}
fun_comp1920(tot_yrdata)

# 室内参与和室外参与的关系？
# 检测总体数据的参与者和鉴定者数量的关系
cor.test(tot_yrdata$users, tot_yrdata$idpa)
# 虽然按城市分组的话，每个城市只有5个样本，但是也可以看看情况如何
lapply(split(tot_yrdata, tot_yrdata$city),
       function(x) {cor.test(x$user, x$idpa)})
# 检测结果发现，一些既往研究认为公民科学由室外转向室内，但是该分析表明两者同增
# 共减，可以作为检验“新冠期间公民科学由室外转向室内”假说的参考，但是这里有点逻
# 辑问题：无法准确说明2019-2020的情况。

# Monthly comparison ----
## General comparison of 2016-2020 and 2019-2020 ----
# 构建各年份月度数据
tot_mthdata <- fun_ls2df(lapply(record, fun_smrydata, dur = "month"))
png(filename = "历年各月各项指标变化.png", res = 300,
    width = 3000, height = 4500)
plot_ls <- fun_plot(
  tot_mthdata,
  var_ls =
    c("obs", "users", "act_days",
      "obs_per_user",
      "actdays_per_user",
      "obs_pu_pd",
      "idpa", "id_rate"),
  plotname =
    c("(a) Observation", "(b) Participant", "(c) Active days",
      "(d) Observations per participant",
      "(e) Active days per participant",
      "(f) Observations per participant per active days",
      "(g) Identification participant", "(h) Identification rate")
)
Reduce("/", plot_ls) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()

# 添加各城市2019-2020各月记录数、活跃用户数、活跃天数和人均指标变化并可视化
tot_mthdata_1920 <- subset(tot_mthdata, year %in% c(2019, 2020))

png(filename = "19-20年各月各项指标变化.png", res = 300,
    width = 3000, height = 4500)
plot_ls <- fun_plot(
  tot_mthdata_1920,
  var_ls =
    c("obs", "users", "act_days",
      "obs_per_user",
      "actdays_per_user",
      "obs_pu_pd",
      "idpa", "id_rate"),
  plotname =
    c("(a) Observation", "(b) Participant", "(c) Active days",
      "(d) Observations per participant",
      "(e) Active days per participant",
      "(f) Observations per participant per active days",
      "(g) Identification participant", "(h) Identification rate")
  )
Reduce("/", plot_ls) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()

## Metrics ~ COVID data ----
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
fun_corcovid <- function(x, testvar, covid_df) {
  # 建立空数据框用于存储数据
  x_output <- vector("list", length = length(testvar))
  names(x_output) <- testvar

  # 将2020年公民科学数据和新冠数据合并
  x_covid <-
    merge(x, covid_df,
          by.x = c("city", "month"), by.y = c("city_en", "month"),
          all.x = TRUE)

  # 按照城市分成分组列表
  x_ls <- split(x_covid, x_covid$city)

  # 分城市进行相关性检验
  for (i in testvar) {
    cor_ls <- lapply(x_ls, function(y) {
      cor.test(y[, i], y$number)$p.value < 0.05
    })
    x_output[[i]] <- Reduce(rbind, cor_ls)
  }

  # 将结果列表合并为数据框
  x_output <- Reduce(cbind, x_output)
  x_output <- as.data.frame(x_output)
  x_output <- cbind(names(x_ls), x_output)
  names(x_output) <- c("city", testvar)
  rownames(x_output) <- NULL
  print(x_output)

  # 结果可视化
  ggplot(melt(x_output, id = "city")) +
    geom_tile(aes(x = city, y = variable, fill = value), alpha = 0.6)
}

# 每月各项变化
tot_mthdata_chg_1920 <- fun_mthchange_1920(tot_mthdata)
fun_plot_mthchg1920_covid(tot_mthdata_chg_1920)
fun_corcovid(x = tot_mthdata_chg_1920,
             testvar = c("obs", "users", "act_days", "obs_per_user",
                         "actdays_per_user", "obs_pu_pd",
                         "idpa", "id_rate"),
             covid_df = covid_monthly)

## Seq and seq-change ~ COVID ----
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
  geom_line(aes(month, value, color = factor(year))) +
  facet_grid(variable ~ city, scales = "free")

# 2020年相比前一年环比的变化率和新冠的关系
tot_mthdata_seqchg_chg_1920 <- fun_mthchange_1920(tot_mthdata_seqchg_1920)
fun_plot_mthchg1920_covid(tot_mthdata_seqchg_chg_1920)
fun_corcovid(x = tot_mthdata_seqchg_chg_1920,
             testvar = c("obs", "users", "act_days", "obs_per_user",
                         "actdays_per_user", "obs_pu_pd",
                         "idpa", "id_rate"),
             covid_df = covid_monthly)

# User behavior ----
# 参与者人均和日均观测数的分析已在前面展示过，此处对参与者进行分组分析，看活跃用
# 户和其他用户在疫情期间的表现有何差异

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
# 视觉上判断：区分效果不如上传年数好，但是也有5个左右城市活跃用户的活跃天数显示
# 稍高

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


# 尝试3：
# 分组：根据用户是第几年参与进行分组-第1年，第2年，第3年及以上
user_yrdata <- fun_ls2df(lapply(record, fun_smrydata))
user_yrdata$rec_yr <-
  ave(as.numeric(user_yrdata$year),
      list(user_yrdata$user, user_yrdata$city), FUN = seq_along)
user_yrdata$rec_yr_grp <-
  user_yrdata$rec_yr
user_yrdata$rec_yr_grp[which(user_yrdata$rec_yr >= 3)] <- 3

# 查看不同分组观测数或活跃天数的差异
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp), obs)) +
  facet_wrap(.~ city, scales = "free")
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp), act_days)) +
  facet_wrap(.~ city, scales = "free")
# 加入年份分组
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp), obs)) +
  facet_grid(year ~ city, scales = "free_y")
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp), act_days)) +
  facet_grid(year ~ city, scales = "free_y")
# 视觉判断：相比上一种尝试，区分效果差不多，结果图像略右偏，右边的样本量减少

# 合并用户分组再看效果
user_yrdata$rec_yr_grp_less <- user_yrdata$rec_yr_grp
user_yrdata$rec_yr_grp_less[which(user_yrdata$rec_yr_grp >= 2)] <- 2
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp_less), obs)) +
  facet_wrap(.~ city, scales = "free")
ggplot(user_yrdata) + geom_boxplot(aes(x = factor(rec_yr_grp_less), act_days)) +
  facet_wrap(.~ city, scales = "free")

# 统计检验分指标各城市跨用户组差异
user_yrdata_ls <- vector("list", 3)
testvar <- c("obs", "act_days", "obs_pd")
for (i in 1:3) {
  user_yrdata_ls[[i]] <-
    subset(user_yrdata,
           select = c("city", "user", "year", "rec_yr_grp_less", testvar[i]))
  names(user_yrdata_ls[[i]]) <-
    c("city", "user", "year", "rec_yr_grp_less", "tarvar")
}

fun_meanse_calc <- function(x, name_var = "year") {
  # 先分别计算样本量、均值、SD再合并起来
  funin_aggregate <- function(x, name_calc, name_newvar) {
    xin_output <- aggregate(tarvar ~ city + rec_yr_grp_less,
                            data = x, FUN = name_calc)
    names(xin_output)[3] <- name_newvar
    xin_output
  }
  x_n <- funin_aggregate(x, "length", "n")
  x_mean <- funin_aggregate(x, "mean", "mean")
  x_sd <- funin_aggregate(x, "sd", "sd")
  x_output <- Reduce(merge, list(x_n, x_mean, x_sd))

  # 计算SE
  x_output$se <- x_output$sd / sqrt(x_output$n)

  # 统计检测两个年份差异是否显著
  x_aov <- vector("logical")
  for (i in unique(x$city)) {
    x_city <- subset(x, city == i)
    x_aov <-
      c(x_aov,
        summary(
          aov(formula = x_city$tarvar ~
                x_city[, name_var]))[[1]]$`Pr(>F)`[1] < 0.05)
  }
  x_aov <- data.frame(city = unique(x$city), aov = x_aov)

  # 合并统计结果到输出数据框
  x_output <- merge(x_output, x_aov, all.x = TRUE)
  x_output$aov_mark <- NA
  x_output$aov_mark[x_output$aov] <- "*"

  # 只留下每个城市两个年份中均值+SE之和较大一行对应的统计检验结果以便作图
  x_output$sum_mean_se <- x_output$mean + x_output$se
  x_output <- x_output[order(x_output$city, x_output$sum_mean_se), ]
  delete_aov_mark <- c(1:nrow(x_output))[as.logical(1:nrow(x_output) %% 2)]
  x_output$aov_mark[delete_aov_mark] <- NA

  return(x_output)
}
fun_meanse_calc(x = user_yrdata_ls[[1]], name_var = "rec_yr_grp_less")
user_yrdata_ls_smry <-
  lapply(user_yrdata_ls, fun_meanse_calc, name_var = "rec_yr_grp_less")

plot_ls <- lapply(
  user_yrdata_ls_smry,
  function(y) {
    ggplot(y, aes(city, mean, color = factor(rec_yr_grp_less))) +
      geom_point(position = position_dodge(.2)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se,
                        width = 0.2),
                    position = position_dodge(.2)) +
      geom_text(aes(x = city, y = (mean + se)*1.05, label = aov_mark),
                color = "black", size = 5) +
      scale_y_continuous(limits = c(0, max(y$mean + y$se)*1.1)) +
      theme(axis.title = element_blank())
  }
)

for (i in 1:3) {
  plot_ls[[i]] <- plot_ls[[i]] +
    labs(title = paste(testvar[i]))
}

Reduce("/", plot_ls) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# 统计分析分用户组各城市各指标跨年份差异
# 函数：AOV统计分析
# 功能：检测分用户组分城市分指标检测同一指标在不同年份的差异
# 输入：各城市各用户各年份各项指标数据框
fun_aov <- function(x, testvar) {
  # 仅保留2019-2020年的数据
  x <- subset(x, year %in% c(2019, 2020))
  # 由于第3用户组出现样本不足的情况，将其和第2用户组合并
  x$rec_yr_grp[which(x$rec_yr_grp == 3)] <- 2

  # 按照城市和用户组分成分组列表
  x_ls <- split(x, f = x$city)

  # 内置函数：对某一用户组检验同一指标不同年份是否有差异
  # 输入：按城市分组的各元素为数据框的列表
  for (i in c(1, 2)) {
    # 建立空列表以存储结果
    x_output <- vector("list", 3)
    names(x_output) <- c("obs", "act_days", "obs_pd")

    for (j in testvar) {
      aov_ls <- lapply(x_ls, function(y){
        y <- subset(y, rec_yr_grp == i)
        summary(aov(
          formula = y[, j] ~ y$year))[[1]]$`Pr(>F)`[1] < 0.05
      })
      x_output[[j]] <- Reduce(rbind, aov_ls)
    }

    # 将结果列表合并为数据框
    x_output <- Reduce(cbind, x_output)
    x_output <- as.data.frame(x_output)
    x_output <- cbind(names(x_ls), x_output)
    names(x_output) <- c("city", testvar)
    rownames(x_output) <- NULL
    print(x_output)

    # 结果可视化
    print(ggplot(melt(x_output, id = "city")) +
            geom_tile(aes(x = city, y = variable, fill = value), alpha = 0.6) +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(title = paste("grp:", i)))
  }
}

fun_aov(user_yrdata, testvar = c("obs", "act_days", "obs_pd"))

# 通过均值误差图可视化
# 函数：对用户年度数据按用户组分成几个数据框
# 输入：各城市各用户各年份多指标数据框
# 输出：按用户组分元素数据框所组成的列表
fun_pagrp_split <- function(x) {
  # 仅保留2019和2020年的数据
  x <- subset(x, year %in% c("2019", "2020"))
  x_ls <- split(x, x$rec_yr_grp)
}

user_yrdata_1920 <- subset(user_yrdata, year %in% c("2019", "2020"))

user_yrdata_1920_ls <- vector("list", 6)

testvar <- data.frame(
  rec_yr_grp_less = rep(c(1, 2), 3),
  tarvar = rep(c("obs", "act_days", "obs_pd"), each = 2)
)

for (i in 1:6) {
  user_yrdata_1920_ls[[i]] <-
    subset(user_yrdata_1920, rec_yr_grp_less == testvar$rec_yr_grp_less[i],
           select = c("city", "user", "year", "rec_yr_grp_less",
                      as.character(testvar$tarvar[i])))
  names(user_yrdata_1920_ls[[i]]) <-
    c("city", "user", "year", "rec_yr_grp_less", "tarvar")
}


# 函数：分组计算输入数据的样本量、均值、SD、SE和统计检验结果等
# 输入：2019和2020年某个用户组数据框，需要进行计算的指标名称
# 输出：各城市统计整理后的数据框
fun_meanse_calc <- function(x, name_var = "year") {
  # 先分别计算样本量、均值、SD再合并起来
  funin_aggregate <- function(x, name_calc, name_newvar) {
    xin_output <- aggregate(tarvar ~ city + year, data = x, FUN = name_calc)
    names(xin_output)[3] <- name_newvar
    xin_output
  }
  x_n <- funin_aggregate(x, "length", "n")
  x_mean <- funin_aggregate(x, "mean", "mean")
  x_sd <- funin_aggregate(x, "sd", "sd")
  x_output <- Reduce(merge, list(x_n, x_mean, x_sd))

  # 计算SE
  x_output$se <- x_output$sd / sqrt(x_output$n)

  # 统计检测两个年份差异是否显著
  x_aov <- vector("logical")
  for (i in unique(x$city)) {
    x_city <- subset(x, city == i)
    x_aov <-
      c(x_aov,
        summary(
          aov(formula = x_city$tarvar ~
                x_city[, name_var]))[[1]]$`Pr(>F)`[1] < 0.05)
  }
  x_aov <- data.frame(city = unique(x$city), aov = x_aov)

  # 合并统计结果到输出数据框
  x_output <- merge(x_output, x_aov, all.x = TRUE)
  x_output$aov_mark <- NA
  x_output$aov_mark[x_output$aov] <- "*"

  # 只留下每个城市两个年份中均值+SE之和较大一行对应的统计检验结果以便作图
  x_output$sum_mean_se <- x_output$mean + x_output$se
  x_output <- x_output[order(x_output$city, x_output$sum_mean_se), ]
  delete_aov_mark <- c(1:nrow(x_output))[as.logical(1:nrow(x_output) %% 2)]
  x_output$aov_mark[delete_aov_mark] <- NA

  return(x_output)
}

user_yrdata_1920_ls_smry <- lapply(user_yrdata_1920_ls, fun_meanse_calc)

plot_ls <- lapply(
  user_yrdata_1920_ls_smry,
  function(y) {
    ggplot(y, aes(city, mean, color = year)) +
      geom_point(position = position_dodge(.2)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se,
                        width = 0.2),
                    position = position_dodge(.2)) +
      geom_text(aes(x = city, y = (mean + se)*1.05, label = aov_mark),
                color = "black", size = 5) +
      scale_color_manual(values = c("2019" = "#00AF64", "2020" = "#bf5930")) +
      scale_y_continuous(limits = c(0, max(y$mean + y$se)*1.1)) +
      theme(axis.title = element_blank())
  }
)

for (i in 1:6) {
  plot_ls[[i]] <- plot_ls[[i]] +
    labs(title = paste("Group", testvar$rec_yr_grp_less[i], testvar$tarvar[i]))
}

png(filename = "分用户组同指标不同年份对比.png", res = 300,
    width = 2000, height = 3500)
Reduce("/", plot_ls) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()

