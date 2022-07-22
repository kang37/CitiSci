# Statement ----
# 用于分析新冠对公民科学行为的影响，主要思路：从年度和月度粒度上分析，然后分用户组进行分析。

# Package ----
library(openxlsx)
library(dplyr)
library(ggplot2)
library(patchwork)
library(reshape2)
library(lubridate)

# Setting ----
sPfig <- TRUE

# Function ----
# 函数：按年或月汇总计算记录数、活跃用户数、活跃天数等数据
# 参数：
# x：含日期、user_id等信息的每条观测原始数据
# dur：按照什么粒度进行汇总，“month”或“year”
SmryData <- function(x, dur = "month") {
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

# 函数：将多城市数据合并为一个数据框
CityLs2Df <- function(x) {
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
SerPlot <- function(x, var_ls, plotname, dur = "month") {
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
        scale_color_manual(
          values = c("2019" = "#00AF64", "2020" = "#bf5930", "2021" = "blue")) +
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

# 函数：对比各城市2019和2020年的数据并可视化
# 参数：
# x：各城市各年份各项指标数值数据框
# yr.base：基准年
# yr.tar：对比年份
CompTwoYr <- function(x, yr.base, yr.tar) {
  # 将数据分成基准年和比较年两部分并且按照城市排序
  x1 <- subset(x, year == yr.base) %>%
    mutate(city = factor(city, levels = kCity)) %>%
    arrange(city)
  x2 <- subset(x, year == yr.tar) %>%
    mutate(city = factor(city, levels = kCity)) %>%
    arrange(city)

  # 计算各项数值的差异
  # 如果比较年比基准年高则判断为TRUE
  cbind(city = as.character(x1$city),
        ifelse(x2[names(x2)[!names(x2) %in% c("city", "year", "yr_sht")]] /
                 x1[names(x1)[!names(x1) %in% c("city", "year", "yr_sht")]] > 1,
               paste0(yr.tar, " > ", yr.base),
               paste0(yr.tar, " <= ", yr.base))) %>%
    as.data.frame() %>%
    # 转化成长数据并作图
    melt(data = ., id = "city") %>%
    mutate(city = factor(city, levels = kCity)) %>%
    ggplot() +
    geom_tile(aes(x = city, y = variable, fill = value), alpha = 0.5) +
    theme(axis.text.x = element_text(angle = 90))
}

# 函数：计算2020年每月相比上年相同月份的变化率
# 输入：各城市各年月的各项指标数据
# 输出：各城市各年月的各项指标相比去年同期的变化率
MthChg1920 <- function(x) {
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
SerPlotMthChg1920 <- function(x) {
  x_lng <- melt(x, id = c("city", "month"))
  # 可视化各月数据变化
  p1 <- ggplot(x_lng) +
    geom_line(aes(month, value)) +
    facet_grid(variable ~ city, scales = "free")
  print(p1)
  # 2019-2020各指标变化率与新冠相关性图
  x_lng <- merge(x_lng, covid.mth,
                 by.x = c("city", "month"), by.y = c("city_en", "month"))
  p2 <- ggplot(x_lng) +
    geom_point(aes(number, value), alpha = 0.4) +
    facet_grid(variable ~ city, scales = "free")
  print(p2)
}

# 函数：检验各城市2020年各月不同指标数据和对应新冠感染数的相关关系
# 输入：各城市2020年各月不同指标数据数据框，各城市2020各月新冠感染数数据框
# 输出：各城市2020年各月不同指标和新冠感染数相关关系的p值数据
CorCovid <- function(x, testvar, covid_df) {
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

# 函数：2019-2020年各项指标环比数据
# 输入：各城市各年月的各项指标数据
# 输出：各城市各年月的各项指标相比去年同期的变化率
SeqChg1920 <- function(x) {
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

# 函数：读取公民科学各条记录文件合成一个列表，列表各元素分别为各个城市的数据
# 参数：
# name.dir：路径名称
GetObs <- function(name.dir) {
  # Get all *.csv file names in the file
  filenames <- list.files(name.dir) %>%
    grep(".csv", x = ., value = TRUE)

  # a new list to store raw data
  record <- vector("list", length = length(filenames))
  names(record) <- gsub(".csv", "", filenames)

  for (i in 1:length(filenames)) {
    record[[i]] <- read.csv(paste0(name.dir, "/", filenames[i])) %>%
      tibble() %>%
      # 生成年月日数据
      mutate(observed_on = as.Date(observed_on),
             year = year(observed_on),
             month = month(observed_on),
             day = day(observed_on)) %>%
      # 由于有些城市只有2016年及之后的数据，所以就保留共同年份的数据
      subset(year > 2015)
  }

  return(record)
}

# Read data ----
## Constant ----
# 城市：按照人口从多到少排序
kCity <- c("Tokyo", "Yokohama", "Osaka", "Nagoya", "Sapporo", "Fukuoka",
           "Kobe", "Kawasaki", "Kyoto", "Saitama", "Hiroshima")

## Prefectures and cities ----
### Raw data ----
pref.city <- read.xlsx("RawData/Prefectures_cities.xlsx") %>%
  tibble() %>%
  # 保留本研究的目标城市
  subset(city_en %in% kCity) %>%
  rename(prefecture = prefecture_en, city = city_en) %>%
  # 按照城市人口从多到少排序
  mutate(city = factor(city, levels = kCity))

### Yearly data ----
# 各城市各年份观测数等数据
record.city.yr <- lapply(record.raw, SmryData, dur = "year") %>%
  CityLs2Df() %>%
  tibble() %>%
  # 生成年份缩写列用于作图
  mutate(yr_sht = year - 2000)

### Monthly data ----
# 构建各年份月度数据
record.city.mth <- lapply(record.raw, SmryData, dur = "month") %>%
  CityLs2Df() %>%
  tibble()

## Raw iNaturalist data ----
# 读取公民科学各条记录文件并合成一个列表，列表的各元素分别为各个城市的数据
record.raw <- GetObs(name.dir = "RawData/iNatData")

## COVID-19 data ----
# bug：之后删除结论
# 基本结论：
# 基本上从二月~四月开始受影响
# 结合十月份的数据，可见不仅受政策，也受到实际疫情数据的影响

# 读取数据：日期，县，各感染者数量
# 此处把县和城市信息加进去后可能会引起误解，误以为感染者人数是各城市的感染者人数，但实际上感染者人数是所在县的感染者人数-人受信息的影响可能存在尺度效应
covid.mth <-
  read.csv("RawData/nhk_news_covid19_prefectures_daily_data.csv") %>%
  rename(date = "日付", prefecture_jp = "都道府県名",
         pref_inf = "各地の感染者数_1日ごとの発表数") %>%
  as_tibble() %>%
  select(date, prefecture_jp, pref_inf) %>%
  # 筛选出目标县
  subset(prefecture_jp %in% pref.city$prefecture_jp) %>%
  # 加入县英文名和城市名信息
  left_join(pref.city, by = "prefecture_jp") %>%
  # 更改数据类型
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date), month = month(date)) %>%
  # 保留2020年的数据
  subset(year == 2020) %>%
  group_by(prefecture, city, month) %>%
  summarise(pref_inf = sum(pref_inf), .groups = "drop_last") %>%
  ungroup() %>%
  # bug：权宜之计：按照从北到南对城市进行排序
  mutate(prefecture = factor(prefecture, levels = kCity))

# Analysis ----
## Annual comparison ----
# 作图：各城市各指标历年变化
png(filename = "ProcData/历年各项指标变化.png", res = 300,
    width = 3000, height = 4500)
(SerPlot(record.city.yr,
         var_ls =
           c(
             "obs", "users", "act_days",
             "obs_per_user",
             "actdays_per_user",
             "obs_pu_pd",
             "idpa", "id_rate"
           ),
         plotname =
           c(
             "(a) Observation", "(b) Participant", "(c) Active day",
             "(d) Observations per participant",
             "(e) Active days per participant",
             "(f) Observations per participant per active day",
             "(g) Identification participant", "(h) Identification rate"
           ),
         dur = "year") %>%
    Reduce("/", x = .) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom"))
dev.off()

# 作图：各年份各指标相比前一年的变化
png(filename = "ProcData/各指标年份两两间比较.png", res = 300,
    width = 2000, height = 4500)
(
  CompTwoYr(record.city.yr, yr.base = 2016, yr.tar = 2017) /
    CompTwoYr(record.city.yr, yr.base = 2017, yr.tar = 2018) /
    CompTwoYr(record.city.yr, yr.base = 2018, yr.tar = 2019) /
    CompTwoYr(record.city.yr, yr.base = 2019, yr.tar = 2020) /
    CompTwoYr(record.city.yr, yr.base = 2020, yr.tar = 2021)
)
dev.off()
# 结论：2020年之前上升的主要是下面的指标，而2020年及之后上升的主要是上面的指标，意味着虽然总观测数、总用户数、总活跃天数等可能减少了，但是新冠期间的用户比此前更加活跃

# 室内参与和室外参与的关系
# 作图：检测观测条数和鉴定条数的关系
plot(log(record.city.yr$obs), log(record.city.yr$idpa))
# 分城市来看
ggplot(record.city.yr) +
  geom_point(aes(obs, idpa, color = factor(year))) +
  facet_wrap(.~ city, scales = "free")
# 结论：两者几乎就是线性相关的，可以推测鉴定条数增加主要由观测条数驱动，一定程度上，和“室外活动减少，室内活动增加”的结论形成对比。当然不排除一种可能性，即说不定室内活动减少得幅度较低，而室外活动受到影响更大，减少得较多。这种可能性也可以检测，但因为这些变化受多种因素影响，就算检测了，结果也未必可靠，因此暂时不检测。

## Monthly comparison ----
### General comparison of 2016-2021 ----
# 作图：历年各月份是否展现出什么类似的趋势
png(filename = "ProcData/历年各月各项指标变化.png", res = 300,
    width = 3000, height = 4500)
SerPlot(
  record.city.mth,
  var_ls =
    c("obs", "users", "act_days",
      "obs_per_user",
      "actdays_per_user",
      "obs_pu_pd",
      "idpa", "id_rate"),
  plotname =
    c("(a) Observation", "(b) Participant", "(c) Active day",
      "(d) Observations per participant",
      "(e) Active days per participant",
      "(f) Observations per participant per active day",
      "(g) Identification participant", "(h) Identification rate")
) %>%
  Reduce("/", x = .) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()
# 结论：各年份中，数据各月份变化并无固定规律。原本还比较了2020年各月份同2019年对应月份之间的差异，但是既然多年来各月份的“基线数据”就没有明显固定的规律，那么这样的同期比较也就没有意义了。因此就把这部分分析删除了。

### Metrics ~ COVID data ----
# 2019-2020每月同比变化
# 每月各项变化
# bug：这部分无法运行，但是似乎也没有必要
record.city.mth_chg_1920 <- MthChg1920(record.city.mth)
SerPlotMthChg1920(record.city.mth_chg_1920)
CorCovid(x = record.city.mth_chg_1920,
             testvar = c("obs", "users", "act_days", "obs_per_user",
                         "actdays_per_user", "obs_pu_pd",
                         "idpa", "id_rate"),
             covid_df = covid.mth)

### Seq and seq-change ~ COVID ----
# 2019-2020年各项指标环比月度变化作图
# bug：环比的意义又在哪里呢？需要进一步理清楚
SeqChg1920(record.city.mth) %>%
  melt(data = ., id = c("city", "year", "month")) %>%
  ggplot() +
  geom_line(aes(month, value, color = factor(year))) +
  facet_grid(variable ~ city, scales = "free")

# 2020年相比前一年环比的变化率和新冠的关系
# bug：这部分无法运行，同时也要反思下这部分分析是否有必要
record.city.mth_seqchg_chg_1920 <- MthChg1920(record.city.mth_seqchg_1920)
SerPlotMthChg1920(record.city.mth_seqchg_chg_1920)
CorCovid(x = record.city.mth_seqchg_chg_1920,
             testvar = c("obs", "users", "act_days", "obs_per_user",
                         "actdays_per_user", "obs_pu_pd",
                         "idpa", "id_rate"),
             covid_df = covid.mth)

## User group analysis ----
# 对参与者进行分组分析，看活跃用户和其他用户在疫情期间的表现有何差异

### Data construction ----
# 函数：输出各用户各年份观测数、活跃天数及日均观测数
# bug：重名函数
SmryData <- function(x) {
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

user_yrdata <- CityLs2Df(lapply(record.raw, SmryData))

# 分组：根据用户是第几年参与进行分组-第1年，第2年，第3年及以上
user_yrdata$rec_yr <-
  ave(as.numeric(user_yrdata$year),
      list(user_yrdata$user, user_yrdata$city), FUN = seq_along)
user_yrdata$rec_yr_grp <-
  user_yrdata$rec_yr
user_yrdata$rec_yr_grp[which(user_yrdata$rec_yr >= 3)] <- 3

# 另一种分组：根据用户是第几年参与进行分组-第1年，第2年，第3年及以上
user_yrdata$rec_yr_grp_less <- user_yrdata$rec_yr_grp
user_yrdata$rec_yr_grp_less[which(user_yrdata$rec_yr_grp >= 2)] <- 2

### Fac: metrics + city; metric ~ user grps ----
# 目标：证明历年老用户均比新用户更活跃
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

png(filename = "ProcData/分指标各城市跨用户组对比.png", res = 300,
    width = 2000, height = 3500)
Reduce("/", plot_ls) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()

### Fac: metrics + user grp; metric ~ year 19-20 ----
# 目标：检验新冠对新用户还是老用户影响更大
# 通过均值误差图可视化分用户组分指标各城市跨2019-2020年对比
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

png(filename = "ProcData/分用户组同指标不同年份对比.png", res = 300,
    width = 2000, height = 3500)
Reduce("/", plot_ls) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()
if (sPfig) {
  source("Figure_metric_year_group.R")
}

