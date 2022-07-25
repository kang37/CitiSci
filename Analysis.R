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

# 函数：输出各用户各年份观测条数、观测天数及每观测天观测条数，该数据主要用于用户分析
# 参数：
# x：带有城市、用户、观测条数等的原始数据
SmryUserData <- function(x) {
  x$year <- factor(x$year)
  x$day <- factor(x$day)

  x_output <- x %>%
    group_by(year, user_id) %>%
    summarise(obs = n(),
              act_days = n_distinct(day),
              obs_pd = obs/act_days) %>%
    ungroup() %>%
    rename(user = user_id)

  return(x_output)
}

# 函数：分组计算输入数据的样本量、均值、SD、SE和统计检验结果等，用于用户分析
# 参数：
# x：各城市各用户带分组信息的数据
# name.var：要分析的指标
# 输出：各城市统计整理后的数据框
MeanSeAov <- function(x, name.grp = "obsr_grp", name.var = "obs") {
  # 对各城市分组计算样本量、均值、SD
  x_output <- x %>%
    group_by(city, get(name.grp)) %>%
    summarise(
      n = n(),
      mean = mean(get(name.var)),
      sd = sd(get(name.var)),
      se = sd/n
    ) %>%
    ungroup()
  names(x_output)[which(names(x_output) == "get(name.grp)")] <- name.grp

  # 对各个城市统计对比不同分组之间的差异
  x_aov <- vector("logical")
  for (i in unique(x$city)) {
    x_city <- subset(x, city == i)
    x_aov <-
      c(x_aov,
        summary(
          aov(formula = x_city[[name.var]] ~
                x_city[[name.grp]]))[[1]]$"Pr(>F)"[1] < 0.05)
  }
  x_aov <- data.frame(city = unique(x$city), aov = x_aov)

  # 合并统计结果到输出数据框
  x_output <- merge(x_output, x_aov, all.x = TRUE)
  x_output$aov_mark <- "   "
  x_output$aov_mark[x_output$aov] <- " * "
  # 将显著性标记加入城市名称
  x_output$city <- paste0(x_output$city, x_output$aov_mark)

  return(x_output)
}

# 函数：做带误差棒点图，并显示组间对比结果，目前用于用户分析
# 参数：
# x：包含各城市分组平均值、误差值等的数据
PlotBarError <- function(x, name.grp = "obsr_grp",
                         name.yaxis = NULL, name.title = NULL) {
  ggplot(x, aes(city, mean, fill = factor(get(name.grp)))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.2),
                  position = position_dodge(0.9)) +
    labs(y = name.yaxis, title = name.title) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}


# 函数：从用户数据中筛除目标年份和用户组别，做带误差棒的条形图，对比新冠期间和此前的差别
# 参数：
# x：带年份、组别、指标的各城市用户数据
# name.yr：目标年份
# user.grp：目标用户组别
# name.var：目标指标
# name.yaxis：Y轴标题
# name.title：图片标题
PlotCovidYr <- function(x, name.yr = c("2019", "2020", "2021"),
                        user.grp, name.var, name.yaxis, name.title) {
  # 保留目标年老用户数据
  subset(x, year %in% name.yr & obsr_grp == user.grp) %>%
    MeanSeAov(., name.grp = "year", name.var = name.var) %>%
    PlotBarError(name.grp = "year",
                 name.yaxis = name.yaxis, name.title = name.title) +
    scale_fill_manual(name = "Year",
                      values = c("#009999", "#FFCE00", "#FF0000"))
}

# Read data ----
## Constant ----
# 城市：按照人口从多到少排序
kCity <- c("Tokyo", "Yokohama", "Osaka", "Nagoya", "Sapporo", "Fukuoka",
           "Kobe", "Kawasaki", "Kyoto", "Saitama", "Hiroshima")

## Prefectures and cities ----
pref.city <- read.xlsx("RawData/Prefectures_cities.xlsx") %>%
  tibble() %>%
  # 保留本研究的目标城市
  subset(city_en %in% kCity) %>%
  rename(prefecture = prefecture_en, city = city_en) %>%
  # 按照城市人口从多到少排序
  mutate(city = factor(city, levels = kCity))

## iNaturalist data ----
### Raw data ----
# 读取公民科学各条记录文件并合成一个列表，列表的各元素分别为各个城市的数据
record.raw <- GetObs(name.dir = "RawData/iNatData")

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

### User data ----
# 构建用户数据：各个城市各个用户各年份各指标的数值
record.city.obsr.yr <- CityLs2Df(lapply(record.raw, SmryUserData))
# 检测是否有用户跨城市上传数据
dim(unique(record.city.obsr.yr[c("city", "user", "year")]))
dim(unique(record.city.obsr.yr[c("user", "year")]))
# 结论：有1000多名用户跨城市上传数据

# 用户分组：有两种方式，一种是根据用户是第几年参与公民科学来分类，可以分成“第一年参加组”、“第二年参加组”等等，另一种根据2016到2021年间共参与了几年来将观测者区分为新用户和老用户，这种分类方法当然带有点“注定”的意味，也就是说一个老用户，就算他是第一年参加活动，他也是被算成一个“老用户”。此外，因为有些用户跨城市上传观测数据，所以用户分组的时候应该无视城市，比如有个用户第一年年在一个城市上传了数据，次年在另一个城市上传了数据，那么如果按照上述第一种方式来分类，他的“次年”就应该被算作“第二年”，而非“第一年”。下面暂时采用第二种分组方式对用户进行分组。

# 构造用户数据，计算用户2016到2021年间有上传观测数据的年数
user.grp <- record.city.obsr.yr %>%
  select(user, year) %>%
  unique() %>%
  # 计算用户总共上传了几年数据
  group_by(user) %>%
  summarise(n_yr = n()) %>%
  ungroup()
table(user.grp$n_yr)
# 结论：绝大部分用户都是只活跃一年的新用户

# 将用户分组数据加入年度观测数据中
record.city.obsr.yr <- record.city.obsr.yr %>%
  left_join(user.grp, by = "user") %>%
  # 观测总年数大于等于2年都划为老用户
  mutate(obsr_grp = case_when(n_yr <= 1 ~ "short", TRUE ~ "long"))

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
# 结论：各年份中，数据各月份变化并无固定规律。原本还比较了2020年各月份同2019年对应月份之间的差异，但是既然多年来各月份的“基线数据”就没有明显固定的规律，那么这样的同期比较也就没有意义了。因此就把这部分分析删除了。出于相同的原因，也删除了新冠数据和各年月度数据之间的关系分析代码，因为2020年或者2021年各月份的指标数据变化可能是随机因素导致的，而非新观点导致的。

## User group analysis ----
### Metrics ~ user grps ----
# 对比新老用户，看老用户在各项指标上是否都高于新用户
png(filename = "ProcData/分指标各城市跨用户组对比点误差棒图.png", res = 300,
    width = 2000, height = 3500)
MeanSeAov(record.city.obsr.yr, name.var = "obs") %>%
  PlotBarError(name.title = "(a)") /
  MeanSeAov(record.city.obsr.yr, name.var = "act_days") %>%
  PlotBarError(name.title = "(b)") /
  MeanSeAov(record.city.obsr.yr, name.var = "obs_pd") %>%
  PlotBarError(name.title = "(c)") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()
# 盒形图也可以作为参考
png(filename = "ProcData/分指标各城市跨用户组对比盒形图.png", res = 300,
    width = 2000, height = 3500)
(ggplot(record.city.obsr.yr) +
  geom_boxplot(aes(city, obs, color = obsr_grp))) /
  (ggplot(record.city.obsr.yr) +
  geom_boxplot(aes(city, act_days, color = obsr_grp))) /
  (ggplot(record.city.obsr.yr) +
  geom_boxplot(aes(city, obs_pd, color = obsr_grp))) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()
# 结论：老用户观测数通常显著高于新用户，其原因主要是老用户观测天数较多。如果看观测强度，即每观测天观测条数的话，尽管大多数不显著，但是新用户往往甚至高于老用户。可见持之以恒，少量多次才是老用户成功超越新用户的关键。并且注意，这里的“观测天数较多”是指年内观测天数多，而不是因为老用户活动年数多导致的研究时长内总的观测天数多。

### Metrics ~ years ----
# 分用户组各指标不同年份对比
((PlotCovidYr(record.city.obsr.yr, user.grp = "long", name.var = "obs",
              name.yaxis = "Observation", name.title = "(a)") /
    PlotCovidYr(record.city.obsr.yr, user.grp = "short", name.var = "obs",
                name.yaxis = "Observation", name.title = "(b)")) |
   (PlotCovidYr(record.city.obsr.yr, user.grp = "long", name.var = "act_days",
                name.yaxis = "Obs. day", name.title = "(c)") /
      PlotCovidYr(record.city.obsr.yr, user.grp = "short", name.var = "act_days",
                  name.yaxis = "Obs. day", name.title = "(d)")) |
   (PlotCovidYr(record.city.obsr.yr, user.grp = "long", name.var = "obs_pd",
                name.yaxis = "Obs. per day", name.title = "(e)") /
      PlotCovidYr(record.city.obsr.yr, user.grp = "short", name.var = "obs_pd",
                  name.yaxis = "Obs. per day", name.title = "(f)"))) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
