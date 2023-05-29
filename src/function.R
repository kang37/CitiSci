# Function ----
# 函数：读取公民科学各条记录文件合成一个列表，列表各元素分别为各个城市的数据
# 参数：
# file.dir：文件路径
GetRaw <- function(file.dir) {
  city.name <- strsplit(file.dir, "/") %>%
    .[[1]] %>%
    .[3] %>%
    gsub(".csv", "", .)
  read.csv(file.dir) %>%
    tibble() %>%
    # 生成年月日数据
    rename(obs_date = observed_on) %>%
    mutate(
      obs_date = as_date(obs_date),
      city = city.name,
      year = year(obs_date),
      month = month(obs_date),
      day = day(obs_date)
    ) %>%
    # 由于有些城市只有2016年及之后的数据，所以就保留共同年份的数据
    subset(year > 2015) %>%
    mutate(year = as.factor(year)) %>%
    select(city, id, user_id, obs_date, year) %>%
    return()
}

# 函数：对比各城市2019和2020年的数据并可视化
# 参数：
# x：各城市各年份各项指标数值数据框
# yr.base：基准年
# yr.tar：对比年份
CompTwoYr <- function(x, yr.base, yr.tar) {
  # 转化数据格式
  x$yr_abbr <- as.numeric(as.character(x$yr_abbr))

  # 将数据分成基准年和比较年两部分并且按照城市排序
  x1 <- subset(x, yr_abbr == yr.base) %>%
    mutate(city = factor(city, levels = kCity)) %>%
    arrange(city)
  x2 <- subset(x, yr_abbr == yr.tar) %>%
    mutate(city = factor(city, levels = kCity)) %>%
    arrange(city)

  # 计算各项数值的差异
  # 如果比较年比基准年高则判断为TRUE
  cbind(city = as.character(x1$city),
        ifelse(x2[names(x2)[!names(x2) %in% c("city", "year", "yr_abbr")]] /
                 x1[names(x1)[!names(x1) %in% c("city", "year", "yr_abbr")]] > 1,
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

  # 根据城市排序
  x_output <- x_output %>%
    mutate(city = factor(city, levels = kCity)) %>%
    arrange(city)

  return(x_output)
}

# 函数：做带误差棒点图，并显示组间对比结果，目前用于用户分析
# 参数：
# x：包含各城市分组平均值、误差值等的数据
# 漏洞：如何解决图中横轴标签按照城市排序的问题？
PlotBarError <- function(x, name.grp = "obsr_grp",
                         name.yaxis = NULL, name.title = NULL) {
  ggplot(data = x, aes(paste0(city, aov_mark), mean, fill = factor(get(name.grp)))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.2),
                  position = position_dodge(0.9)) +
    labs(y = name.yaxis, title = name.title) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# 函数：做带误差棒的条形图，对比新老用户的表现差异
# 参数：
# x：带年份、组别、指标的各城市用户数据
# name.yr：目标年份
# user.grp：目标用户组别
# name.var：目标指标
# name.yaxis：Y轴标题
# name.title：图片标题
PlotCompObsr <- function(x, name.var, name.title, ...) {
  # 横轴按照城市排序所需因子水平数据
  x.axis.lab <- MeanSeAov(x, name.var = name.var) %>%
    select(city, aov_mark) %>%
    unique()

  MeanSeAov(x, name.var = name.var) %>%
    PlotBarError(name.title = name.title, ...) +
    scale_fill_manual(name = "User group", values = c("#FFA500", "#1047A9")) +
    # 横轴标签按照城市进行排序
    scale_x_discrete(limits = paste0(x.axis.lab$city, x.axis.lab$aov_mark))
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
  # 横轴按照城市排序所需因子水平数据
  x.axis.lab <- subset(x, year %in% name.yr & obsr_grp == user.grp) %>%
    MeanSeAov(., name.grp = "year", name.var = name.var) %>%
    select(city, aov_mark) %>%
    unique()

  # 保留目标年老用户数据
  subset(x, year %in% name.yr & obsr_grp == user.grp) %>%
    # 计算平均值、误差、显著性等
    MeanSeAov(., name.grp = "year", name.var = name.var) %>%
    PlotBarError(name.grp = "year",
                 name.yaxis = name.yaxis, name.title = name.title) +
    scale_fill_manual(name = "Year",
                      values = c("#009999", "#FFCE00", "#FF0000")) +
    # 横轴标签按照城市进行排序
    scale_x_discrete(limits = paste0(x.axis.lab$city, x.axis.lab$aov_mark))
}
