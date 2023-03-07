# 统计分析各月份新冠感染数和政策强度对公民科学各项指标的影响
# bug：其中政策强度计划用每个月的紧急事态天数表示，但目前还没找到数据
# 注意：新冠的实际感染数也受到检测数量和上报数量影响，但是因为我们的研究考虑的是
# 这方面信息如何影响人们出行行为，因此这并构成问题
covid.citisci <- covid_monthly %>%
  left_join(subset(tot_mthdata, year == 2020), by = c("city", "month"))

# 使用绝对值进行分析
# 混合各城市数据进行分析
# bug：由于各城市的人口数不同，这样做对于总量指标显然不合理，但是先看看状况
# 作图查看各个指标和对应县感染数的关系

# 函数：作图查看月度数据各个指标和对应县感染数的关系
# 参数：
# x：包含各指标列和感染人数列的数据框
PlotCovnumMet <- function(
  x, name.covnum = "number",
  name.met = c("obs", "obs_id", "idpa", "users", "act_days",
               "obs_per_user", "actdays_per_user", "obs_pu_pd", "id_rate")) {
  # 划分画布分格
  par(mfrow = c(2, 2))
  for (i in name.met) {
    plot(x[[name.covnum]], x[[i]], main = i)
  }
  # 复原画布分格
  par(mfrow = c(1, 1))
}

# 函数：计算各指数和新冠感染人数的相关性
# 参数：
# x：包含新冠感染人数和各指标列的数据框
CorCovnumMet <- function(x) {
  # 建立列表以存储相关分析结果
  cor_res <- vector("list", length = 3)

  # 各自变量和因变量的相关性作图和计算
  for (i in c("obs", "obs_id", "idpa", "users", "act_days",
              "obs_per_user", "actdays_per_user", "obs_pu_pd", "id_rate")) {
    # 如果样本量大于10则计算，否则直接将结果定为NA
    if(length(na.omit(x[[i]])) > 10) {
      res <- cor.test(x[[i]], x[["number"]])
      cor_res[[1]] <- c(cor_res[[1]], i)
      cor_res[[2]] <- c(cor_res[[2]], res$estimate)
      cor_res[[3]] <- c(cor_res[[3]], res$p.value)
    } else {
      cor_res[[1]] <- c(cor_res[[1]], i)
      cor_res[[2]] <- c(cor_res[[2]], NA)
      cor_res[[3]] <- c(cor_res[[3]], NA)
    }
  }

  # 将结果转化为数据框
  cor_res <- tibble(
    variable = cor_res[[1]],
    correlation = cor_res[[2]],
    p = cor_res[[3]]
  )
  # 生成显著性标记数据列
  cor_res$p_mark <- ""
  cor_res$p_mark[which(cor_res$p < 0.05)] <- "*"
  cor_res$p_mark[which(cor_res$p < 0.01)] <- "**"
  cor_res$p_mark[which(cor_res$p < 0.001)] <- "***"

  cor_res$result <-
    paste0(round(cor_res$correlation, digits = 2), cor_res$p_mark)
  return(cor_res)
}
CorCovnumMet(covid.citisci.scale)

# 查看标准化前的分析结果
PlotCovnumMet(covid.citisci)
CorCovnumMet(covid.citisci)
# 基本结论：
# 可见大部分数值都集中在较低的范围内，难以看出有什么关联
# 统计结果虽然表现出一定的相关性，但从分析过程上说，这个分析过程并不合理，可能是
# 过拟合，而且这些结果的相关系数都非常微弱，可见效用并不大

# 函数：对数值进行标准化
# 参数：
# x：数值型向量
ScaleMinMax <- function(x) {
  output <- (x - min(x)) / (max(x) - min(x))
  return(output)
}

covid.citisci.scale <- covid.citisci %>%
  group_by(city) %>%
  summarise(number = ScaleMinMax(number),
            obs = ScaleMinMax(obs),
            obs_id = ScaleMinMax(obs_id),
            idpa = ScaleMinMax(idpa),
            users = ScaleMinMax(users),
            act_days = ScaleMinMax(act_days),
            obs_per_user = ScaleMinMax(obs_per_user),
            actdays_per_user = ScaleMinMax(actdays_per_user),
            obs_pu_pd = ScaleMinMax(obs_pu_pd),
            id_rate = ScaleMinMax(id_rate)
  ) %>%
  ungroup()

# 对标准化后的数据作图以及进行统计分析
PlotCovnumMet(covid.citisci.scale)
CorCovnumMet(covid.citisci.scale)
