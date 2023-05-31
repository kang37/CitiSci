# Statement ----
# 用于分析新冠对公民科学行为的影响，主要思路：从年度和月度粒度上分析，然后分用户组进行分析。

# Package ----
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggh4x)
library(patchwork)
library(reshape2)
library(lubridate)

# Function ----
source("src/function.R")

# Read data ----
## Constant ----
# 城市：按照人口从多到少排序
kCity <- c("Tokyo", "Yokohama", "Osaka", "Nagoya", "Sapporo", "Fukuoka",
           "Kobe", "Kawasaki", "Kyoto", "Saitama", "Hiroshima")

## iNaturalist data ----
### Raw data ----
# 文件路径
inat.file <- list.files("data_raw/iNatData", full.names = TRUE)

# middle data of raw data
record.raw <- lapply(inat.file, GetRaw) %>%
  do.call(rbind, .)

# Identify the "super user" - it should be noted that a "super user" is city- and year-specific, for example, a super user in one city for a certain year might not be a super user in another city.
super.user <- record.raw %>%
  # criterion 1: high proportion of contribution
  group_by(city, year, user_id) %>%
  summarise(obs = n()) %>%
  ungroup() %>%
  group_by(city, year) %>%
  mutate(obs_prop = obs / sum(obs)) %>%
  ungroup() %>%
  # criterion 2: extremly higher observation than the others
  group_by(city, year) %>%
  mutate(obs_99 = quantile(obs, 0.99)) %>%
  ungroup() %>%
  mutate(super_user = case_when(
    obs_prop > 0.3 | obs > obs_99 ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  select(city, year, user_id, super_user)

# 用户分组：有两种方式，一种是根据用户是第几年参与公民科学来分类，可以分成“第一年参加组”、“第二年参加组”等等，另一种根据2016到2021年间共参与了几年来将观测者区分为新用户和老用户，这种分类方法当然带有点“注定”的意味，也就是说一个老用户，就算他是第一年参加活动，他也是被算成一个“老用户”。此外，因为有些用户跨城市上传观测数据，所以用户分组的时候应该无视城市，比如有个用户第一年年在一个城市上传了数据，次年在另一个城市上传了数据，那么如果按照上述第一种方式来分类，他的“次年”就应该被算作“第二年”，而非“第一年”。下面暂时采用第二种分组方式对用户进行分组。

# 构造用户数据，计算用户2016到2021年间有上传观测数据的年数
user.grp <- record.raw %>%
  select(user_id, year) %>%
  distinct() %>%
  # 计算用户总共上传了几年数据
  group_by(user_id) %>%
  summarise(n_yr = n()) %>%
  ungroup() %>%
  # 观测总年数大于等于2年都划为老用户
  mutate(obsr_grp = case_when(n_yr <= 1 ~ "short", TRUE ~ "long"))
table(user.grp$n_yr)
# 结论：绝大部分用户都是只活跃一年的短期用户

# target city: cities with max year user > 50
tar.city <- record.raw %>%
  group_by(city, year) %>%
  summarise(user_pop = length(unique(user_id))) %>%
  ungroup() %>%
  group_by(city) %>%
  summarise(max_user_pop = max(user_pop)) %>%
  ungroup() %>%
  filter(max_user_pop > 50) %>%
  mutate(city = factor(city, levels = kCity))

# remove super users and non-target cities
record.raw <- record.raw %>%
  filter(city %in% tar.city$city) %>%
  left_join(super.user, c("city", "user_id", "year")) %>%
  filter(!super_user) %>%
  select(city, id, user_id, obs_date, year)

### Yearly user data ----
# data of each city-year-user
record.user.yr <- record.raw %>%
  group_by(city, year, user_id) %>%
  summarise(
    obs = n(),
    act_day = length(unique(obs_date)),
    obs_per_day = obs / act_day
  ) %>%
  ungroup() %>%
  left_join(user.grp, by = "user_id") %>%
  # 生成年份缩写列用于作图
  mutate(yr_abbr = as.factor(as.numeric(as.character(year)) - 2000))

### Yearly data ----
# data of each city-year
record.yr <- record.user.yr %>%
  group_by(city, yr_abbr) %>%
  summarise(
    obs = sum(obs),
    user_pop = n(),
    act_day = sum(act_day),
    prop_long_user = sum(obsr_grp == "long") / user_pop
  ) %>%
  ungroup() %>%
  mutate(
    city = factor(city, levels = kCity),
    day_per_user = act_day / user_pop,
    obs_per_day = obs / act_day
  )

# Analysis ----
## Observation change ----
# scaled observation change
record.yr %>%
  select(city, yr_abbr, obs) %>%
  group_by(city) %>%
  mutate(obs_scale = obs / max(obs)) %>%
  ggplot() +
  geom_line(aes(as.numeric(as.character(yr_abbr)), obs_scale, col = city))

# panel observation change plot
png(filename = "data_proc/obs_chg.png", res = 350,
    width = 3600, height = 800)
(
  record.yr %>%
    ggplot() +
    geom_line(
      aes(as.numeric(as.character(yr_abbr)), obs)
    ) +
    geom_vline(xintercept = 19, col = "red", alpha = 0.5) +
    facet_wrap(.~ city, scale = "free", nrow = 1) +
    expand_limits(y = 0) +
    labs(x = "Year", y = "Number of observations")
)
dev.off()

## User group ----
### Metrics ~ user grps ----
# 对比新老用户，看老用户在各项指标上是否都高于新用户
png(filename = "data_proc/User_behavior_comparison.png", res = 300,
    width = 2000, height = 1000)
(
  PlotCompObsr(record.user.yr, name.var = "obs",
               name.yaxis = "Observation", name.title = "(a)") +
    PlotCompObsr(record.user.yr, name.var = "act_day",
                 name.yaxis = "Active day", name.title = "(b)") +
    PlotCompObsr(record.user.yr, name.var = "obs_per_day",
                 name.yaxis = "Daily observation", name.title = "(c)") +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")
)
dev.off()
# 结论：老用户观测数通常显著高于新用户，其原因主要是老用户观测天数较多。如果看观测强度，即每观测天观测条数的话，尽管大多数不显著，但是新用户往往甚至高于老用户。可见持之以恒，少量多次才是老用户成功超越新用户的关键。并且注意，这里的“观测天数较多”是指年内观测天数多，而不是因为老用户活动年数多导致的研究时长内总的观测天数多。

### Metrics ~ years ----
# 分用户组各指标不同年份对比
((PlotCovidYr(record.user.yr, user.grp = "long", name.var = "obs",
              name.yaxis = "Observation", name.title = "(a)") /
    PlotCovidYr(record.user.yr, user.grp = "short", name.var = "obs",
                name.yaxis = "Observation", name.title = "(d)")) |
   (PlotCovidYr(record.user.yr, user.grp = "long", name.var = "act_day",
                name.yaxis = "Active day", name.title = "(b)") /
      PlotCovidYr(record.user.yr, user.grp = "short", name.var = "act_day",
                  name.yaxis = "Active day", name.title = "(e)")) |
   (PlotCovidYr(record.user.yr, user.grp = "long", name.var = "obs_per_day",
                name.yaxis = "Daily observation", name.title = "(c)") /
      PlotCovidYr(record.user.yr, user.grp = "short", name.var = "obs_per_day",
                  name.yaxis = "Daily observation", name.title = "(f)"))) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
# 结论：新冠对长短期用户的影响不同

## Factor mean value change ----
png(filename = "data_proc/metric_change_for_each_city.png", res = 300,
    width = 3500, height = 2000)
(
  record.yr %>%
    select(city, yr_abbr, user_pop, prop_long_user, day_per_user, obs_per_day) %>%
    pivot_longer(cols = c(user_pop, prop_long_user, day_per_user, obs_per_day),
                 names_to = "metric", values_to = "metric_val") %>%
    mutate(metric = case_when(
      metric == "user_pop" ~ "Population",
      metric == "prop_long_user" ~ "Structure",
      metric == "day_per_user" ~ "Frequency",
      metric == "obs_per_day" ~ "Intensity"
    )) %>%
    mutate(metric = factor(
      metric, levels = c("Population", "Structure", "Frequency", "Intensity")
    )) %>%
    ggplot() +
    geom_line(
      aes(as.numeric(as.character(yr_abbr)), metric_val)
    ) +
    geom_vline(xintercept = 19, col = "red", alpha = 0.5) +
    expand_limits(y = 0) +
    facet_grid2(vars(metric), vars(city), scales = "free", independent = "y") +
    labs(x = "Year", y = "Metric")
)
dev.off()

# 作图：各年份各指标相比前一年的变化
CompTwoYr(record.yr, yr.base = 16, yr.tar = 17) /
  CompTwoYr(record.yr, yr.base = 17, yr.tar = 18) /
  CompTwoYr(record.yr, yr.base = 18, yr.tar = 19) /
  CompTwoYr(record.yr, yr.base = 19, yr.tar = 20) /
  CompTwoYr(record.yr, yr.base = 20, yr.tar = 21)
# 结论：2020年之前上升的主要是下面的指标，而2020年及之后上升的主要是上面的指标，意味着虽然总观测数、总用户数、总活跃天数等可能减少了，但是新冠期间的用户比此前更加活跃

## LMDI ----
lmdi <- record.user.yr %>%
  group_by(city, year, obsr_grp) %>%
  summarise(o_i = sum(obs), p_i = n(), d_i = sum(act_day)) %>%
  arrange(city, year, obsr_grp)
# check data
table(lmdi$city, lmdi$year, lmdi$obsr_grp)

lmdi <- inner_join(
  lmdi %>%
    rename(year_0 = year, o_i0 = o_i, p_i0 = p_i, d_i0 = d_i) %>%
    mutate(year_t = as.factor(as.numeric(as.character(year_0)) + 1)),
  lmdi %>%
    rename(year_t = year, o_it = o_i, p_it = p_i, d_it = d_i),
  by = c("city", "year_t", "obsr_grp")
) %>%
  select(city, year_0, year_t, obsr_grp, o_i0, o_it, p_i0, p_it, d_i0, d_it) %>%
  filter(!(city == "Kawasaki" & (year_0 == "2017" | year_t == "2017"))) %>%
  filter(!(city == "Nagoya" & (year_0 == "2016" | year_t == "2016"))) %>%
  group_by(city, year_0) %>%
  mutate(p_0 = sum(p_i0)) %>%
  ungroup() %>%
  group_by(city, year_t) %>%
  mutate(p_t = sum(p_it)) %>%
  ungroup()

# manual check: row number should be 80
nrow(lmdi)

# calculate the variables for LMDI
lmdi <- lmdi %>%
  mutate(
    s_i0 = p_i0 / p_0,
    s_it = p_it / p_t,
    f_i0 = d_i0 / p_i0,
    f_it = d_it / p_it,
    i_i0 = o_i0 / d_i0,
    i_it = o_it / d_it
  ) %>%
  mutate(
    delt_p_i = (o_it - o_i0) / log(o_it / o_i0) * log(p_t / p_0),
    delt_s_i = (o_it - o_i0) / log(o_it / o_i0) * log(s_it / s_i0),
    delt_f_i = (o_it - o_i0) / log(o_it / o_i0) * log(f_it / f_i0),
    delt_i_i = (o_it - o_i0) / log(o_it / o_i0) * log(i_it / i_i0)
  ) %>%
  group_by(city, year_t, year_0) %>%
  summarise(
    o_0 = sum(o_i0),
    o_t = sum(o_it),
    delt_p = sum(delt_p_i),
    delt_s = sum(delt_s_i),
    delt_f = sum(delt_f_i),
    delt_i = sum(delt_i_i)
  ) %>%
  ungroup() %>%
  mutate(delt_o = o_t - o_0)

# visualization
# original plot
png(filename = "data_proc/LMDI_effect_1.png", res = 300,
    width = 1800, height = 2200)
(lmdi %>%
    select(-o_0, -o_t, -delt_o) %>%
    pivot_longer(cols = c(delt_p, delt_s, delt_f, delt_i),
                 names_to = "delt", values_to = "delt_val") %>%
    group_by(city, year_t, year_0) %>%
    mutate(delt_abs_max = max(abs(delt_val))) %>%
    ungroup() %>%
    mutate(delt_val_scale = delt_val / delt_abs_max) %>%
    mutate(pos_neg = case_when(
      delt_val_scale > 0 ~ 1,
      TRUE ~ -1
    )) %>%
    mutate(delt = factor(delt, levels = c("delt_p", "delt_s", "delt_f", "delt_i"))) %>%
    ggplot() +
    geom_col(aes(year_t, delt_val_scale, fill = as.character(pos_neg))) +
    theme_bw() +
    facet_grid(city ~ delt) +
    scale_fill_manual(
      name = "Effect Direction",
      limits = c("1", "-1"),
      values = c("darkgreen", "darkred"),
      labels = c("Positive", "Negative")
    ) +
    theme(legend.position = "bottom") +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 1)) +
    labs(x = "", y = "Scaled Effect"))
dev.off()

# line plot
png(filename = "data_proc/LMDI_effect_2.png", res = 300,
    width = 2000, height = 1500)
(lmdi %>%
    select(-o_0, -o_t, -delt_o) %>%
    pivot_longer(cols = c(delt_p, delt_s, delt_f, delt_i),
                 names_to = "delt", values_to = "delt_val") %>%
    group_by(city, year_t, year_0) %>%
    mutate(delt_abs_max = max(abs(delt_val))) %>%
    ungroup() %>%
    mutate(delt_val_scale = delt_val / delt_abs_max) %>%
    mutate(delt = factor(delt, levels = c("delt_p", "delt_s", "delt_f", "delt_i"))) %>%
    ggplot() +
    geom_line(aes(year_t, delt_val_scale, group = city, col = city)) +
    theme_bw() +
    facet_wrap(.~ delt))
dev.off()

# tile plot
png(filename = "data_proc/LMDI_effect_3.png", res = 300,
    width = 2400, height = 1500)
(lmdi %>%
    select(-o_0, -o_t, -delt_o) %>%
    pivot_longer(cols = c(delt_p, delt_s, delt_f, delt_i),
                 names_to = "delt", values_to = "delt_val") %>%
    group_by(city, year_t, year_0) %>%
    mutate(delt_abs_max = max(abs(delt_val))) %>%
    ungroup() %>%
    mutate(delt_val_scale = delt_val / delt_abs_max) %>%
    mutate(delt = factor(delt, levels = c("delt_p", "delt_s", "delt_f", "delt_i"))) %>%
    ggplot(aes(year_t, city)) +
    geom_tile(aes(fill = delt_val_scale)) +
    # theme_bw() +
    scale_fill_gradient2(
      name = "Effect", low = "darkred", high = "darkgreen", mid = "white"
    ) +
    geom_text(aes(label = sprintf("%.1f", delt_val_scale))) +
    facet_wrap(.~ delt, nrow = 1) +
    labs(x = "", y = "City"))
dev.off()
