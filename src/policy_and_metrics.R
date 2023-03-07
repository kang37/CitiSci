# Package
library(tidyr)

# 统计分析各月份新冠感染数和政策强度对公民科学各项指标的影响

# 读取并解析每月紧急事态时长数据
emer <- read.xlsx("RawData/Japan_emergency_period_2019.xlsx") %>%
  rename(order = "序号", start = "开始或延长开始时间",
         end = "结束时间", pref = "实施区域") %>%
  # 将“全部”改成目标范围内的都道府县
  mutate(pref = ifelse(
    pref == "全部", paste(pre_city$prefecture_jp, collapse = ","), pref
  )) %>%
  # 拆分县信息一列为多列
  separate(col = "pref", sep = ",",
           into = as.character(seq_along(1:nrow(pre_city)))) %>%
  # 提取县名，将宽数据改成长数据
  pivot_longer(cols = as.character(seq_along(1:nrow(pre_city))),
               names_to = "col", values_to = "pref") %>%
  subset(!is.na(pref)) %>%
  select(-col) %>%
  # 更改日期格式
  mutate(start = as_date(start), end = as_date(end)) %>%
  # 添加跨月份节点
  mutate(intrmth = ifelse(
    month(end) - month(start) > 0,
    paste(year(start), month(start), "1", sep = "-"),
    NA
  )) %>%
  mutate(intrmth = as_date(intrmth) + months(1))%>% mutate(
    prd_mth = month(start),
    prd = ifelse(
      is.na(intrmth),
      as.numeric(end - start) + 1,
      as.numeric(intrmth - start)
    )) %>%
  mutate(
    prd_nxt_mth = ifelse(
      is.na(intrmth),
      NA,
      month(end)
    ),
    prd_nxt = ifelse(
      !is.na(intrmth),
      as.numeric(end - intrmth) + 1,
      NA
    ))

emer <- rbind(
  select(emer, -prd_nxt_mth, -prd_nxt),
  select(emer, -prd_mth, -prd) %>% subset(!is.na(intrmth)) %>%
    rename(prd_mth = prd_nxt_mth, prd = prd_nxt)
) %>% group_by(pref, prd_mth) %>%
  summarise(prd = sum(prd)) %>%
  ungroup() %>%
  arrange(pref, prd_mth) %>%
  # 加入县和城市英文名等信息
  left_join(pre_city, by = c("pref" = "prefecture_jp"))


emer.citisci <- tot_mthdata %>%
  subset(year == 2019) %>%
  left_join(emer,
            by = c("city", "month" = "prd_mth")) %>%
  as_tibble()

for (i in c("obs", "obs_id", "idpa", "users", "act_days", "obs_per_user",
            "actdays_per_user", "obs_pu_pd", "id_rate")) {
  print(ggplot(emer.citisci) +
          geom_col(aes(x = month, y = ifelse(is.na(prd), 0, Inf)),
                   fill = "red", alpha = 0.5) +
          geom_line(aes_string("month", i)) +
          facet_wrap(.~ city, scales = "free_y") +
          labs(y = i))
}
