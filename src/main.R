# Statement ----
# 用于分析新冠对公民科学行为的影响，主要思路：从年度和月度粒度上分析，然后分用户组进行分析。

# Package ----
pacman::p_load(
  openxlsx, dplyr, tidyr, ggplot2, ggh4x, patchwork, reshape2, lubridate
)

# Function ----
source("src/function.R")

# Read data ----
## Constant ----
# City factor levels based on pupulation.
kCity <- c("Tokyo", "Yokohama", "Osaka", "Nagoya", "Sapporo", "Fukuoka",
           "Kobe", "Kawasaki", "Kyoto", "Saitama")

## iNaturalist data ----
### Raw data ----
# Raw data directory.
inat.file <- list.files("data_raw/iNatData", full.names = TRUE)

# Middle data of raw data.
record.raw <- lapply(inat.file, GetRaw) %>%
  do.call(rbind, .)
# The original number of observations.
nrow(record.raw)

# Identify the "super user" - it should be noted that a "super user" is city- and year-specific, for example, a super user in one city for a certain year might not be a super user in another city.
super.user <- record.raw %>%
  # Criterion 1: high proportion of contribution.
  group_by(city, year, user_id) %>%
  summarise(obs = n(), .groups = "drop") %>%
  group_by(city, year) %>%
  mutate(obs_prop = obs / sum(obs)) %>%
  ungroup() %>%
  # Criterion 2: extremely higher observation than the others.
  group_by(city, year) %>%
  mutate(obs_99 = quantile(obs, 0.99)) %>%
  ungroup() %>%
  mutate(super_user = case_when(
    obs_prop > 0.3 | obs > obs_99 ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  select(city, year, user_id, super_user)

# User data.frame.
user.grp <- record.raw %>%
  select(user_id, year) %>%
  distinct() %>%
  # Number of years with observation.
  group_by(user_id) %>%
  summarise(n_yr = n()) %>%
  ungroup() %>%
  # User group based on number of years with observation.
  mutate(obsr_grp = case_when(n_yr <= 1 ~ "short", TRUE ~ "long"))
table(user.grp$n_yr)
# Most users are short-term users.

# Target city: cities with max year user > 50.
tar.city <- record.raw %>%
  group_by(city, year) %>%
  summarise(user_pop = length(unique(user_id)), .groups = "drop") %>%
  group_by(city) %>%
  summarise(max_user_pop = max(user_pop), .groups = "drop") %>%
  filter(max_user_pop > 50) %>%
  mutate(city = factor(city, levels = kCity))

# Remove super users and non-target cities.
record.raw <- record.raw %>%
  filter(city %in% tar.city$city) %>%
  left_join(super.user, c("city", "user_id", "year")) %>%
  filter(!super_user) %>%
  select(city, id, user_id, obs_date, year)

### Yearly user data ----
# Data of each city-year-user.
record.user.yr <- record.raw %>%
  group_by(city, year, user_id) %>%
  summarise(
    obs = n(),
    act_day = length(unique(obs_date)),
    obs_per_day = obs / act_day,
    .groups = "drop"
  ) %>%
  left_join(user.grp, by = "user_id")

### Yearly data ----
# Data of each city-year.
record.yr <- record.user.yr %>%
  group_by(city, year) %>%
  summarise(
    obs = sum(obs),
    user_pop = n(),
    act_day = sum(act_day),
    prop_long_user = sum(obsr_grp == "long") / user_pop,
    .groups = "drop"
  ) %>%
  mutate(
    city = factor(city, levels = kCity),
    day_per_user = act_day / user_pop,
    obs_per_day = obs / act_day
  )
# Manually test the data distribution among year-city matrix.
table(record.yr$year, record.yr$city)
# Add data to the matrix gaps.

# Analysis ----
## Observation change ----
# Scaled observation change.
record.yr %>%
  select(city, year, obs) %>%
  group_by(city) %>%
  mutate(obs_scale = obs / max(obs)) %>%
  ggplot() +
  geom_line(aes(as.numeric(as.character(year)), obs_scale, col = city))

# Panel observation change plot.
# If there is no dir called "data_proc", make one.
jpeg(filename = "data_proc/obs_chg.jpg", res = 350,
    width = 3600, height = 800)
(
  record.yr %>%
    ggplot() +
    geom_line(aes(as.numeric(as.character(year)), obs)) +
    geom_point(aes(as.numeric(as.character(year)), obs), size = 0.6) +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_vline(xintercept = 2019, col = "red", alpha = 0.5) +
    facet_wrap(.~ city, scale = "free", nrow = 1) +
    expand_limits(y = 0) +
    labs(x = "Year", y = "Number of observations")
)
dev.off()

## User group ----
### Metrics ~ user grps ----
# Indexes comparison between long- and short-term users.
jpeg(filename = "data_proc/user_behavior_comparison.jpg", res = 300,
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
# Conclusion: Long-term users has higher observation because they active more days every year.

### Metrics ~ years ----
# Indexes of user groups by year.
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
# Conclusion: The pandemic has different impacts on different user groups.

## Factor mean value change ----
jpeg(filename = "data_proc/metric_change_for_each_city.jpg", res = 300,
    width = 3500, height = 2000)
(
  record.yr %>%
    select(city, year, user_pop, prop_long_user, day_per_user, obs_per_day) %>%
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
    geom_line(aes(as.numeric(as.character(year)), metric_val)) +
    geom_point(aes(as.numeric(as.character(year)), metric_val), size = 0.6) +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_vline(xintercept = 2019, col = "red", alpha = 0.5) +
    expand_limits(y = 0) +
    facet_grid2(
      vars(metric), vars(city), scales = "free", independent = "y"
    ) +
    labs(x = "Year", y = "")
)
dev.off()

# How do the indexes change compared to the last year?
# Bug: Might not work after delete the var yr_abbr.
CompTwoYr(record.yr, yr.base = 2016, yr.tar = 2017) /
  CompTwoYr(record.yr, yr.base = 2017, yr.tar = 2018) /
  CompTwoYr(record.yr, yr.base = 2018, yr.tar = 2019) /
  CompTwoYr(record.yr, yr.base = 2019, yr.tar = 2020) /
  CompTwoYr(record.yr, yr.base = 2020, yr.tar = 2021)
# Conclusion: Some users are more active during the pandemic?

## LMDI ----
# To attribute the change of observations into different factors.
lmdi <- record.user.yr %>%
  group_by(city, year, obsr_grp) %>%
  summarise(
    o_i = sum(obs), p_i = n(), d_i = sum(act_day), .groups = "drop"
  ) %>%
  arrange(city, year, obsr_grp)
# Check data.
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
  # Filter the missing data.
  group_by(city, year_0) %>%
  mutate(p_0 = sum(p_i0)) %>%
  ungroup() %>%
  group_by(city, year_t) %>%
  mutate(p_t = sum(p_it)) %>%
  ungroup()

# Row number should be 80; but since we filtered some rows with missin data in the last step, so it should be less than 80.
nrow(lmdi)

# Calculate the variables for LMDI.
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
    diff_o_it_o_i0 = case_when(o_it - o_i0 == 1 ~ 0.0001, TRUE ~ o_it - o_i0),
    rate_o_it_o_i0 = case_when(o_it / o_i0 == 1 ~ 0.9999, TRUE ~ o_it / o_i0)
  ) %>%
  mutate(
    delt_p_i = diff_o_it_o_i0 / log(rate_o_it_o_i0) * log(p_t / p_0),
    delt_s_i = diff_o_it_o_i0 / log(rate_o_it_o_i0) * log(s_it / s_i0),
    delt_f_i = diff_o_it_o_i0 / log(rate_o_it_o_i0) * log(f_it / f_i0),
    delt_i_i = diff_o_it_o_i0 / log(rate_o_it_o_i0) * log(i_it / i_i0)
  ) %>%
  group_by(city, year_t, year_0) %>%
  summarise(
    o_0 = sum(o_i0),
    o_t = sum(o_it),
    delt_p = sum(delt_p_i),
    delt_s = sum(delt_s_i),
    delt_f = sum(delt_f_i),
    delt_i = sum(delt_i_i),
    .groups = "drop"
  ) %>%
  mutate(delt_o = o_t - o_0)

# Visualization by tile plot.
jpeg(filename = "data_proc/LMDI_effect.jpg", res = 300,
    width = 3500, height = 2000)
(
  lmdi %>%
    select(-o_0, -o_t, -delt_o) %>%
    pivot_longer(cols = c(delt_p, delt_s, delt_f, delt_i),
                 names_to = "delt", values_to = "delt_val") %>%
    group_by(city, year_t, year_0) %>%
    mutate(delt_abs_max = max(abs(delt_val))) %>%
    ungroup() %>%
    mutate(delt_val_scale = delt_val / delt_abs_max) %>%
    mutate(
      delt = case_when(
        delt == "delt_p" ~ "Population",
        delt == "delt_s" ~ "Structure",
        delt == "delt_f" ~ "Frequency",
        delt == "delt_i" ~ "Intensity"
      ),
      year = case_when(
        year_t == 2017 ~ "2016 - 2017",
        year_t == 2018 ~ "2017 - 2018",
        year_t == 2019 ~ "2018 - 2019",
        year_t == 2020 ~ "2019 - 2020",
        year_t == 2021 ~ "2020 - 2021",
        year_t == 2022 ~ "2021 - 2022",
        year_t == 2023 ~ "2022 - 2023"
      )
    ) %>%
    mutate(
      delt = factor(
        delt, levels = c("Population", "Structure", "Frequency", "Intensity")
      ),
      city = factor(city, levels = rev(kCity))
    ) %>%
    ggplot(aes(year, city)) +
    geom_tile(aes(fill = delt_val_scale)) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_gradient2(
      name = "Effect", low = "darkred", high = "darkgreen", mid = "white"
    ) +
    geom_text(aes(label = sprintf("%.1f", delt_val_scale))) +
    facet_wrap(.~ delt, nrow = 1) +
    labs(x = "", y = "City")
)
dev.off()
