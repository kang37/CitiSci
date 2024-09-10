# Statement ----
# Long-term, multiple-city study of citizen science change in Japan, with driving factor analysis.

# Package ----
pacman::p_load(
  openxlsx, sf, reshape2, lubridate, dplyr, tidyr, ggplot2, ggh4x, patchwork
)

# Function ----
source("src/function.R")

# Read data ----
## Constant ----
# City factor levels based on population.
kCityRaw <- c(
  "Tokyo", "Yokohama", "Osaka", "Nagoya", "Sapporo", "Fukuoka",
  "Kobe", "Kawasaki", "Kyoto", "Saitama", "Hiroshima", "Sendai"
)

## iNaturalist data ----
### Raw data ----
# Filter Kyoto Prefecture data to keep only Kyoto city data.
if (!file.exists("data_raw/iNatData/Kyoto.csv")) {
  # Kyoto boundary and latitude and longitude range.
  kyoto.boundary <-
    st_read("data_raw/KyotoCityBoundary", layer = "kyoto_city_boundary")
  kyoto.latlon.range <- st_bbox(kyoto.boundary)

  # Process Kyoto prefecture data.
  read.csv("data_raw/iNatData/Kyoto_pref.csv") %>%
    tibble() %>%
    filter(
      latitude > kyoto.latlon.range[names(kyoto.latlon.range) == "ymin"],
      latitude < kyoto.latlon.range[names(kyoto.latlon.range) == "ymax"],
      longitude > kyoto.latlon.range[names(kyoto.latlon.range) == "xmin"],
      longitude < kyoto.latlon.range[names(kyoto.latlon.range) == "xmax"]
    ) %>%
    # Turn to sf.
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(kyoto.boundary)) %>%
    # Keep iNaturalist data inside Kyoto Boundary.
    st_intersection(kyoto.boundary) %>%
    as_tibble() %>%
    write.csv("data_raw/iNatData/Kyoto.csv")
}

# Raw data directory.
inat.file <- paste0("data_raw/iNatData/", kCityRaw, ".csv")

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
  filter(max_user_pop > 50)
kCity <- kCityRaw[kCityRaw %in% tar.city$city]

# Remove super users and non-target cities.
record.filt <- record.raw %>%
  filter(city %in% tar.city$city) %>%
  left_join(super.user, c("city", "user_id", "year")) %>%
  filter(!super_user) %>%
  select(city, id, user_id, obs_date, year) %>%
  mutate(city = factor(city, levels = kCity))
# Observation number of filtered data.
nrow(record.filt)

### Yearly user data ----
# Data of each city-year-user.
record.user.yr <- record.filt %>%
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
## Research area ----
# Japan boundary.
japan.boundary <- st_read(
  dsn = "data_raw/JapanBoundary", layer = "Japan_boundary"
)

# City location.
city.loc <- c(
  "Tokyo", 139.6917, 35.6895, 0.6, 0,
  "Yokohama", 139.6380, 35.4437, -0.6, -0.3,
  "Osaka", 135.5022, 34.6937, 0.3, -0.2,
  "Nagoya", 136.9066, 35.1815, 0, 0.3,
  "Sapporo", 141.3545, 43.0618, 0, 0.3,
  "Fukuoka", 130.4017, 33.5904, 0, 0.3,
  "Kobe", 135.1955, 34.6901, -0.7, 0,
  "Kawasaki", 139.7172, 35.5308, 0.6, 0,
  "Kyoto", 135.7681, 35.0116, -0.1, 0.2,
  "Saitama", 139.6489, 35.8617, -0.6, 0.3,
  "Hiroshima", 132.4596, 34.3853, 0, 0.3,
  "Sendai", 140.8719, 38.2682, 0, 0
) %>%
  matrix(byrow = TRUE, ncol = 5) %>%
  as.data.frame() %>%
  rename_with(~ c("city", "longitude", "latitude", "x_adj", "y_adj")) %>%
  filter(city %in% kCity) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(japan.boundary)) %>%
  mutate(x_adj = as.numeric(x_adj), y_adj = as.numeric(y_adj))

# Research area map.
jpeg(
  filename = paste0("data_proc/map_", Sys.Date(), ".jpg"),
  res = 300, width = 160, height = 80, units = "mm"
)
ggplot() +
  geom_sf(data = japan.boundary, fill = "darkgrey", col = NA) +
  geom_sf(data = city.loc, size = 1.5, fill = "red", col = "white", shape = 21, stroke = 0.2) +
  theme_bw()
dev.off()

## General description ----
record.filt %>%
  group_by(city) %>%
  summarise(n = n(), .groups = "drop")

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
jpeg(
  filename = paste0("data_proc/obs_chg_", Sys.Date(), ".jpg"),
  res = 350, width = 4000, height = 600
)
(
  record.yr %>%
    ggplot() +
    geom_line(aes(as.numeric(as.character(year)), obs)) +
    geom_point(aes(as.numeric(as.character(year)), obs), size = 0.6) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_vline(xintercept = 2019, col = "red", alpha = 0.5) +
    facet_wrap(.~ city, scale = "free", nrow = 1) +
    expand_limits(y = 0) +
    labs(x = "Year", y = "Observations")
)
dev.off()

## User group ----
# Compare long-term and short-term users.
# Firstly, test normality of values for each city-group-metric.
record.user.yr %>%
  select(-n_yr) %>%
  pivot_longer(
    cols = c(obs, act_day, obs_per_day), names_to = "metric", values_to = "val"
  ) %>%
  group_by(city, obsr_grp, metric) %>%
  summarise(norm_dist = shapiro.test(val)$p.value, .groups = "drop") %>%
  mutate(norm_dist_dir = c(norm_dist > 0.05)) %>%
  pull(norm_dist_dir) %>%
  table()
# Since no data is normal distributed, use Mann-Whitney test.
# One side Wilcox test: long-term > short-term.
user.comp.res <-
  record.user.yr %>%
  select(-n_yr) %>%
  pivot_longer(
    cols = c(obs, act_day, obs_per_day),
    names_to = "metric", values_to = "val"
  ) %>%
  mutate(obsr_grp = factor(obsr_grp, levels = c("long", "short"))) %>%
  # Calculate p mark from Wilcox test.
  group_by(city, metric) %>%
  summarise(
    long_short =
      list(wilcox.test(val ~ obsr_grp, alternative = "greater")) %>%
      lapply(., function(x) x$p.value) %>%
      unlist(),
    short_long =
      list(wilcox.test(val ~ obsr_grp, alternative = "less")) %>%
      lapply(., function(x) x$p.value) %>%
      unlist(),
    .groups = "drop"
  ) %>%
  mutate(city = factor(city, levels = kCity))

jpeg(
  filename = paste0("data_proc/user_behavior_comp_", Sys.Date(), ".jpg"),
  res = 300, width = 160, height = 80, units = "mm"
)
user.comp.res %>%
  mutate(
    metric = factor(metric, levels = rev(c("obs", "act_day", "obs_per_day"))),
    comp_res = case_when(
      long_short < 0.05 & short_long >= 0.05 ~ "Regular > Non-regular",
      long_short >= 0.05 & short_long < 0.05 ~ "Non-regular > Regular",
      long_short >= 0.05 & short_long >= 0.05 ~ "No significant difference",
      long_short < 0.05 & short_long < 0.05 ~ "Error"
    )
  ) %>%
  ggplot() +
  geom_tile(aes(city, metric, fill = comp_res), col = "white") +
  labs(x = "", y = "Metric", fill = "Comparison result") +
  scale_fill_manual(
    breaks = c(
      "Regular > Non-regular",
      "Non-regular > Regular",
      "No significant difference"
    ),
    values = c("#1047A9", "#FFA500", "grey")
  ) +
  scale_y_discrete(
    breaks = c("obs", "act_day", "obs_per_day"),
    labels = c(
      "Annual\n observations",
      "Annual\n active\n days",
      "Annual\n daily\n observations"
    )
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
dev.off()
# Conclusion: Long-term users has higher observation because they active more days every year.

## Factor mean value change ----
jpeg(
  filename = paste0("data_proc/metric_chg_for_each_city_", Sys.Date(), ".jpg"),
  res = 300, width = 3500, height = 1600
)
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
    theme_bw() +
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
  arrange(city, year, obsr_grp) %>%
  # To solve zero-value problem, give zero-value a very small value.
  pivot_longer(
    cols = c(o_i, p_i, d_i), names_to = "metric", values_to = "metric_val"
  ) %>%
  mutate(metric_val = as.numeric(metric_val)) %>%
  pivot_wider(
    names_from = "obsr_grp", values_from = "metric_val", values_fill = 1
  ) %>%
  pivot_longer(
    cols = c(long, short), names_to = "obsr_grp", values_to = "metric_val"
  ) %>%
  pivot_wider(
    names_from = "metric", values_from = "metric_val"
  )
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
  ungroup() %>%
  # Calculate the variables for LMDI.
  mutate(
    s_i0 = p_i0 / p_0,
    s_it = p_it / p_t,
    f_i0 = d_i0 / p_i0,
    f_it = d_it / p_it,
    i_i0 = o_i0 / d_i0,
    i_it = o_it / d_it
  ) %>%
  mutate(
    diff_o_it_o_i0 = o_it - o_i0,
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

# Scale the effects.
lmdi_scale <- lmdi %>%
  # Scale by dividing max value.
  select(-o_0, -o_t, -delt_o) %>%
  pivot_longer(cols = c(delt_p, delt_s, delt_f, delt_i),
               names_to = "delt", values_to = "delt_val") %>%
  group_by(city, year_t, year_0) %>%
  mutate(delt_abs_max = max(abs(delt_val))) %>%
  ungroup() %>%
  mutate(delt_val_scale = delt_val / delt_abs_max) %>%
  # Revise values.
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
  # Add NA values.
  select(city, year, delt, delt_val_scale) %>%
  pivot_wider(names_from = city, values_from = delt_val_scale) %>%
  pivot_longer(
    cols = all_of(kCity),
    names_to = "city", values_to = "delt_val_scale"
  ) %>%
  # Add factor information.
  mutate(
    delt = factor(
      delt, levels = c("Population", "Structure", "Frequency", "Intensity")
    ),
    city = factor(city, levels = rev(kCity))
  )

# Visualization by tile plot.
jpeg(
  filename = paste0("data_proc/lmdi_effect_", Sys.Date(), ".jpg"),
  res = 400, width = 3500, height = 2000
)
(
  lmdi_scale %>%
    ggplot(aes(year, city)) +
    geom_tile(aes(fill = delt_val_scale), col = "grey") +
    scale_fill_gradient2(
      name = "Effect", low = "darkred", high = "darkgreen", mid = "white",
      na.value = "grey"
    ) +
    geom_text(aes(label = sprintf("%.1f", delt_val_scale)), size = 3) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(.~ delt, nrow = 1) +
    labs(x = "", y = "City")
)
dev.off()

# Importance of effects.
lmdi_scale %>%
  group_by(delt) %>%
  summarise(
    delt_val_scale_abs_mean = mean(abs(delt_val_scale)), .groups = "drop"
  ) %>%
  arrange(-delt_val_scale_abs_mean)
