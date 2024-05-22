# Function ----
# Function: Read and merge raw data into a list, each element of the list for a city.
# Argument:
# file.dir: file directory.
GetRaw <- function(file.dir) {
  city.name <- strsplit(file.dir, "/") %>%
    .[[1]] %>%
    .[3] %>%
    gsub(".csv", "", .)
  read.csv(file.dir) %>%
    tibble() %>%
    # Get date.
    rename(obs_date = observed_on) %>%
    mutate(
      obs_date = as_date(obs_date),
      city = city.name,
      year = year(obs_date),
      month = month(obs_date),
      day = day(obs_date)
    ) %>%
    # Keep the data after 2015.
    filter(year > 2015, year < 2024) %>%
    mutate(year = as.factor(year)) %>%
    select(city, id, user_id, obs_date, year) %>%
    return()
}

# Function: Compare data of two years and visualize the results.
# Argument:
# x: data frame for each city-year-index.
# yr.base: base year.
# yr.tar: the second year.
CompTwoYr <- function(x, yr.base, yr.tar) {
  x$year <- as.numeric(as.character(x$year))

  # Divide data by year.
  x1 <- subset(x, year == yr.base) %>%
    mutate(city = factor(city, levels = kCity)) %>%
    arrange(city)
  x2 <- subset(x, year == yr.tar) %>%
    mutate(city = factor(city, levels = kCity)) %>%
    arrange(city)

  # Calculate difference between the values.
  cbind(city = as.character(x1$city),
        ifelse(x2[names(x2)[!names(x2) %in% c("city", "year", "year")]] /
                 x1[names(x1)[!names(x1) %in% c("city", "year", "year")]] > 1,
               paste0(yr.tar, " > ", yr.base),
               paste0(yr.tar, " <= ", yr.base))) %>%
    as.data.frame() %>%
    # Turn to long data and visualization.
    melt(data = ., id = "city") %>%
    mutate(city = factor(city, levels = kCity)) %>%
    ggplot() +
    geom_tile(aes(x = city, y = variable, fill = value), alpha = 0.5) +
    theme(axis.text.x = element_text(angle = 90))
}

# Function: group-summarise the sample size, mean, SD, SE, etc.
# Argument:
# x: data of each city with user group information.
# name.var: target index.
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

  # Statistic comparison of data of different cities.
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

  # Merge statistic analysis results to a data.frame.
  x_output <- merge(x_output, x_aov, all.x = TRUE)
  x_output$aov_mark <- "   "
  x_output$aov_mark[x_output$aov] <- " * "

  x_output <- x_output %>%
    mutate(city = factor(city, levels = kCity)) %>%
    arrange(city)

  return(x_output)
}

# Function: Mean value column plot with SE bar, for user group analysis.
# Argument:
# x: data with mean value and SE of each city.
# Bug: How to fix the order of the labels - not important though?
PlotBarError <- function(x, name.grp = "obsr_grp",
                         name.yaxis = NULL, name.title = NULL) {
  ggplot(
    data = x, aes(paste0(city, aov_mark), mean,
                  fill = factor(get(name.grp)))
  ) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.2),
                  position = position_dodge(0.9)) +
    labs(y = name.yaxis, title = name.title) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# Function: Mean-value column plot with SE bar, to compare user groups.
# Argument:
# x：user data with year, group, index information.
# name.yr：target year.
# user.grp：target user group.
# name.var：target index.
# name.yaxis：Y-axis title.
# name.title：figure title.
PlotCompObsr <- function(x, name.var, name.title, ...) {
  x.axis.lab <- MeanSeAov(x, name.var = name.var) %>%
    select(city, aov_mark) %>%
    unique()

  MeanSeAov(x, name.var = name.var) %>%
    PlotBarError(name.title = name.title, ...) +
    scale_fill_manual(name = "User group", values = c("#FFA500", "#1047A9")) +
    scale_x_discrete(limits = paste0(x.axis.lab$city, x.axis.lab$aov_mark))
}

# Function: Plot to show the difference before and after COVID-19.
# Argument:
# x：user data with year, group, index information.
# name.yr: target year.
# user.grp: target user group.
# name.var: target index.
# name.yaxis：Y-axis title.
# name.title：figure title.
PlotCovidYr <- function(x, name.yr = c("2019", "2020", "2021"),
                        user.grp, name.var, name.yaxis, name.title) {
  x.axis.lab <- subset(x, year %in% name.yr & obsr_grp == user.grp) %>%
    MeanSeAov(., name.grp = "year", name.var = name.var) %>%
    select(city, aov_mark) %>%
    unique()

  subset(x, year %in% name.yr & obsr_grp == user.grp) %>%
    # Calculate mean value, SE, p-value etc.
    MeanSeAov(., name.grp = "year", name.var = name.var) %>%
    PlotBarError(name.grp = "year",
                 name.yaxis = name.yaxis, name.title = name.title) +
    scale_fill_manual(name = "Year",
                      values = c("#009999", "#FFCE00", "#FF0000")) +
    scale_x_discrete(limits = paste0(x.axis.lab$city, x.axis.lab$aov_mark))
}
