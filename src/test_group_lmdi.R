# 如果要分用户组进行LMDI分解的话 ----

# 历年变化量分解
# 建立空列表以存储结果：初级按长短期用户分组，次级按年份分组，各用户组各年份对应一个包含多城市数据的数据框
lmdi.ls1 <- vector("list", length = 2)
names(lmdi.ls1) <- c("long", "short")

# get basic data for LMDI
for (i in names(lmdi.ls1)) {
  lmdi.ls1[[i]] <-
    # get Oi (obs number of i group) and Pi (population of i group) first
    record.city.obsr.yr %>%
    subset(obsr_grp == i) %>%
    group_by(city, year) %>%
    summarise(Oi = sum(obs),
              Pi = n()) %>%
    ungroup()
}
for (i in names(lmdi.ls1)) {
  lmdi.ls1[[i]] <- lmdi.ls1[[i]] %>%
    # get total population column for both long- and short-term group
    mutate(P = lmdi.ls1[["long"]]$Pi + lmdi.ls1[["short"]]$Pi) %>%
    # other basic data
    mutate(
      # get Si (structure effect of i group)
      Si = Pi / P,
      # get Ii (intensity effect of i group)
      Ii = Oi / Pi
    )
}






