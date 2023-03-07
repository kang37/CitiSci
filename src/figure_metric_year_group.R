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
      theme(axis.title = element_blank(), axis.text.x = element_text(angle = 90))
  }
)

for (i in 1:6) {
  plot_ls[[i]] <- plot_ls[[i]] +
    labs(title = paste("Group", testvar$rec_yr_grp_less[i], testvar$tarvar[i]))
}

for (i in 1:4) {
  plot_ls[[i]] <- plot_ls[[i]] +
    theme(axis.text.x = element_blank())
}

png(filename = "报告_分用户组同指标不同年份对比.png", res = 300,
    width = 2000, height = 3500)
print((plot_ls[[1]] + plot_ls[[2]]) /
        (plot_ls[[3]] + plot_ls[[4]]) /
        (plot_ls[[5]] + plot_ls[[6]]) +
        plot_layout(guides = "collect") & theme(legend.position = "bottom"))
dev.off()
