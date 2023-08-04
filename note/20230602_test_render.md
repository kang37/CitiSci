---
title: "Test render md file"
author: "Kang"
date: "2023-06-02"
output: 
  html_document: 
    keep_md: yes
---



## R Markdown

This is a test. 


```r
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
        year_t == 2021 ~ "2020 - 2021"
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
```

![](20230602_test_render_files/figure-html/cars-1.png)<!-- -->