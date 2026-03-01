pacman::p_load(
    Ipaper, data.table, dplyr, lubridate, 
    ggplot2
)

df = list(
    `100ky`=fread("data/100kyrCO2.csv"), 
    `200ky`=fread("data/200kyrCO2.csv"),
    `500ky`=fread("data/500kyrCO2.csv")
) %>% melt_list("step")

p = ggplot(df, aes(x = age, q50)) +
  # 绘制 25%-75% 置信区间的 ribbon
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5), alpha = 0.3, color = NA) + 
  geom_line() + 
  scale_x_reverse(breaks = seq(70, 0, by = -10), limits = c(68, 0)) + 
  facet_wrap(~step, nrow = 1) 

write_fig(p, 'd:/Rplot.pdf', 15, 5)
