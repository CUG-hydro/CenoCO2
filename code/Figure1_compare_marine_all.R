library(ggplot2)
library(dplyr)
library(tidyr)

# 读取数据
cp_all <- read.csv("out/200kyrCO2.csv")
cp_marine <- read.csv("out/200kyrCO2MarOnly.csv")

# 为数据添加类型标签
cp_all$type <- "All proxies"
cp_marine$type <- "Marine only"

# 合并数据
cp_combined <- rbind(cp_all, cp_marine)

# 创建图形
p <- ggplot(cp_combined, aes(x = ages, color = type, fill = type)) +
  # 绘制 25%-75% 置信区间的 ribbon
  geom_ribbon(aes(ymin = X25., ymax = X75.), alpha = 0.3, color = NA) +
  # 绘制中位数线
  geom_line(aes(y = X50.), linewidth = 1) +
  # 反转 x 轴（从过去到现在）
  scale_x_reverse(breaks = seq(70, 0, by = -10)) +
  # 设置 y 轴刻度为实际 CO2 浓度
  scale_y_continuous(
    breaks = log(c(100, 200, 400, 800, 1600)),
    labels = c(100, 200, 400, 800, 1600)
  ) +
  # 设置颜色
  scale_color_manual(values = c("All proxies" = "dodgerblue4", "Marine only" = "darkred")) +
  scale_fill_manual(values = c("All proxies" = "dodgerblue4", "Marine only" = "darkred")) +
  # 标签
  labs(
    x = "Age (Ma)",
    y = expression("CO"[2] * " (ppm)"),
    title = "Comparison of CO2 Reconstructions: All Proxies vs Marine Only",
    subtitle = "Median (line) and interquartile range (ribbon, 25%-75%)",
    color = "Dataset",
    fill = "Dataset"
  ) +
  # 主题设置
  theme_bw(base_size = 12) +
  theme(
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30")
  )

# 保存图形
ggsave("out/main_figs/Compare_Marine_All.png", 
       plot = p, 
       width = 10, 
       height = 6, 
       dpi = 600)

# 显示图形
print(p)

# 计算关键时期的差异统计
cat("\n=== Key Statistics ===\n")

# 选择几个关键时间点
key_ages <- c(0.1, 16, 34, 50, 66)

for (age in key_ages) {
  all_data <- cp_all[which.min(abs(cp_all$ages - age)), ]
  marine_data <- cp_marine[which.min(abs(cp_marine$ages - age)), ]
  
  cat(sprintf("\nAge: %.1f Ma\n", age))
  cat(sprintf("  All proxies:   %.0f ppm (IQR: %.0f-%.0f)\n", 
              exp(all_data$X50.), exp(all_data$X25.), exp(all_data$X75.)))
  cat(sprintf("  Marine only:   %.0f ppm (IQR: %.0f-%.0f)\n", 
              exp(marine_data$X50.), exp(marine_data$X25.), exp(marine_data$X75.)))
  cat(sprintf("  Difference:    %.0f ppm\n", 
              exp(all_data$X50.) - exp(marine_data$X50.)))
}
