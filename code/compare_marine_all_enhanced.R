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

# 定义地质时代
epochs <- data.frame(
  start = c(0, 2.58, 5.33, 23, 33.9, 56, 66),
  end = c(2.58, 5.33, 23, 33.9, 56, 66, 70),
  name = c("Pleistocene", "Pliocene", "Miocene", "Oligocene", "Eocene", "Paleocene", "")
)

# 创建图形 1: 主对比图
p1 <- ggplot(cp_combined, aes(x = ages, color = type, fill = type)) +
  # 绘制 25%-75% 置信区间的 ribbon
  geom_ribbon(aes(ymin = X25., ymax = X75.), alpha = 0.25, color = NA) +
  # 绘制中位数线
  geom_line(aes(y = X50.), linewidth = 1.2) +
  # 反转 x 轴（从过去到现在）
  scale_x_reverse(breaks = seq(70, 0, by = -10)) +
  # 设置 y 轴刻度为实际 CO2 浓度
  scale_y_continuous(
    breaks = log(c(100, 200, 400, 800, 1600)),
    labels = c(100, 200, 400, 800, 1600),
    limits = c(log(200), log(1800))
  ) +
  # 设置颜色
  scale_color_manual(
    values = c("All proxies" = "dodgerblue4", "Marine only" = "darkred"),
    name = ""
  ) +
  scale_fill_manual(
    values = c("All proxies" = "dodgerblue4", "Marine only" = "darkred"),
    name = ""
  ) +
  # 标签
  labs(
    x = "Age (Ma)",
    y = expression("CO"[2] * " (ppm)"),
    title = "Cenozoic CO2 Reconstruction: All Proxies vs Marine Only",
    subtitle = "Solid line: median; Shaded area: interquartile range (25%-75%)"
  ) +
  # 主题设置
  theme_bw(base_size = 13) +
  theme(
    legend.position = c(0.12, 0.88),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray50"),
    legend.key.size = unit(1.5, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray40", margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )

# 保存主图
ggsave("out/main_figs/Compare_Marine_All_Enhanced.png", 
       plot = p1, 
       width = 11, 
       height = 6.5, 
       dpi = 600)

print(p1)

# 创建图形 2: 差异图
diff_data <- cp_all %>%
  select(ages, all_50 = X50., all_25 = X25., all_75 = X75.) %>%
  left_join(
    cp_marine %>% select(ages, marine_50 = X50., marine_25 = X25., marine_75 = X75.),
    by = "ages"
  ) %>%
  mutate(
    diff_50 = exp(all_50) - exp(marine_50),
    diff_25 = exp(all_25) - exp(marine_25),
    diff_75 = exp(all_75) - exp(marine_75)
  )

p2 <- ggplot(diff_data, aes(x = ages)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_ribbon(aes(ymin = diff_25, ymax = diff_75), 
              fill = "gray60", alpha = 0.3) +
  geom_line(aes(y = diff_50), color = "black", linewidth = 1.2) +
  scale_x_reverse(breaks = seq(70, 0, by = -10)) +
  labs(
    x = "Age (Ma)",
    y = expression(Delta * "CO"[2] * " (ppm, All - Marine)"),
    title = "Difference between All Proxies and Marine Only Reconstructions",
    subtitle = "Positive values: All proxies > Marine only"
  ) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.title = element_text(size = 12)
  )

ggsave("out/main_figs/Difference_Marine_All.png", 
       plot = p2, 
       width = 11, 
       height = 5, 
       dpi = 600)

print(p2)

# 统计摘要
cat("\n=== Statistical Summary ===\n")
cat(sprintf("Mean absolute difference: %.1f ppm\n", mean(abs(diff_data$diff_50))))
cat(sprintf("Median absolute difference: %.1f ppm\n", median(abs(diff_data$diff_50))))
cat(sprintf("Max positive difference: %.1f ppm at %.1f Ma\n", 
            max(diff_data$diff_50), 
            diff_data$ages[which.max(diff_data$diff_50)]))
cat(sprintf("Max negative difference: %.1f ppm at %.1f Ma\n", 
            min(diff_data$diff_50), 
            diff_data$ages[which.min(diff_data$diff_50)]))

# 关键时期统计
cat("\n=== Key Time Periods ===\n")
key_ages <- c(0.1, 2.6, 16, 33.9, 50, 66)

for (age in key_ages) {
  idx <- which.min(abs(diff_data$ages - age))
  cat(sprintf("\n%.1f Ma:\n", diff_data$ages[idx]))
  cat(sprintf("  All proxies:   %.0f ppm [%.0f, %.0f]\n", 
              exp(diff_data$all_50[idx]), 
              exp(diff_data$all_25[idx]), 
              exp(diff_data$all_75[idx])))
  cat(sprintf("  Marine only:   %.0f ppm [%.0f, %.0f]\n", 
              exp(diff_data$marine_50[idx]), 
              exp(diff_data$marine_25[idx]), 
              exp(diff_data$marine_75[idx])))
  cat(sprintf("  Difference:    %.0f ppm [%.0f, %.0f]\n", 
              diff_data$diff_50[idx],
              diff_data$diff_25[idx],
              diff_data$diff_75[idx]))
}

cat("\n图形已保存到:\n")
cat("  - out/main_figs/Compare_Marine_All_Enhanced.png\n")
cat("  - out/main_figs/Difference_Marine_All.png\n")
