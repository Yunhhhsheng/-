# 加载必要的包
library(ggplot2)
library(dplyr)

# ------------------------------
# 1. 数据整理（基于 csv 文件及原图信息）
# ------------------------------

# 定义每个子类别的名称、y轴位置（来自 csv 第三列的稳定值）、效应均值、置信区间上下限
# 以及观测数（N）和显著性标记（*）
df_data <- data.frame(
  subgroup = c("Overall", "Grassland", "Forest", "Farmland", "Desert",
               "Mesh bag", "Pesticide", "Eye pick",
               "Humid", "Dry sub-humid", "Semi-Arid", "Arid"),
  y_pos = c(374.4, 324.5, 299.34, 274.7, 249.36,
            199.3, 174.0, 150.1,
            99.6, 74.61, 49.6, 24.5),
  mean = c(-41.5856, -27.4461, -44.7512, -39.6863, -59.7349,
           -36.5207, -57.8355, -24.4916,
           -27.4461, -61.8452, -58.8907, -59.7349),
  lower = c(-54.66996, -49.81609, -57.83553, -78.93931, -83.16006,
            -50.66024, -69.86468, -57.83553,
            -41.58562, -86.11459, -73.87440, -74.92959),
  upper = c(-27.44608, -3.17674, -28.50127, 65.41055, -4.23193,
            -19.42665, -41.58562, 37.13148,
            -6.34230, 10.96280, -40.53043, -34.62137),
  N = c(187, 51, 124, 3, 9,
        136, 37, 14,
        120, 12, 37, 18),
  sig = c("***", "*", "***", "", "***",
          "**", "***", "",
          "*", "", "***", "***"),
  effect_type = c("negative", "negative", "negative", "non-signif", "negative",
                  "negative", "negative", "non-signif",
                  "negative", "non-signif", "negative", "negative")
)

# 添加用于显示的标签（含样本量）
df_data$label <- paste0(df_data$subgroup, " (", df_data$N, ")")

# 确保顺序从上到下与 y_pos 递减一致
df_data <- df_data %>% arrange(desc(y_pos))

# 定义分组主标题的 y 坐标（用于左侧注释）
group_annotations <- data.frame(
  label = c("Ecosystems", "Method", "Aridity Index"),
  y = c(287, 175, 62),   # 对应各组子类别中间位置
  x = -95
)

# ------------------------------
# 2. 绘图
# ------------------------------

p <- ggplot(df_data, aes(y = y_pos)) +
  # 误差线（水平）
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 1.2, color = "black", linewidth = 0.6) +
  # 均值点：显著负效应为蓝色实心圆，不显著为空心圆（黑色边框）
  geom_point(aes(x = mean, fill = effect_type, color = effect_type),
             shape = 21, size = 3.5, stroke = 0.8) +
  # 添加显著性星号（位于点的右侧偏移 3 个单位）
  geom_text(aes(x = mean + 3, label = sig),
            hjust = 0, vjust = 0.5, size = 4, color = "black") +
  # 垂直参考线在 x = 0 处加粗
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) +
  # 水平网格线（仅保留主要水平线，美化）
  geom_hline(yintercept = df_data$y_pos, color = "gray90", linewidth = 0.4) +
  # 左侧分组标题（英文文字）
  annotate("text", x = group_annotations$x, y = group_annotations$y,
           label = group_annotations$label, hjust = 0, vjust = 0.5,
           fontface = "bold", size = 4.5, color = "black") +
  # 坐标轴与主题设置
  scale_y_reverse(breaks = df_data$y_pos, labels = df_data$label) +
  scale_x_continuous(breaks = seq(-100, 80, by = 20),
                     limits = c(-100, 80),
                     expand = c(0.02, 0)) +
  # 颜色填充与边框：显著负=蓝色填充，不显著=白色填充，边框均为黑色
  scale_fill_manual(values = c("negative" = "#1f77b4", "non-signif" = "white")) +
  scale_color_manual(values = c("negative" = "#1f77b4", "non-signif" = "black")) +
  # 标签
  labs(x = "Changes in percentage (%)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    # 移除部分默认网格线，保留我们自定义的水平线
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_line(color = "black"),
    axis.text.y = element_text(size = 10, hjust = 1, margin = margin(r = 5)),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 8)),
    plot.margin = margin(10, 15, 10, 20),
    # 背景与边框
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  # 隐藏图例（因为图中颜色含义明显，且原图无图例）
  guides(fill = "none", color = "none") +
  # 扩展左边距，防止左侧文字被裁剪
  coord_cartesian(xlim = c(-100, 80), ylim = c(420, 10), clip = "off")

# 显示图形
print(p)

# ------------------------------
# 3. 保存图片（可根据需要调整尺寸）
# ------------------------------
ggsave("Figure_3a_reproduced.pdf", p, width = 7, height = 9, dpi = 300)
ggsave("Figure_3a_reproduced.png", p, width = 7, height = 9, dpi = 300)