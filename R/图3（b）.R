# 加载必要的包
library(ggplot2)
library(dplyr)

# ------------------------------
# 1. 数据构建（基于 Excel 提取及原图信息）
# ------------------------------

df_data_b <- data.frame(
  subgroup = c("Woody litter", "Root", "Leaf", "Grass", "Straw", "Dung",
               "Surface", "Buried"),
  # y 轴位置（来自 Excel C 列，已按顺序从上到下）
  y_pos = c(359.684, 319.857, 279.595, 240.118, 200.204, 159.681,
            80.2896, 40.0276),
  # 均值（来自 Excel B 列每组中间值）
  mean = c(-43.2966, -15.6299, -32.3377, -44.0153, -24.433, -52.8183,
           -43.2966, -21.5585),
  # 95% CI 下限（每组第一个值）
  lower = c(-54.2556, -72.4006, -55.6928, -63.5976, -65.2145, -71.682,
            -52.8183, -52.0997),
  # 95% CI 上限（每组第三个值）
  upper = c(-25.1516, 174.983, 6.10824, -11.3182, 72.4006, -14.1927,
            -29.4633, 30.1819),
  # 观测数（来自原图说明）
  N = c(118, 4, 26, 9, 11, 19, 169, 18),
  # 显著性标记（* P<0.05, ** P<0.01, *** P<0.001）
  sig = c("***", "", "", "*", "", "*", "***", ""),
  # 效应类型：显著负为 negative，不显著为 non-signif
  effect_type = c("negative", "non-signif", "non-signif", "negative",
                  "non-signif", "negative", "negative", "non-signif")
)

# 添加显示标签（含样本量）
df_data_b$label <- paste0(df_data_b$subgroup, " (", df_data_b$N, ")")

# 确保顺序从上到下（y_pos 递减）
df_data_b <- df_data_b %>% arrange(desc(y_pos))

# 左侧分组标题的坐标
group_annotations_b <- data.frame(
  label = c("Substrate", "Environment"),
  y = c(260, 60),   # 两组中心位置
  x = -95
)

# ------------------------------
# 2. 绘图
# ------------------------------

p_b <- ggplot(df_data_b, aes(y = y_pos)) +
  # 水平误差线（95% CI）
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 1.2, color = "black", linewidth = 0.6) +
  # 均值点：显著负效应为蓝色实心圆，不显著为空心圆（黑边）
  geom_point(aes(x = mean, fill = effect_type, color = effect_type),
             shape = 21, size = 3.5, stroke = 0.8) +
  # 显著性星号（位于均值点右侧偏移 3 个单位）
  geom_text(aes(x = mean + 3, label = sig),
            hjust = 0, vjust = 0.5, size = 4, color = "black") +
  # 垂直参考线 x = 0
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) +
  # 水平辅助网格线（灰色浅线）
  geom_hline(yintercept = df_data_b$y_pos, color = "gray90", linewidth = 0.4) +
  # 左侧分组标题（英文文字）
  annotate("text", x = group_annotations_b$x, y = group_annotations_b$y,
           label = group_annotations_b$label, hjust = 0, vjust = 0.5,
           fontface = "bold", size = 4.5, color = "black") +
  # 坐标轴与主题设置
  scale_y_reverse(breaks = df_data_b$y_pos, labels = df_data_b$label) +
  scale_x_continuous(breaks = seq(-80, 180, by = 40),
                     limits = c(-80, 180),
                     expand = c(0.02, 0)) +
  # 颜色映射：显著负=蓝色填充，不显著=白色填充，边框均为黑色
  scale_fill_manual(values = c("negative" = "#1f77b4", "non-signif" = "white")) +
  scale_color_manual(values = c("negative" = "#1f77b4", "non-signif" = "black")) +
  # 标签
  labs(x = "Changes in percentage (%)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_line(color = "black"),
    axis.text.y = element_text(size = 10, hjust = 1, margin = margin(r = 5)),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 8)),
    plot.margin = margin(10, 15, 10, 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(fill = "none", color = "none") +
  coord_cartesian(xlim = c(-80, 180), ylim = c(380, 20), clip = "off")

# 显示图形
print(p_b)

# ------------------------------
# 3. 保存图片（尺寸可根据需要调整）
# ------------------------------
ggsave("Figure_3b_reproduced.pdf", p_b, width = 7, height = 6.5, dpi = 600)
ggsave("Figure_3b_reproduced.png", p_b, width = 7, height = 6.5, dpi = 600)