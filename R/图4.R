# 加载所需包
library(tidyverse)
library(patchwork)
library(grid)

# -------------------------- 1. 数据集（完全不变） --------------------------
# a面板：分解环境（Forests/Grassland/Surface/Buried）
data_a <- tibble(
  panel = "a",
  group = c(rep("Forests",7), rep("Grassland",7), rep("Surface",7), rep("Buried",7)),
  factor = rep(c("Evaporation","Duration","Elevation","Longitude","Latitude","MAP","MAT"),4),
  mean = c(0.8, 0.1, 0.0, -0.3, 1.2, 0.0, -2.3,
           1.5, 0.0, 0.0, -0.3, 0.7, 0.0, -1.5,
           1.1, 0.0, 0.0, -0.3, 0.8, 0.0, -2.2,
           0.5, 0.0, 0.0, -0.3, -1.0, 0.0, -0.8),
  lower = c(0.0, -0.3, -0.3, -0.6, 0.9, -0.3, -3.6,
            0.7, -0.3, -0.3, -0.6, 0.0, -0.3, -2.3,
            0.5, -0.3, -0.3, -0.6, 0.1, -0.3, -3.1,
            0.1, -0.4, -0.3, -0.6, -3.7, -0.3, -1.1),
  upper = c(1.5, 0.3, 0.4, 0.0, 1.7, 0.3, -0.9,
            2.0, 0.3, 0.3, 0.0, 2.1, 0.3, -0.7,
            1.7, 0.3, 0.3, 0.0, 1.4, 0.3, -1.3,
            0.8, 0.3, 0.3, 0.0, 1.7, 0.3, -0.0),
  n = c(123,123,123,123,123,43,51,51,51,51,51,51,49,51,169,169,169,169,169,87,97,18,18,18,18,18,17,18),
  sig = c("","","***","***","*","","**","**","","*","","","","**","***","","***","***","*","**","**","***","","","***","","***","*"),
  effect = c("ns","ns","negative","positive","negative","ns","positive","negative","ns","ns","ns","ns","ns","positive","negative","ns","negative","positive","negative","positive","positive","negative","ns","ns","positive","ns","positive","positive")
)

# b面板：基质（Woody litter/Leaf/Dung/Straw）
data_b <- tibble(
  panel = "b",
  group = c(rep("Woody litter",7), rep("Leaf",7), rep("Dung",7), rep("Straw",7)),
  factor = rep(c("Evaporation","Duration","Elevation","Longitude","Latitude","MAP","MAT"),4),
  mean = c(1.4, 0.8, 0.8, 0.4, 1.6, 0.8, -1.5,
           1.6, 0.4, 0.7, 0.5, -2.2, 0.7, -0.7,
           2.9, 0.8, 0.6, 1.2, -1.8, 0.7, -1.5,
           1.7, 0.5, 0.8, 0.3, -1.1, 0.8, -0.2),
  lower = c(0.7, 0.0, 0.1, -0.2, 0.9, 0.0, -2.9,
            0.8, -0.2, 0.0, -0.2, -10.9, 0.0, -1.5,
            -0.4, 0.0, -0.1, 0.1, -5.6, 0.0, -4.8,
            0.9, -0.2, 0.0, -0.5, -4.6, 0.0, -1.1),
  upper = c(2.3, 1.6, 1.5, 1.4, 2.4, 1.6, -0.0,
            2.4, 1.1, 1.6, 1.2, 7.2, 1.5, 0.0,
            6.2, 1.5, 1.3, 2.4, 2.3, 1.4, 1.7,
            2.3, 1.2, 1.4, 1.2, 2.7, 1.5, 0.6),
  n = c(118,118,118,118,118,38,46,26,26,26,26,26,26,26,19,19,19,19,17,17,19,11,11,11,11,11,10,11),
  sig = c("","***","***","*","","*","**","*","*","","","","","**","","","**","","","","","*","*","","","*","","*"),
  effect = c("ns","negative","positive","negative","negative","positive","positive","negative","positive","ns","ns","ns","ns","positive","ns","ns","positive","ns","ns","ns","ns","negative","positive","ns","ns","positive","ns","positive")
)

# c面板：研究方法（Mesh bag/Pesticide/Eye pick/Grass）
data_c <- tibble(
  panel = "c",
  group = c(rep("Mesh bag",7), rep("Pesticide",7), rep("Eye pick",7), rep("Grass",7)),
  factor = rep(c("Evaporation","Duration","Elevation","Longitude","Latitude","MAP","MAT"),4),
  mean = c(0.6, -0.4, -0.4, -0.9, 0.7, -0.6, -2.4,
           1.5, -0.5, -0.6, 0.1, -3.5, -0.5, -2.9,
           0.2, -0.5, -0.5, -0.5, -1.3, -0.5, -1.7,
           2.3, -0.5, -0.6, 0.3, -1.0, -0.6, -3.9),
  lower = c(-0.1, -1.1, -1.1, -1.5, 0.0, -1.1, -3.2,
            0.0, -1.1, -1.2, -0.6, -5.1, -1.1, -5.2,
            -0.5, -1.1, -1.1, -1.1, -1.9, -1.1, -2.5,
            -2.5, -1.1, -1.1, -1.1, -3.0, -1.2, -7.1),
  upper = c(1.4, 0.1, 0.1, 1.4, 1.4, 0.0, -1.8,
            3.1, 0.1, 0.0, 0.6, -1.7, 0.1, -0.6,
            0.9, 0.1, 0.0, 0.2, -0.8, 0.2, -0.9,
            7.4, 0.1, 0.0, 0.4, 1.2, 0.0, -0.4),
  n = c(136,136,136,136,136,67,67,37,37,37,37,34,34,34,14,14,14,14,14,14,14,9,9,9,9,9,9,9),
  sig = c("**","***","***","**","**","**","**","*","","","**","**","","","","","","","*","","","","","","","","",""),
  effect = c("negative","negative","positive","negative","negative","positive","positive","negative","ns","ns","positive","positive","ns","ns","ns","ns","ns","positive","positive","ns","positive","ns","ns","ns","ns","ns","ns","ns")
)

# 合并所有数据
data_all <- bind_rows(data_a, data_b, data_c) %>%
  mutate(
    factor = factor(factor, levels = c("Evaporation","Duration","Elevation","Longitude","Latitude","MAP","MAT")),
    group = factor(group, levels = c("Forests","Grassland","Surface","Buried","Woody litter","Leaf","Dung","Straw","Mesh bag","Pesticide","Eye pick","Grass")),
    y = as.numeric(factor) + (as.numeric(group)-1)*7
  )

# 颜色映射
color_map <- c("ns" = "white", "positive" = "red", "negative" = "blue")

# -------------------------- 2. 绘图函数（核心修改：1:4横纵比+保留紫虚线+X=0竖虚线） --------------------------
plot_forest <- function(data, panel_id, x_lim, x_breaks, title){
  d <- data %>% filter(panel == panel_id)
  group_y <- d %>% group_by(group) %>% summarise(y_min = min(y)-0.5, y_max = max(y)+0.5)
  
  ggplot(d) +
    # 1. 置信区间横线
    geom_segment(aes(x = lower, xend = upper, y = y, yend = y), color = "grey", linewidth = 1) +
    # 2. 均值点
    geom_point(aes(x = mean, y = y, fill = effect), shape = 21, size = 4, color = "black", stroke = 1) +
    # 3. 样本量+显著性标注
    geom_text(aes(x = x_lim[2]*0.95, y = y, label = paste0("(",n,")",sig)), hjust = 1, size = 3) +
    # ✅ 保留：分组紫色水平虚线
    geom_hline(data = group_y[-nrow(group_y),], aes(yintercept = y_max), color = "purple", linetype = "dashed", alpha = 0.7) +
    # ✅ 新增：X=0垂直黑色虚线
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.8) +
    # 4. 左侧分组名称
    geom_text(data = group_y, aes(x = x_lim[1]*0.95, y = (y_min+y_max)/2, label = group), 
              hjust = 1, size = 4, fontface = "bold", color = "darkgreen") +
    # 坐标轴
    scale_y_continuous(breaks = d$y, labels = d$factor, expand = expansion(add = 2)) +
    scale_x_continuous(limits = x_lim, breaks = x_breaks, expand = expansion(add = 0.2)) +
    scale_fill_manual(values = color_map) +
    # 主题+✅横纵比1:4（宽:高=1:4）
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0, size = 14, face = "bold"),
      axis.title.x = element_text(size = 12), axis.title.y = element_blank(),
      axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
      legend.position = "none", panel.grid = element_blank(),
      aspect.ratio = 4  # 核心：宽:高=1:4
    ) +
    labs(title = title, x = "Changes in percentage (%)")
}

# -------------------------- 3. 绘图+拼接+保存（完全不变） --------------------------
p_a <- plot_forest(data_all, "a", x_lim = c(-5, 3), x_breaks = seq(-5, 3, 1), title = "(a)")
p_b <- plot_forest(data_all, "b", x_lim = c(-12, 8), x_breaks = seq(-12, 8, 4), title = "(b)")
p_c <- plot_forest(data_all, "c", x_lim = c(-6, 8), x_breaks = seq(-6, 8, 2), title = "(c)")

# 三面板等宽对齐
final_plot <- p_a + p_b + p_c + plot_layout(nrow = 1, widths = c(1,1,1))

# 导出高清图片（宽度18cm不变，高度按1:4适配）
ggsave("图4_最终版.png", final_plot, width = 18, height = 72, dpi = 1200, units = "cm")
print(final_plot)