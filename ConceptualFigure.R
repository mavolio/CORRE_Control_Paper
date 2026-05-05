# ============================================================
# Conceptual figure: ambient change vs. GCD-driven change
# ============================================================

# Install if needed
# install.packages(c("ggplot2", "patchwork", "grid", "ggforce", "dplyr"))
install.packages("scales")
install.packages("ggplot2")

# then restart R / RStudio completely
# Session > Restart R

library(ggplot2)
library(ggplot2)
library(patchwork)
library(grid)
library(ggforce)
library(dplyr)

# ----------------------------
# Colors
# ----------------------------
col_start <- "grey35"
col_ambient <- "#2B7BBB"
col_gcd <- "#D9471F"
col_low <- "#DCEBFA"
col_mid <- "#E7F2DC"
col_high <- "#FCE5D8"

# ----------------------------
# Helper function for community panel
# ----------------------------
make_panel <- function(title, subtitle, ambient_arrow_size, gcd_arrow_size,
                       fill_col, border_col, note, note_bold_col) {
  
  circles <- data.frame(
    x = c(1, 3.3, 5.6),
    y = c(2.4, 2.4, 2.4),
    lab = c("O", "A", "C"),
    sub = c("Starting\ncommunity", "After ambient\nchange", "After GCD\nchange"),
    border = c(col_start, col_ambient, col_gcd)
  )
  
  plants <- expand.grid(
    circle = 1:3,
    stem = 1:9
  ) %>%
    mutate(
      x_center = rep(c(1, 3.3, 5.6), each = 9),
      x = x_center + rep(seq(-0.45, 0.45, length.out = 9), 3),
      y0 = 1.95,
      y1 = y0 + rep(c(0.45, 0.75, 0.55, 0.9, 0.65, 0.5, 0.8, 0.6, 0.7), 3),
      flower = rep(c("●", "", "✦", "", "●", "", "✦", "", ""), 3),
      flower_col = rep(c("#7B3294", "#4D9221", "#E6AB02", "#4D9221",
                         "#D9471F", "#4D9221", "#E6AB02", "#4D9221", "#4D9221"), 3)
    )
  
  ggplot() +
    # panel background
    annotate("rect", xmin = 0, xmax = 6.6, ymin = 0, ymax = 4.5,
             fill = fill_col, color = border_col, linewidth = 0.8, alpha = 0.45) +
    
    # title
    annotate("text", x = 3.3, y = 4.18, label = title,
             fontface = "bold", size = 5.2, color = border_col) +
    annotate("text", x = 3.3, y = 3.92, label = subtitle,
             fontface = "italic", size = 3.8, color = border_col) +
    
    # circles
    geom_circle(data = circles,
                aes(x0 = x, y0 = y, r = 0.62),
                fill = "white", color = circles$border, linewidth = 1.1) +
    
    # plant stems
    geom_segment(data = plants,
                 aes(x = x, xend = x + 0.06, y = y0, yend = y1),
                 color = "#3A6B22", linewidth = 0.8) +
    geom_text(data = plants %>% filter(flower != ""),
              aes(x = x + 0.06, y = y1 + 0.05, label = flower, color = flower_col),
              size = 4, show.legend = FALSE) +
    scale_color_identity() +
    
    # arrows
    annotate("segment", x = 1.75, xend = 2.55, y = 2.4, yend = 2.4,
             arrow = arrow(length = unit(0.22, "inches"), type = "closed"),
             linewidth = ambient_arrow_size, color = col_ambient) +
    annotate("segment", x = 4.05, xend = 4.85, y = 2.4, yend = 2.4,
             arrow = arrow(length = unit(0.22, "inches"), type = "closed"),
             linewidth = gcd_arrow_size, color = col_gcd) +
    
    # arrow labels
    annotate("text", x = 2.15, y = 3.1, label = "Ambient change\n(background)",
             size = 3.2, color = col_ambient, fontface = "bold") +
    annotate("text", x = 4.45, y = 3.1, label = "GCD-driven\nchange",
             size = 3.2, color = col_gcd, fontface = "bold") +
    
    # O A C labels
    geom_text(data = circles, aes(x = x, y = 1.45, label = lab),
              fontface = "bold", size = 5, color = circles$border) +
    geom_text(data = circles, aes(x = x, y = 1.08, label = sub),
              size = 3.4, lineheight = 0.9) +
    
    # note box
    annotate("rect", xmin = 0.22, xmax = 6.38, ymin = 0.15, ymax = 0.72,
             fill = "white", color = border_col, alpha = 0.75, linewidth = 0.6) +
    annotate("text", x = 3.3, y = 0.43, label = note,
             size = 3.4, color = "black", lineheight = 0.95) +
    
    coord_cartesian(xlim = c(0, 6.6), ylim = c(0, 4.5), clip = "off") +
    theme_void()
}

# ----------------------------
# Three top panels
# ----------------------------
p1 <- make_panel(
  title = "A. Low background change",
  subtitle = "Large GCD response despite low variability",
  ambient_arrow_size = 1.4,
  gcd_arrow_size = 3.5,
  fill_col = col_low,
  border_col = col_ambient,
  note = "Even with little background change,\nGCD-driven change can be large.",
  note_bold_col = col_ambient
)

p2 <- make_panel(
  title = "B. Intermediate background change",
  subtitle = "Responses increase with background change",
  ambient_arrow_size = 2.5,
  gcd_arrow_size = 2.8,
  fill_col = col_mid,
  border_col = "#5B8E2E",
  note = "GCD-driven change increases\nwith background change.",
  note_bold_col = "#5B8E2E"
)

p3 <- make_panel(
  title = "C. High background change",
  subtitle = "Less-than-proportional scaling",
  ambient_arrow_size = 4.2,
  gcd_arrow_size = 3.2,
  fill_col = col_high,
  border_col = col_gcd,
  note = "GCD-driven change continues to increase,\nbut less than proportionally.",
  note_bold_col = col_gcd
)

# ----------------------------
# Legend
# ----------------------------
legend_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 4.5, ymin = 0, ymax = 4.5,
           fill = "white", color = "grey80", linewidth = 0.7) +
  annotate("text", x = 2.25, y = 4.12, label = "Key",
           fontface = "bold", size = 5) +
  annotate("point", x = 0.65, y = 3.45, size = 9, shape = 21,
           fill = "white", color = col_start, stroke = 1.2) +
  annotate("text", x = 0.65, y = 3.45, label = "O", fontface = "bold", size = 4) +
  annotate("text", x = 1.2, y = 3.45, label = "Starting community",
           hjust = 0, size = 3.5) +
  annotate("point", x = 0.65, y = 2.75, size = 9, shape = 21,
           fill = "white", color = col_ambient, stroke = 1.2) +
  annotate("text", x = 0.65, y = 2.75, label = "A", fontface = "bold",
           color = col_ambient, size = 4) +
  annotate("text", x = 1.2, y = 2.75, label = "After ambient/background change",
           hjust = 0, size = 3.5) +
  annotate("point", x = 0.65, y = 2.05, size = 9, shape = 21,
           fill = "white", color = col_gcd, stroke = 1.2) +
  annotate("text", x = 0.65, y = 2.05, label = "C", fontface = "bold",
           color = col_gcd, size = 4) +
  annotate("text", x = 1.2, y = 2.05, label = "After GCD-driven change",
           hjust = 0, size = 3.5) +
  annotate("segment", x = 0.35, xend = 0.95, y = 1.25, yend = 1.25,
           arrow = arrow(length = unit(0.18, "inches"), type = "closed"),
           linewidth = 2.2, color = col_ambient) +
  annotate("text", x = 1.2, y = 1.25, label = "Ambient/background change",
           hjust = 0, size = 3.5) +
  annotate("segment", x = 0.35, xend = 0.95, y = 0.65, yend = 0.65,
           arrow = arrow(length = unit(0.18, "inches"), type = "closed"),
           linewidth = 2.8, color = col_gcd) +
  annotate("text", x = 1.2, y = 0.65, label = "GCD-driven change",
           hjust = 0, size = 3.5) +
  annotate("text", x = 0.35, y = 0.22,
           label = "Arrow width = magnitude of change",
           hjust = 0, size = 3.2) +
  coord_cartesian(xlim = c(0, 4.5), ylim = c(0, 4.5), clip = "off") +
  theme_void()

# ----------------------------
# Bottom relationship plot
# ----------------------------
curve_dat <- data.frame(
  x = seq(0, 1, length.out = 200)
) %>%
  mutate(y = 0.72 * (1 - exp(-2.6 * x)))

points_dat <- data.frame(
  x = c(0.18, 0.50, 0.82),
  y = 0.72 * (1 - exp(-2.6 * c(0.18, 0.50, 0.82))),
  group = c("Low", "Intermediate", "High"),
  col = c(col_ambient, "#8AB33F", col_gcd)
)

relationship_plot <- ggplot(curve_dat, aes(x, y)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey45", linewidth = 0.8) +
  geom_line(color = "#6A3D9A", linewidth = 1.4) +
  geom_segment(data = points_dat,
               aes(x = x, xend = x, y = 0, yend = y, color = col),
               linetype = "dashed", linewidth = 0.7, show.legend = FALSE) +
  geom_point(data = points_dat,
             aes(x = x, y = y, fill = col),
             shape = 21, size = 4.5, color = "black", stroke = 0.4,
             show.legend = FALSE) +
  scale_color_identity() +
  scale_fill_identity() +
  annotate("text", x = 0.78, y = 0.88, label = "1:1 proportional\nexpectation",
           size = 3.3, color = "grey30", hjust = 0) +
  annotate("text", x = 0.77, y = 0.34,
           label = "Observed pattern:\nresponses increase,\nbut not proportionally",
           size = 3.5, color = "#6A3D9A", hjust = 0) +
  labs(
    x = "Background change\n(year-to-year community change)",
    y = "GCD-driven change\n(treatment–control difference)"
  ) +
  coord_cartesian(xlim = c(0, 1.05), ylim = c(0, 1.05), clip = "off") +
  theme_classic(base_size = 12) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(8, 20, 8, 8)
  )

# ----------------------------
# Assemble final figure
# ----------------------------
top_row <- p1 + p2 + p3 + plot_layout(widths = c(1, 1, 1))

bottom_row <- legend_plot + relationship_plot + plot_layout(widths = c(0.8, 1.7))

final_fig <- top_row / bottom_row +
  plot_layout(heights = c(1.2, 0.9)) +
  plot_annotation(
    title = "Community responses to global change scale with background change, but not proportionally",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

# Show figure
final_fig

# ----------------------------
# Save figure
# ----------------------------
ggsave(
  filename = "conceptual_background_GCD_change_figure.png",
  plot = final_fig,
  width = 13,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "conceptual_background_GCD_change_figure.pdf",
  plot = final_fig,
  width = 13,
  height = 8
)