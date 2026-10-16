# Run from the portfolio root: Rscript weekly-notes/assets/generate-week7-figures.R
# Exact five-house teaching values from week7_slides.html, not empirical sales.
library(ggplot2)
houses <- data.frame(house = factor(LETTERS[1:5], levels = LETTERS[1:5]),
                     residual = c(100, 80, 20, -50, -70))
z <- houses$residual - mean(houses$residual)
B <- outer(1:5, 1:5, function(i, j) as.numeric(abs(i - j) == 1))
W <- B / rowSums(B)
houses$z <- z
houses$lag_z <- drop(W %*% z)
houses$direction <- ifelse(houses$residual > 0, "Underprediction", "Overprediction")
I_B <- 5 / sum(B) * sum(z * (B %*% z)) / sum(z^2)
I_W <- sum(z * (W %*% z)) / sum(z^2)
stopifnot(abs(I_B - 0.6023123909) < 1e-9,
          abs(I_W - 0.7229493892) < 1e-9)
theme_set(theme_minimal(base_size = 14) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", size = 19),
                  plot.subtitle = element_text(size = 12),
                  plot.caption = element_text(hjust = 0, size = 11),
                  plot.margin = margin(16, 20, 16, 16)))
palette <- c(Underprediction = "#b2182b", Overprediction = "#2166ac")
p1 <- ggplot(houses, aes(house, residual)) +
  geom_hline(yintercept = 0, color = "#444444", linewidth = 0.6) +
  geom_col(aes(fill = direction), width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 16, linetype = "dashed", color = "#555555") +
  geom_text(aes(y = residual + ifelse(residual > 0, 9, -9),
                label = sprintf("%+d", residual)), size = 5) +
  annotate("text", x = 4.5, y = 23, label = "Mean = +16", size = 4) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(limits = c(-100, 125), breaks = seq(-100, 100, 50)) +
  labs(title = "Five neighboring houses and their residuals",
       subtitle = "Teaching example from the lecture; houses form the chain A–B–C–D–E",
       x = "House in block order", y = "Actual minus predicted price ($1,000)",
       caption = "Positive = underprediction; negative = overprediction. This is not a geographic map.")
ggsave("weekly-notes/assets/week7-house-residuals.png", p1,
       width = 9.6, height = 5.7, dpi = 160, bg = "white")
p2 <- ggplot(houses, aes(z, lag_z)) +
  geom_hline(yintercept = 0, color = "#999999", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "#999999", linewidth = 0.5) +
  geom_abline(slope = I_W, intercept = coef(lm(lag_z ~ z, houses))[1],
              color = "#444444", linewidth = 0.8) +
  geom_point(aes(color = direction), size = 3.5, show.legend = FALSE) +
  geom_text(aes(label = house), nudge_y = 9, size = 5) +
  scale_color_manual(values = palette) +
  coord_cartesian(xlim = c(-110, 110), ylim = c(-90, 90)) +
  labs(title = "A centered Moran scatterplot",
       subtitle = sprintf("Same five houses, row-standardized W: slope = Moran's I = %.4f", I_W),
       x = "My residual minus mean ($1,000)",
       y = "Neighbors' mean centered residual ($1,000)",
       caption = sprintf("Each row sums to 1. Binary weights give I = %.4f instead; do not mix the two calculations.", I_B))
ggsave("weekly-notes/assets/week7-moran-scatter.png", p2,
       width = 9.6, height = 6.2, dpi = 160, bg = "white")
print(c(I_binary = I_B, I_row_standardized = I_W))
