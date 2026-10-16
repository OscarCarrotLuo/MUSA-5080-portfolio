# Teaching illustrations for Weeks 5 and 6. All plotted observations are
# simulated; only the model-log values are transcribed from the supplied lecture.
# Run from the portfolio root: Rscript weekly-notes/assets/generate-study-figures.R
out <- "weekly-notes/assets"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
blue <- "#24658B"
orange <- "#C46528"
grey <- "#596774"
open_plot <- function(name, width = 1500, height = 650) {
  ragg::agg_png(file.path(out, name), width, height, res = 150, background = "white")
  par(family = "sans", col.axis = grey, col.lab = grey, fg = grey,
      mar = c(4.6, 4.8, 3.5, 1.2), mgp = c(2.8, .8, 0), las = 1)
}

set.seed(5080)
open_plot("week5-residuals.png", 1800, 700)
par(mfrow = c(1, 3), oma = c(2, 0, 0, 0))
x <- seq(30, 120, length.out = 22)
y <- 690 + 8.1 * x + rnorm(length(x), 0, 150)
m <- lm(y ~ x)
plot(x, y, pch = 19, col = blue, xlab = "Annual income ($1,000)",
     ylab = "Monthly rent ($)", main = "Residual = observed - fitted")
segments(x, fitted(m), x, y, col = "#9EBCCB")
abline(m, col = orange, lwd = 2)
legend("topleft", c("Observations", "Fitted line", "Vertical residuals"),
       col = c(blue, orange, "#9EBCCB"), pch = c(19, NA, NA),
       lty = c(NA, 1, 1), bty = "n", cex = .78)
x <- seq(0, 100, length.out = 180)
e1 <- rnorm(length(x), 0, 70)
e2 <- rnorm(length(x), 0, 20 + 1.8 * x)
plot(x, e1, pch = 19, col = adjustcolor(blue, .55), ylim = c(-450, 450),
     xlab = "Predictor", ylab = "Residual ($)", main = "Constant spread")
abline(h = 0, col = orange, lwd = 2)
plot(x, e2, pch = 19, col = adjustcolor(blue, .55), ylim = c(-450, 450),
     xlab = "Predictor", ylab = "Residual ($)", main = "Increasing spread: a fan")
abline(h = 0, col = orange, lwd = 2)
mtext("SIMULATED teaching examples; the fan suggests heteroskedasticity, not necessarily a biased slope.",
      outer = TRUE, side = 1, line = .3, cex = .82)
dev.off()

open_plot("week5-folds.png", 1300, 780)
par(mar = c(4, 5, 4, 2))
plot(NA, xlim = c(.5, 5.5), ylim = c(.5, 5.8), axes = FALSE,
     xlab = "Fold (20 observations each)", ylab = "Validation round",
     main = "Five-fold cross-validation: 100 observations")
axis(1, at = 1:5, labels = paste("Fold", 1:5))
axis(2, at = 1:5, labels = 5:1)
for (r in 1:5) for (f in 1:5) {
  yy <- 6 - r
  rect(f - .44, yy - .37, f + .44, yy + .37,
       col = if (f == r) orange else "#D8E8F0", border = "white")
  text(f, yy, if (f == r) "Validate" else "Train", cex = .9,
       col = if (f == r) "white" else blue)
}
text(3, 5.65, "Refit on 80 observations; predict the held-out 20. Repeat five times.", cex = .9)
dev.off()

open_plot("week6-transformations.png", 1500, 710)
par(mfrow = c(1, 2), oma = c(2, 0, 0, 0))
x <- seq(20000, 200000, length.out = 150)
y <- 1000 + 500 * log(x / 20000)
plot(x / 1000, y, type = "l", lwd = 3, col = blue,
     xlab = "Annual income ($1,000)", ylab = "Predicted monthly rent ($)",
     main = "Log X: equal ratios, equal changes")
points(c(20, 40, 80, 160), 1000 + 500 * log(c(1, 2, 4, 8)), pch = 19, col = orange)
legend("bottomright", "Each doubling adds 500 log(2), about $347", bty = "n", cex = .82)
age <- seq(5, 85, length.out = 200)
y <- 2300 - 28.0044512 * age + .1823244 * age^2
plot(age, y, type = "l", col = blue, lwd = 3, main = "Quadratic: slope changes with age",
     xlab = "Building age (years)", ylab = "Illustrative predicted rent ($)")
turn <- 28.0044512 / (2 * .1823244)
abline(v = turn, lty = 2, col = orange)
legend("topright", sprintf("Turn at %.1f years", turn), bty = "n", text.col = orange)
mtext("ILLUSTRATIONS, not observed Philadelphia data. Quadratic slopes use lecture coefficients; intercept is illustrative.",
      side = 1, outer = TRUE, line = .3, cex = .78)
dev.off()

open_plot("week6-groups.png", 1800, 700)
par(mfrow = c(1, 3), oma = c(2, 0, 0, 0))
x <- seq(0, 2, length.out = 100)
cols <- c(blue, orange, "#43865B")
plot(x, 1000 + 500*x, type = "n", ylim = c(700, 2300),
     xlab = "Centered log income", ylab = "Predicted rent ($)", main = "Category: shift the intercept")
for (g in 1:3) lines(x, 1000 - 120*(g-1) + 500*x, col = cols[g], lwd = 2.5)
legend("topleft", c("Core", "Inner", "Outer"), col = cols, lty = 1, bty = "n", cex = .8)
plot(x, 1000 + 500*x, type = "n", ylim = c(700, 2300),
     xlab = "Centered log income", ylab = "Predicted rent ($)", main = "Interaction: change the slope")
for (g in 1:3) lines(x, 1000 - 120*(g-1) + c(650, 430, 410)[g]*x, col = cols[g], lwd = 2.5)
legend("topleft", c("Core", "Inner", "Outer"), col = cols, lty = 1, bty = "n", cex = .8)
plot(x, 1000 + 500*x, type = "n", ylim = c(700, 2300),
     xlab = "Centered log income", ylab = "Predicted rent ($)", main = "District FE: group intercepts")
for (g in 1:3) lines(x, 750 + 200*(g-1) + 400*x, col = cols[g], lwd = 2.5)
legend("topleft", c("District A", "District B", "District C"), col = cols, lty = 1, bty = "n", cex = .8)
mtext("ILLUSTRATIVE coefficients. Fixed effects alone allow different intercepts; interactions allow different slopes.",
      side = 1, outer = TRUE, line = .3, cex = .82)
dev.off()

open_plot("week6-model-log.png", 1300, 820)
par(mar = c(5, 15, 4, 2))
labels <- c("m0: baseline", "m1: log income", "m2: log rent + log income",
            "m3: quadratic age", "m4: distance rings", "m5: income x ring",
            "m6: district FE (merged groups)")
values <- c(265, 265, 263, 261, 265, 264, 256)
plot(values, 7:1, xlim = c(252, 269), ylim = c(.5, 7.5), pch = 19, cex = 1.4,
     col = blue, yaxt = "n", ylab = "", xlab = "Out-of-fold RMSE ($ / month): lower is better",
     main = "Week 6 lecture model log")
axis(2, at = 7:1, labels = labels, tick = FALSE, cex.axis = .85)
segments(252, 7:1, values, 7:1, col = "#C7D7E2")
points(values, 7:1, pch = 19, col = blue, cex = 1.4)
text(values + .7, 7:1, values, pos = 4, cex = .85)
mtext("Rounded values transcribed from the supplied 9 Oct lecture HTML; not newly re-estimated.", side = 1, line = 3.5, cex = .78)
dev.off()
