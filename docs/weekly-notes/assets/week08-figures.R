# Reproducible teaching figures; run from the portfolio root.
# Source: supplied Week 8 slides (Delmelle, 2026-10-23).
# Figure 1 is the five-house example, NOT an empirical Philadelphia map.
out <- "weekly-notes/assets"
e <- c(A = 100, B = 80, C = 20, D = -50, E = -70)
W <- matrix(0, 5, 5, dimnames = list(names(e), names(e)))
W[cbind(1:4, 2:5)] <- W[cbind(2:5, 1:4)] <- 1
W <- W / rowSums(W)
z <- e - mean(e)
lag_z <- drop(W %*% z)
Ii <- z * lag_z / mean(z^2)
stopifnot(abs(sum(z^2) - 22920) < 1e-9,
          abs(mean(Ii) - 0.7229493892) < 1e-9)
png(file.path(out, "week08-local-moran.png"), 1500, 1050, res = 150)
par(mar = c(5, 5, 4, 1), family = "sans", col.axis = "#334155", col.lab = "#334155")
plot(z, lag_z, type = "n", xlim = c(-110, 110), ylim = c(-95, 95),
     xlab = "Own centered residual z (thousands of dollars)",
     ylab = "Neighbors' weighted mean centered residual Wz ($000)",
     main = "Local Moran: own deviation x neighbors' deviation")
rect(-110, 0, 0, 95, col = "#fff4e6", border = NA)
rect(0, 0, 110, 95, col = "#fce8e6", border = NA)
rect(-110, -95, 0, 0, col = "#e4effa", border = NA)
rect(0, -95, 110, 0, col = "#f2edf7", border = NA)
abline(h = 0, v = 0, col = "#64748b", lty = 2)
text(c(-58, 58, -58, 58), c(84, 84, -84, -84),
     c("LH: unlike neighbors", "HH: similar high values",
       "LL: similar low values", "HL: unlike neighbors"), cex = .87)
points(z, lag_z, pch = 21, bg = c("#b91c1c", "#b91c1c", "#7c3aed", "#17649b", "#17649b"), cex = 1.7)
text(z, lag_z, labels = paste0(names(e), ": I = ", sprintf("%.4f", Ii)),
     pos = c(2, 2, 1, 4, 4), cex = .9, offset = .7)
mtext("Reconstructed five-house example. Quadrant labels alone do NOT establish statistical significance.",
      side = 1, line = 3.7, cex = .75)
dev.off()

png(file.path(out, "week08-vacancy-check.png"), 1500, 1000, res = 150)
par(mar = c(5, 5, 5, 1), family = "sans")
counts <- matrix(c(536, 705, 169, 169), nrow = 2,
                 dimnames = list(c("Crime feature", "+ Vacancy rate"), c("Low-Low", "High-High")))
b <- barplot(counts, beside = TRUE, ylim = c(0, 850),
             col = c("#94a3b8", "#175b83"), border = NA,
             ylab = "Number of homes classified significant (raw p < 0.01)",
             main = "Lecture output: lower RMSE did not shrink Low-Low clusters")
text(b, counts + 28, labels = as.vector(counts), cex = 1.1)
legend("topright", legend = rownames(counts), fill = c("#94a3b8", "#175b83"), bty = "n")
mtext("CV RMSE: $432,287 -> $431,883   |   Global I: 0.260 -> 0.258", side = 3, line = .6, cex = .9)
mtext("Source: Week 8 lecture's reported comparison; not a new model run or reconstructed empirical map.",
      side = 1, line = 3, cex = .75)
dev.off()
