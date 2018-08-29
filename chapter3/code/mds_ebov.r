source("BEAST_convergence_pipeline.r")
folder <- "../data/EBOV_1610/combined/with1200/"
LT.list <- getLTs(folder) ## lower triangle matrices
Full.list <- lapply(LT.list, make_full)
getMDS_mod <- function(d, step_size = 5E4, ntrees = 200){
  ## specifically designed for EBOV experiment
  if (sum(d) == 0) {
    x <- rep(0, ncol(d))
    y <- rep(0, nrow(d))
    mds <- data.frame(x = x, y = y)
  }
  else {
    mds <- cmdscale(d, k = 2)
  }
  points <- as.data.frame(mds)
  J <- nrow(mds)/ntrees
  points <- data.frame(points,
                       STATE = seq(step_size, ntrees * step_size, by = step_size),
                       run = rep(paste0("Run_", 1:3), each = nrow(mds)/3),
                       operator =  rep(rep(c("default", "STL"), each = ntrees), J/2))
  row.names(points) <- seq(nrow(points))
  names(points) <- c("x", "y", "STATE", "run", "operator")
  return(points)
}
MDS <- lapply(Full.list, getMDS_mod)

Dt.RF <- MDS$RF_combined.csv
Dt.KC0 <- MDS$KC0_combined.csv
Dt.KChalf <- MDS$KChalf_combined.csv
Dt.BL <- MDS$BL_combined.csv
Dt.CD <- MDS$CD_combined.csv
Dt.SP <- MDS$SP_combined.csv


number_ticks <- function(n) {function(limits) pretty(limits, n)}

ggplot(Dt.RF, aes(x = x, y = y, symbol = operator, colour = run, size = STATE)) + 
  geom_point(alpha = .5) +
  scale_x_continuous("First component", expand = c(0, 0), limits = c(-800, 80), breaks = number_ticks(10)) +
  scale_y_continuous("Second component", expand = c(0, 0), breaks = number_ticks(10) ) +
  facet_grid(~operator) +
  ggtitle("Robinson-Foulds") + 
  theme_bw()

ggplot(Dt.KC0, aes(x = x, y = y, symbol = operator, colour = run, size = STATE)) + 
  geom_point(alpha = .5) +
  scale_x_continuous("First component", expand = c(0, 0), breaks = number_ticks(10)) +
  scale_y_continuous("Second component", expand = c(0, 0), breaks = number_ticks(10) ) +
  facet_grid(~operator) +
  ggtitle("Kendall-Colijn, lambda = 0") + 
  theme_bw()

ggplot(Dt.KChalf, aes(x = x, y = y, symbol = operator, colour = run, size = STATE)) + 
  geom_point(alpha = .5) +
  scale_x_continuous("First component", expand = c(0, 0), breaks = number_ticks(10)) +
  scale_y_continuous("Second component", expand = c(0, 0), breaks = number_ticks(10) ) +
  facet_grid(~operator) +
  ggtitle("Kendall-Colijn, lambda = 0.5") + 
  theme_bw()

ggplot(Dt.SP, aes(x = x, y = y, symbol = operator, colour = run, size = STATE)) + 
  geom_point(alpha = .5) +
  scale_x_continuous("First component", expand = c(0, 0), limits = c(-150, 150), breaks = number_ticks(10)) +
  scale_y_continuous("Second component", expand = c(0, 0), limits = c(-100, 200), breaks = number_ticks(10) ) +
  facet_grid(~operator) +
  ggtitle("Steel-Penny") + 
  theme_bw()

ggplot(Dt.CD, aes(x = x, y = y, symbol = operator, colour = run, size = STATE)) + 
  geom_point(alpha = .5) +
  scale_x_continuous("First component", expand = c(0, 0), limits = c(-20, 20),  breaks = number_ticks(10)) +
  scale_y_continuous("Second component", expand = c(0, 0), limits = c(-5, 10), breaks = number_ticks(10) ) +
  facet_grid(~operator) +
  ggtitle("Clade distance") + 
  theme_bw()

ggplot(Dt.BL, aes(x = x, y = y, symbol = operator, colour = run, size = STATE)) + 
  geom_point(alpha = .5) +
  scale_x_continuous("First component", expand = c(0, 0), limits = c(-4, 1), breaks = number_ticks(10)) +
  scale_y_continuous("Second component", expand = c(0, 0), limits = c(-4, 3), breaks = number_ticks(10) ) +
  facet_grid(~operator) +
  ggtitle("Branch length distance") + 
  theme_bw()