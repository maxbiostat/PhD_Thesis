source("~/DUMP/PIPELINE/code/BEAST_convergence_pipeline.r")

LT  <- getLTs(path = "")

RF <- lapply(LT[grep("RF", names(LT))], make_full)
KC0 <- lapply(LT[grep("KC0", names(LT))], make_full)

process_names <- function(x){
  Split <- strsplit(x, "_")
  return(
    data.frame(dataset = Split[[1]][4],
               seed = Split[[1]][3],
               dates = gsub(".csv", "", Split[[1]][5]),
               sample = ifelse(is.na(Split[[1]][6]), "posterior", gsub(".csv", "", Split[[1]][6]))
    )
  )
}

variation.RF <- data.frame(data.table::rbindlist(lapply(names(RF), process_names)),
           rf_mean = unlist(lapply(RF, mean)), rf_sd = unlist(lapply(RF, sd)))
rownames(variation.RF) <- NULL
variation.RF$upr <- variation.RF$rf_mean + qnorm(.975) * variation.RF$rf_sd
variation.RF$lwr <- variation.RF$rf_mean - qnorm(.975) * variation.RF$rf_sd

variation.KC0 <- data.frame(data.table::rbindlist(lapply(names(RF), process_names)),
           kc_mean = unlist(lapply(KC0, mean)), kc_sd = unlist(lapply(KC0, sd)))
rownames(variation.KC0) <- NULL
variation.KC0$upr <- variation.KC0$kc_mean + qnorm(.975) * variation.KC0$kc_sd
variation.KC0$lwr <- variation.KC0$kc_mean - qnorm(.975) * variation.KC0$kc_sd

number_ticks <- function(n) {function(limits) pretty(limits, n)}
ggplot(variation.RF,
       aes(x = sample, y = rf_mean, colour = dates),  position = "dodge") + 
  geom_point(alpha = .5) +
  geom_linerange(aes(ymin = upr, ymax = lwr)) +
  scale_y_continuous("Distance", expand = c(0, 0), breaks = number_ticks(10) ) +
  facet_grid(.~dataset, scales = "free") +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  ggtitle("Robinson-Foulds") + 
  theme_bw()

ggplot(variation.KC0,
       aes(x = sample, y = kc_mean, colour = dates),  position = "dodge") + 
  geom_point(alpha = .5) +
  geom_linerange(aes(ymin = upr, ymax = lwr)) +
  scale_y_continuous("Distance", expand = c(0, 0), breaks = number_ticks(10) ) +
  facet_grid(.~dataset, scales = "free") +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  ggtitle("Kendall-Colijn, lambda = 0") + 
  theme_bw()

variation.RF$metric <- "RF"
variation.RF$mean_dist <- variation.RF$rf_mean

variation.KC0$metric <- "KC"
variation.KC0$mean_dist <- variation.KC0$kc_mean

variation.all <-  data.frame(
  rbind(variation.RF[, c("mean_dist", "lwr", "upr", "metric", "dates", "dataset", "sample")],
        variation.KC0[, c("mean_dist", "lwr", "upr", "metric", "dates", "dataset", "sample")]))


ggplot(variation.all ,
       aes(x = sample, y = mean_dist, colour = dates),  position = "dodge") + 
  geom_point(alpha = .5) +
  geom_linerange(aes(ymin = upr, ymax = lwr)) +
  scale_y_continuous("Distance", expand = c(0, 0), breaks = number_ticks(10) ) +
  facet_grid(metric~dataset, scales = "free") +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  theme_bw()

# RF.procrustes <- parallel::mclapply(seq_along(RF), function(i){
#       Name <- names(RF)[i]
#       Pos <- grep(strsplit(Name, "_")[[1]][4], names(RF))
#       subList <- RF[Pos]
#       return(
#         vegan::procrustes(X = subList[[1]], Y = RF[[i]], scale = FALSE)$Yrot
#       )
#     },
#     mc.cores = 5)
# 
# KC0.procrustes <- parallel::mclapply(seq_along(KC0), function(i){
#   Name <- names(KC0)[i]
#   Pos <- grep(strsplit(Name, "_")[[1]][4], names(KC0))
#   subList <- KC0[Pos]
#   return(
#     vegan::procrustes(X = subList[[1]], Y = KC0[[i]], scale = FALSE)$Yrot
#   )
# },
# mc.cores = 5)

MDS.RF <- parallel::mclapply(RF, getMDS, step_size = 10000, mc.cores = 10) ## RF.procrustes
names(MDS.RF) <- names(RF)

MDS.KC0 <- parallel::mclapply(KC0, getMDS, step_size = 10000, mc.cores = 10)  ## KC0.procrustes
names(MDS.KC0) <- names(KC0)

complete.RF <- data.table::rbindlist(
  lapply(seq_along(MDS.RF), function(i){
    data.frame(MDS.RF[[i]], process_names(names(MDS.RF)[i]))
  })
)
complete.RF$metric <- "RF"
complete.RF$dataset <- gsub("50taxa", "", complete.RF$dataset)

complete.KC0 <- data.table::rbindlist(
  lapply(seq_along(MDS.KC0), function(i){
    data.frame(MDS.KC0[[i]], process_names(names(MDS.KC0)[i]))
  })
)
complete.KC0$metric <- "KC0"
complete.KC0$dataset <- gsub("50taxa", "", complete.KC0$dataset)

forPlot <- rbind(complete.RF, complete.KC0)

library(ggplot2)
library(viridis)

ggplot(complete.RF,
              aes(x = x, y = y, colour = dates, size = STATE)) + 
  geom_point(alpha = .5) +
  scale_x_continuous("First component", expand = c(0, 0),  breaks = number_ticks(10)) +
  scale_y_continuous("Second component", expand = c(0, 0), breaks = number_ticks(10) ) +
  facet_grid(dataset~sample) +
  scale_size_continuous(range = c(1, 4)) + 
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  # scale_color_viridis(discrete = TRUE) +
  # scale_fill_viridis(discrete = TRUE) +
  ggtitle("Robinson-Foulds") + 
  theme_bw()

ggplot(complete.KC0,
       aes(x = x, y = y, colour = dates, size = STATE, shape = seed)) + 
  geom_point(alpha = .5) +
  scale_x_continuous("First component", expand = c(0, 0), breaks = number_ticks(10)) +
  scale_y_continuous("Second component", expand = c(0, 0), breaks = number_ticks(10) ) +
  facet_grid(dataset~sample) +
  scale_size_continuous(range = c(1, 4)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  # scale_color_viridis(discrete = TRUE, option = "D") +
  # scale_fill_viridis(discrete = TRUE, option = "D") +
  ggtitle("Kendall-Colijn, lambda = 0") + 
  theme_bw()
