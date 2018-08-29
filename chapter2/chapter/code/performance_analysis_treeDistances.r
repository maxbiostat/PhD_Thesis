Files <- system("ls *.tmlog", intern = TRUE)
Logs <- lapply(Files, read.table, header = TRUE)
names(Logs) <- Files

burnin <- .5
PLogs <- lapply(Logs, function(x) x[ round(burnin * nrow(x)):nrow(x), ])


process_names <- function(x){
  Split <- strsplit(x, "_")
  return(
    data.frame(dataset = Split[[1]][5],
               seed = Split[[1]][[4]],
               operator = gsub(".trees.tmlog", "", Split[[1]][6])
    )
  )
}
forPlot <- data.table::rbindlist(
  lapply(seq_along(Logs), function(i) {
    data.frame(Logs[[i]], process_names(names(Logs)[i])) 
  })
)
forAnalysis <- data.table::melt(forPlot, id = c("state", "dataset", "seed", "operator"), variable.name = "metric")

getQuants <- function(x, alpha = .95){
  c(
    lwr = quantile(x, probs = (1-alpha)/2),
    mean = mean(x),
    upr =  quantile(x, probs = (1 + alpha)/2)  
  )
} 

DistancesOperators <- aggregate(value ~ dataset + metric + seed + operator, data = forAnalysis, getQuants)
trueDists <- read.csv("/home/max/DUMP/GOLDEN/distToTrue/golden_distanceTrueTree.csv")
names(trueDists) <- c("dataset",  "metric", "true.value.lwr.2.5.", "true.value.mean", "true.value.upr.97.5.")
subset(trueDists, dataset == "Dengue4")

library(ggplot2)
library(viridis)

number_ticks <- function(n) { function(limits) pretty(limits, n) }

PlotStuff <- data.frame(as.matrix(DistancesOperators))
PlotStuff$value.lwr.2.5. <- as.numeric(as.character(PlotStuff$value.lwr.2.5.))
PlotStuff$value.mean <- as.numeric(as.character(PlotStuff$value.mean))
PlotStuff$value.upr.97.5. <- as.numeric(as.character(PlotStuff$value.upr.97.5.))

ggplot(subset(PlotStuff, dataset == "Dengue4"),
       aes(x = operator, y = value.mean, colour = operator, fill = operator)) + 
  geom_boxplot(alpha = .5) +
  scale_x_discrete("", expand = c(0, 0)) +
  scale_y_continuous("Average distance to golden tree",  breaks = number_ticks(5) ) +
  geom_hline(data = subset(trueDists, dataset %in% c("Dengue4")),
             aes(yintercept = true.value.mean), linetype = "longdash") + 
  facet_wrap(~metric, scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Dengue 4 env (17 taxa)") +
  theme_bw()

ggplot(subset(PlotStuff, dataset == "denv2env"),
       aes(x = operator, y = value.mean, colour = operator, fill = operator)) + 
  geom_boxplot(alpha = .5) +
  scale_x_discrete("", expand = c(0, 0)) +
  scale_y_continuous("Average distance to golden tree",  breaks = number_ticks(5) ) +
  geom_hline(data = subset(trueDists, dataset %in% c("denv2env")),
             aes(yintercept = true.value.mean), linetype = "longdash") + 
  facet_wrap(~metric, scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Dengue 2 env (90 taxa)") +
  theme_bw()

ggplot(subset(PlotStuff, dataset == "Dengue4"),
       aes(x = operator, y = value.lwr.2.5., colour = operator, fill = operator)) + 
  geom_boxplot(alpha = .5) +
  scale_x_discrete("", expand = c(0, 0)) +
  scale_y_continuous("Distance to golden tree (quantile 2.5%)",  breaks = number_ticks(5) ) +
  geom_hline(data = subset(trueDists, dataset %in% c("Dengue4")),
             aes(yintercept = true.value.lwr.2.5.), linetype = "longdash") + 
  facet_wrap(~metric, scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Dengue 4 env (17 taxa)") +
  theme_bw()

ggplot(subset(PlotStuff, dataset == "Dengue4"),
       aes(x = operator, y = value.upr.97.5., colour = operator, fill = operator)) + 
  geom_boxplot(alpha = .5) +
  scale_x_discrete("", expand = c(0, 0)) +
  scale_y_continuous("Distance to golden tree (quantile 97.5%)",  breaks = number_ticks(5) ) +
  geom_hline(data = subset(trueDists, dataset %in% c("Dengue4")),
             aes(yintercept = true.value.upr.97.5.), linetype = "longdash") + 
  facet_wrap(~metric, scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Dengue 4 env (17 taxa)") +
  theme_bw()

ggplot(subset(PlotStuff, dataset == "denv2env"),
       aes(x = operator, y = value.lwr.2.5., colour = operator, fill = operator)) + 
  geom_boxplot(alpha = .5) +
  scale_x_discrete("", expand = c(0, 0)) +
  scale_y_continuous("Distance to golden tree (quantile 2.5%)",  breaks = number_ticks(5) ) +
  geom_hline(data = subset(trueDists, dataset %in% c("denv2env")),
             aes(yintercept = true.value.lwr.2.5.), linetype = "longdash") + 
  facet_wrap(~metric, scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Dengue 2 env (90 taxa)") +
  theme_bw()

ggplot(subset(PlotStuff, dataset == "denv2env"),
       aes(x = operator, y = value.upr.97.5., colour = operator, fill = operator)) + 
  geom_boxplot(alpha = .5) +
  scale_x_discrete("", expand = c(0, 0)) +
  scale_y_continuous("Distance to golden tree (quantile 97.5%)",  breaks = number_ticks(5) ) +
  geom_hline(data = subset(trueDists, dataset %in% c("denv2env")),
             aes(yintercept = true.value.upr.97.5.), linetype = "longdash") + 
  facet_wrap(~metric, scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Dengue 2 env (90 taxa)") +
  theme_bw()

forError <- merge(PlotStuff, trueDists, by = c("dataset", "metric"))
forError$error_sq_mean_average <- (forError$value.mean-forError$true.value.mean)^2
forError$error_sq_mean_lwr <- (forError$value.lwr.2.5.-forError$true.value.lwr.2.5.)^2
forError$error_sq_mean_upr <- (forError$value.upr.97.5.-forError$true.value.upr.97.5.)^2

RSMSE.average <- aggregate(error_sq_mean_average ~ dataset + metric + operator, data = forError, function(x) sqrt(mean(x)))
RSMSE.lwr <- aggregate(error_sq_mean_lwr ~ dataset + metric + operator, data = forError, function(x) sqrt(mean(x)))
RSMSE.upr <- aggregate(error_sq_mean_upr ~ dataset + metric + operator, data = forError, function(x) sqrt(mean(x)))

write.csv(RSMSE.average, file = "root_mean_squared_error_average_operators.csv", row.names = FALSE)
write.csv(RSMSE.lwr, file = "root_mean_squared_error_lower_operators.csv", row.names = FALSE)
write.csv(RSMSE.upr, file = "root_mean_squared_error_upper_operators.csv", row.names = FALSE)


ErrorList.average <- split(RSMSE.average, f = RSMSE.average$metric)
lapply(ErrorList.average, function(x) subset(x, dataset == "Dengue4") )
lapply(ErrorList.average, function(x) subset(x, dataset == "denv2env") )

ErrorList.lwr <- split(RSMSE.lwr, f = RSMSE.lwr$metric)
lapply(ErrorList.lwr, function(x) subset(x, dataset == "Dengue4") )
lapply(ErrorList.lwr, function(x) subset(x, dataset == "denv2env") )

ErrorList.upr <- split(RSMSE.upr, f = RSMSE.upr$metric)
lapply(ErrorList.upr, function(x) subset(x, dataset == "Dengue4") )
lapply(ErrorList.upr, function(x) subset(x, dataset == "denv2env") )

ESS.calcs <- parallel::mclapply(PLogs, function(x) apply(x, 2, coda::effectiveSize), mc.cores = 12) 
ESS.plain <-  data.table::melt(
    data.table::rbindlist(
      lapply(seq_along(ESS.calcs), function(i) {
        dt <- data.frame(matrix(ESS.calcs[[i]], nrow = 1))
        names(dt) <- names(ESS.calcs[[i]])
        return(
          data.frame(dt[, -1], ## excludes 'state'
                     process_names(names(ESS.calcs)[i]))
        )
      })
    ),
    id = c("dataset", "seed", "operator"), variable.name = "metric")

ggplot(ESS.plain,
       aes(x = operator, y = value, colour = operator, fill = operator)) + 
  geom_boxplot(alpha = .5) +
  scale_x_discrete("", expand = c(0, 0)) +
  scale_y_continuous("Effective sample size of distance to golden",  breaks = number_ticks(5) ) +
  geom_hline(yintercept = 200, linetype = "dotted") + 
  facet_grid(dataset~metric, scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(axis.title.x  = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 