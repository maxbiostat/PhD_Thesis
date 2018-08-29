Files <- system("ls *.tmlog", intern = TRUE)
Logs <- lapply(Files, read.table, header = TRUE)
names(Logs) <- Files

trueDists <- read.csv("/home/max/DUMP/GOLDEN/distToTrue/golden_distanceTrueTree.csv")
names(trueDists) <- c("dataset",  "metric", "true.value.lwr.2.5.", "true.value.mean", "true.value.upr.97.5.")

process_names <- function(x){
  Split <- strsplit(x, "_")
  return(
    data.frame(dataset = Split[[1]][5],
               seed = Split[[1]][[4]],
               operator = gsub(".trees.tmlog", "", Split[[1]][6])
    )
  )
}
optIter <- function(y, ll, ul, max.it = .5){
  maxIter <- round(max.it * length(y))
    Out <- 0
    for(i in seq_along(y)){
      Out <- Out + 1
      score <- as.numeric(y[i] >= ll) + as.numeric(y[i] < ul)
      if(i >= maxIter){
        break
      }
      if(score == 2){
        break
      }
    }
    Out <- Out/length(y)
  return(Out)
}
#
Metrics <- sort(unique(trueDists$metric))
AllInflections <- function(dt, name){
  res <- sapply(Metrics, function(m){
   optIter(y = dt[, paste(m)],
                          ll = subset(trueDists, metric ==  m & dataset == name)$true.value.lwr.2.5.,
                          ul = subset(trueDists, metric ==  m & dataset == name)$true.value.upr.97.5.)
  })
  res <- data.frame(matrix(res, nrow = 1, ncol = length(Metrics)))
  names(res) <- Metrics
  return(res)
}
#
Inflections <- lapply(seq_along(Logs), function(i) {
  Info <- process_names(names(Logs)[i])
  Points <- AllInflections(dt = Logs[[i]], name = paste(Info$dataset))
  return(
    data.frame(Points, Info)
  )
})
#
InflectionData <- data.table::melt(data = data.table::rbindlist(Inflections),
                                   id.vars = c("dataset", "seed", "operator"),
                                   variable.name = "metric", value.name = "fraction")

library(ggplot2)
library(viridis)
number_ticks <- function(n) { function(limits) pretty(limits, n) }

ggplot(InflectionData,
       aes(x = operator, y = fraction, colour = operator, fill = operator)) + 
  geom_boxplot(alpha = .5) +
  scale_x_discrete("", expand = c(0, 0)) +
  scale_y_continuous("Fraction of the chain until typical set (p_t)",
                     breaks = number_ticks(5), expand = c(0, 0), limits = c(0, .1) ) +
  facet_grid(dataset~metric, scales = "free") +
  geom_hline(yintercept = .01, linetype = "dotted") + 
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw()