Files <- system("ls *.tmlog", intern = TRUE)
Logs <- lapply(Files, read.table, header = TRUE)
names(Logs) <- Files

process_names <- function(x){
  Split <- strsplit(x, "_")
  return(
    data.frame(dataset = Split[[1]][3],
               seed = gsub(".tmlog", "", Split[[1]][5])
    )
  )
}

forPlot <- data.table::rbindlist(
  lapply(seq_along(Logs), function(i) {
   data.frame(Logs[[i]], process_names(names(Logs)[i])) 
  })
)
forAnalysis <- data.table::melt(forPlot, id = c("state", "dataset", "seed"), variable.name = "metric")

library(ggplot2)
library(viridis)

number_ticks <- function(n) { function(limits) pretty(limits, n) }

rf<- ggplot(forPlot,
              aes(x = rf, colour = seed, fill = seed)) + 
  geom_density(alpha = .5) +
  scale_x_continuous("Distance to (golden) true tree", expand = c(0, 0), breaks = number_ticks(5)) +
  scale_y_continuous(expand = c(0, 0), breaks = number_ticks(5) ) +
  facet_wrap(~dataset, scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  guides(fill = FALSE, colour = FALSE) +
  ggtitle("Robinson-Foulds") + 
  theme_bw()
  
kc0 <- ggplot(forPlot,
       aes(x = kc.0.0., colour = seed, fill = seed)) + 
  geom_density(alpha = .5) +
  scale_x_continuous("Distance to (golden) true tree", expand = c(0, 0), breaks = number_ticks(5)) +
  scale_y_continuous(expand = c(0, 0), breaks = number_ticks(5) ) +
  facet_wrap(~dataset, scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  guides(fill = FALSE, colour = FALSE) +
  ggtitle("Kendall-Colijn lambda = 0") + 
  theme_bw()
  
  kchalf <- ggplot(forPlot,
                aes(x = kc.0.5., colour = seed, fill = seed)) + 
    geom_density(alpha = .5) +
    scale_x_continuous("Distance to (golden) true tree", expand = c(0, 0), breaks = number_ticks(5)) +
    scale_y_continuous(expand = c(0, 0), breaks = number_ticks(5) ) +
    facet_wrap(~dataset, scales = "free") +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    guides(fill = FALSE, colour = FALSE) +
    ggtitle("Kendall-Colijn lambda = 1/2") + 
  theme_bw()  
    
  kc1 <- ggplot(forPlot,
                aes(x = kc.1.0., colour = seed, fill = seed)) + 
    geom_density(alpha = .5) +
    scale_x_continuous("Distance to (golden) true tree", expand = c(0, 0), breaks = number_ticks(5)) +
    scale_y_continuous(expand = c(0, 0), breaks = number_ticks(5) ) +
    facet_wrap(~dataset, scales = "free") +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle("Kendall-Colijn lambda = 1") + 
  theme_bw()
  
  sp <- ggplot(forPlot,
               aes(x = sp, colour = seed, fill = seed)) + 
    geom_density(alpha = .5) +
    scale_x_continuous("Distance to (golden) true tree", expand = c(0, 0), breaks = number_ticks(5)) +
    scale_y_continuous(expand = c(0, 0), breaks = number_ticks(5) ) +
    facet_wrap(~dataset, scales = "free") +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle("Steel-Penny") +
  theme_bw()
  
  clade <- ggplot(forPlot,
               aes(x = clade, colour = seed, fill = seed)) + 
    geom_density(alpha = .5) +
    scale_x_continuous("Distance to (golden) true tree", expand = c(0, 0), breaks = number_ticks(5)) +
    scale_y_continuous(expand = c(0, 0), breaks = number_ticks(5) ) +
    facet_wrap(~dataset, scales = "free") +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle("Clade distances") +
  theme_bw()
  
  branch <- ggplot(forPlot,
                  aes(x = branch, colour = seed, fill = seed)) + 
    geom_density(alpha = .5) +
    scale_x_continuous("Distance to (golden) true tree", expand = c(0, 0), breaks = number_ticks(5)) +
    scale_y_continuous(expand = c(0, 0), breaks = number_ticks(5) ) +
    facet_wrap(~dataset, scales = "free") +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle("Branch-difference score") +
  theme_bw()
  
  rf
  kc0
  kchalf
  kc1
  sp
  clade
  branch

library(cowplot)
plot_grid(rf, kc0, kchalf, sp, align = 'hv', nrow = 2, ncol = 2, labels = LETTERS[1:4])
    
getQuants <- function(x, alpha = .95){
  c(
    lwr = quantile(x, probs = (1-alpha)/2),
    mean = mean(x),
    upr =  quantile(x, probs = (1 + alpha)/2)  
  )
} 

aggregate(value ~ dataset + metric + seed, data = forAnalysis, getQuants)
golden_ci <- aggregate(value ~ dataset + metric, data = forAnalysis, getQuants)
write.csv(data.frame(golden_ci), file = "golden_distanceTrueTree.csv", row.names = FALSE)