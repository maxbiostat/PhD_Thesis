process_names <- function(x){
  Split <- strsplit(x, "_")
  return(
    data.frame(dataset = Split[[1]][3],
               seed = Split[[1]][2],
               dates = gsub(".tmlog", "", Split[[1]][4]),
               sample = ifelse(is.na(Split[[1]][5]), "posterior", gsub(".tmlog", "", Split[[1]][5]))
    )
  )
}
source("~/DUMP/PIPELINE/code/BEAST_convergence_pipeline.r")
Files <- system("ls *.tmlog", intern = TRUE)
Logs <- parallel::mclapply(Files, read.table, header = TRUE, mc.cores = 10)


complete.data <- data.table::rbindlist(
  lapply(seq_along(Logs), function(i){
    data.frame(Logs[[i]], process_names(Files[i]))
  })
)
complete.data$dataset <- gsub("50taxa", "", complete.data$dataset)


library(ggplot2)
library(viridis)
number_ticks <- function(n) {function(limits) pretty(limits, n)}

p0 <- ggplot(complete.data,
              aes(x = seed, y = rf, colour = dates)) + 
  geom_boxplot(alpha = .5) +
  scale_x_discrete("Replicate") +
  scale_y_continuous("Distance to true (generating) tree", breaks = number_ticks(10) ) +
  facet_grid(dataset~sample) +
  guides(fill = FALSE, colour = FALSE) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  ggtitle("Robinson-Foulds") + 
  theme_bw()

p1 <- ggplot(complete.data,
       aes(x = seed, y = kc.0.0., colour = dates)) + 
  geom_boxplot(alpha = .5) +
  scale_x_discrete("Replicate") +
  scale_y_continuous("Distance to true (generating) tree", breaks = number_ticks(10) ) +
  facet_grid(dataset~sample) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  ggtitle("Kendall-Colijn, lambda = 0") + 
  theme_bw()


library(cowplot)
plot_grid(p0, p1, align = 'hv', nrow = 1, ncol = 2, labels = LETTERS[1:2])