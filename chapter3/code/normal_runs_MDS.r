source("~/DUMP/PIPELINE/code/BEAST_convergence_pipeline.r")

LT  <- getLTs(path = "")

Full <- lapply(LT, make_full)
names(Full) <- gsub("denv2_env", "denv2Env", names(Full))
names(Full) <- gsub("denv2_genome", "denv2Genome", names(Full))
# RF <- lapply(LT[grep("RF", names(LT))], make_full)
# KC0 <- lapply(LT[grep("KC0", names(LT))], make_full)
MDS <- parallel::mclapply(Full, getMDS, step_size = 1000, mc.cores = 10) ## RF.procrustes
names(MDS) <- names(Full)
process_names <- function(x){
  Split <- strsplit(x, "_")
  return(
    data.frame(dataset = Split[[1]][5],
               seed = Split[[1]][4],
               metric = Split[[1]][1]
    )
  )
}

MDS.flattened <- data.table::rbindlist(
  lapply(seq_along(MDS), function(i){
    data.frame(MDS[[i]], process_names(names(MDS)[i]))
  })
)
# MDS.flattened$state <- MDS.flattened$STATE
MDS.flattened$STATE <- NULL
# MDS.flattened$dataset <- gsub("50taxa", "", MDS.flattened$dataset)

logfiles <- system("ls *.log", intern = TRUE)
Logs <- lapply(logfiles, read.table, header = TRUE, fill = TRUE)
names(Logs) <- logfiles
process_names2 <- function(x){
  Split <- strsplit(x, "_")
  return(
    data.frame(dataset = Split[[1]][3],
               seed = Split[[1]][2]
    )
  )
}
Tails <- data.table::rbindlist(lapply(seq_along(Logs),
                                      function(i){
                                        x <- Logs[[i]]
                                        data.frame(
                                          state = x[(nrow(x)-999):nrow(x), ]$state,
                                          likelihood = x[(nrow(x)-999):nrow(x), ]$likelihood,
                                          process_names2(logfiles[i])
                                        )
                                      } ))
Metric.Split <- split(MDS.flattened, f = MDS.flattened$metric)
Metric.Split <- lapply(Metric.Split, function(x) data.frame(x, Tails))  
complete <- data.table::rbindlist(Metric.Split)


library(ggplot2)
library(viridis)

number_ticks <- function(n) {function(limits) pretty(limits, n)}

Re.Split <- split(complete, f = complete$dataset)
make_plot <- function(dt) {
      ggplot(dt,
         aes(x = x, y = y, colour = likelihood, size = state, shape = seed)) + 
    geom_point(alpha = .5) +
    scale_x_continuous("First component", expand = c(0, 0),  breaks = number_ticks(5)) +
    scale_y_continuous("Second component", expand = c(0, 0), breaks = number_ticks(5) ) +
    facet_wrap(~metric, scales = "free") +
    scale_size_continuous(range = c(1, 4)) + 
    scale_color_gradient(low = "blue", high = "red") +
    # scale_color_viridis(discrete = TRUE) +
    # scale_fill_viridis(discrete = TRUE) +
    ggtitle(paste(dt$dataset[1])) + 
    theme_bw()
}

lapply(Re.Split,  make_plot)
####################################

Denv2 <- subset(complete, dataset %in% c("denv2Genome", "denv2Env"))
Denv2$dataset <- gsub("denv2", "", Denv2$dataset)
SDenv2 <- split(Denv2, f = Denv2$metric)
make_plot2 <- function(dt){
  if(dt$metric[1] == "KChalf"){
    ggplot(dt,
           aes(x = x, y = y, colour = seed, size = STATE)) + 
      geom_point(alpha = .5) +
      scale_x_continuous("First component", expand = c(0, 0),  breaks = number_ticks(10)) +
      scale_y_continuous("Second component", expand = c(0, 0), breaks = number_ticks(10) ) +
      scale_size_continuous(range = c(1, 4)) + 
      facet_grid(~dataset) +
      scale_color_viridis(discrete = TRUE, option = "C") +
      scale_fill_viridis(discrete = TRUE, option = "C") +
      ggtitle(paste(dt$metric[1])) + 
      theme_bw()
  }else{
    ggplot(dt,
           aes(x = x, y = y, colour = seed, size = STATE)) + 
      geom_point(alpha = .5) +
      scale_x_continuous("First component", expand = c(0, 0),  breaks = number_ticks(10)) +
      scale_y_continuous("Second component", expand = c(0, 0), breaks = number_ticks(10) ) +
      scale_size_continuous(range = c(1, 4)) + 
      facet_grid(~dataset) +
      guides(fill = FALSE, colour = FALSE, size = FALSE) +
      scale_color_viridis(discrete = TRUE, option = "C") +
      scale_fill_viridis(discrete = TRUE, option = "C") +
      ggtitle(paste(dt$metric[1])) + 
      theme_bw()
  }
}
denv2Plots <- lapply(SDenv2,  make_plot2)
library(cowplot)
plot_grid(denv2Plots$KC0, denv2Plots$KC1, denv2Plots$KChalf, denv2Plots$RF, denv2Plots$SP,
          align = 'v', nrow = 5, ncol = 1, labels = LETTERS[1:5])

###################

ISOS <- lapply(Full, vegan::wcmdscale, k = 2, eig = TRUE)
nc <- 5
Screes <- data.table::rbindlist(
  lapply(seq_along(ISOS), function(i){
    w <- ISOS[[i]]$eig/sum(ISOS[[i]]$eig)
    data.frame(
      component = 1:nc,
      percent = w[1:nc],
      process_names(names(ISOS)[i])
    )
  } )
)

ggplot(Screes,
       aes(x = component, y = percent, colour = seed)) + 
  geom_point(alpha = .5) +
  geom_line(alpha = .5) +
  scale_x_continuous("Component", expand = c(0, 0),  breaks = number_ticks(min(nc, 10))) +
  scale_y_continuous("Normalised eigenvalue", expand = c(0, 0), breaks = number_ticks(10) ) +
  facet_grid(metric~dataset) +
  # guides(fill = FALSE, colour = FALSE, size = FALSE) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme_bw()
