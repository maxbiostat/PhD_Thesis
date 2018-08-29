source("BEAST_convergence_pipeline.r")
folder <- "../data/EBOV_1610/forplot/"
Logs <- getLogs(folder)
ProcessedLogs <- process_logs(Logs)
WithRun <- lapply(seq_along(ProcessedLogs), function(i){
  dt <- ProcessedLogs[[i]]
  dt$run <- paste("Run", strsplit(names(ProcessedLogs)[i], "_")[[1]][2])
  dt$operator <- gsub(".log", "", strsplit(names(ProcessedLogs)[i], "_")[[1]][3])
  return(dt)
})

ForPlot <- data.table::rbindlist(WithRun)

library(ggplot2)

ggplot(data =  ForPlot, aes(x = state, y = likelihood, colour = run)) +
  geom_line() +
  scale_x_continuous("Iteration", expand = c(0, 0)) + 
  scale_y_continuous("Likelihood", expand = c(0, 0)) +
  facet_grid(~operator) + 
  theme_bw() + 
  theme(legend.title = element_blank()) 

ggplot(data = ForPlot, aes(x = state, y = posterior, colour = run)) +
  geom_line() +
  scale_x_continuous("Iteration", expand = c(0, 0)) + 
  scale_y_continuous("Likelihood", expand = c(0, 0)) +
  facet_grid(~operator) + 
  theme(legend.title = element_blank()) +
  theme_bw()