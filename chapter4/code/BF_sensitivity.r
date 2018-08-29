getBF <- function(post, prior){
  (post)/(1-post) / (prior)/(1-prior)
}
#
getBF2 <- function(post, w, P){
  prior <- 1 - w^{1/P}
  getBF(post = post, prior = prior)
}
#
getPosterior <- function(BF, w, P){
  prior <- 1 - w^{1/P}
  priorOdds <- prior/(1-prior)
  return((priorOdds * BF)/(1 + priorOdds * BF))
}
###############
Dims <-  c(10, 20, 50)
Grid <- expand.grid(posterior = seq(0.01, .99, length.out = 50),
                    w = seq(.2, .8, by = .1),
                    P = Dims,
                    refBFs = c(3.2, 10, 100) 
)
Result <- data.frame(Grid,
                     BF = apply(Grid, 1, function(x) getBF2(post = x[1], w = x[2], P = x[3])),
                     postCut = apply(Grid, 1, function(x) getPosterior(BF = x[4], w = x[2], P = x[3]))
)
Result$w <- as.factor(Result$w)
Result$P <- factor(Result$P,
                   labels = paste("P==", Dims, sep = ""))
###############
library(ggplot2)
library(scales)
number_ticks <- function(n) {function(limits) pretty(limits, n)}

marks <- data.frame(unique(Result[, c("w", "P", "refBFs", "postCut")]))
names(marks) <- c("w", "P", "BF", "posterior")
marks <- subset(marks, w == "0.5")
marks$BF_cutoff <- as.factor(marks$BF)


p0 <- ggplot(data = Result, aes(x = posterior, y = BF, colour = w)) +
  geom_line(size = 1) +
  scale_colour_discrete(name = expression("Pr(S=0)")) +
  scale_colour_discrete(name = "Probability no \n predictors included") +
  scale_x_continuous("Posterior probability", expand = c(0, 0), breaks = number_ticks(10)) +
  scale_y_continuous(trans = log_trans(base = 10),
                     "Bayes factor",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     expand = c(0, 0)) +
  geom_segment(data = marks, aes(xend = -Inf, yend = BF, linetype = BF_cutoff), size = .5, colour = "black") +#9999CC"
  geom_segment(data = marks, aes(xend = posterior, yend = 0, linetype = BF_cutoff), size = .5,  colour = "black")  +
  facet_grid(. ~ P, labeller = label_parsed) +
  scale_linetype_manual(name = "Bayes factor cutoff", values = c("dotted", "dotdash", "longdash")) + 
  theme_bw()

p0
