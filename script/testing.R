
library(Conet1)

x = c(10000,100,10,0,0,0)
names(x) <- names_david


theta <- c(alpha0 = 100, alpha = 4000, beta = 0.04, delta = 0.25, kappa = 0.7, eta = 0.45, gamma = 0.3, n = 1.4)

inp <- list(transitions = transitions_david, lvrates = lvrates_david)

gen_david(x, theta, 0, 1, inp)
