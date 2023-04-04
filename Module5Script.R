ObsAdmit <- c(432, 247, 226, 1118)
res <- chisq.test(ObsAdmit, p = c(.574, .031, 0.008, .387))
res
res$expected
