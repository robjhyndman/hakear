d <- data.frame(data = c(rep("allw", 7), rep("allb", 9)),
                dtype = c(rep("w", 4), rep("b", 3), rep("w", 3), rep("b", 6)),
                d = c(rep(c(1.2, 1.4), 2), rep(0, 3),
                      rep(0, 3), c(1.2, 1.2, 1.4, 1.4, 1.2, 1.2)))
d

lambda <- seq(0.1, 0.9, 0.1)

f_allw <- NULL
f_allb <- NULL
for (i in 1:length(lambda)) {
  f_allw <- c(f_allw, max(d$d[d$data == "allw" & d$dtype == "w"]*lambda[i],
                          d$d[d$data == "allw" & d$dtype == "b"]*(1-lambda[i])))
  f_allb <- c(f_allb, max(d$d[d$data == "allb" & d$dtype == "w"]*lambda[i],
                          d$d[d$data == "allb" & d$dtype == "b"]*(1-lambda[i])))
}
f_allw
f_allb
