data <- read.table("data.csv", col.names = c("x"))
quantile(data$x, c(0, .5, .9, .99, .999, .9999), type=1)
