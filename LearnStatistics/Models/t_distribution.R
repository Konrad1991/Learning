curve(dnorm(x), -4, 4, col = "blue", lwd = 2)
curve(dt(x, df = 3), add = TRUE, col = "red", lwd = 2)
curve(dt(x, df = 30), add = TRUE, col = "green", lwd = 2)

legend("topright",
       legend = c("Normal", "t df=3", "t df=30"),
       col = c("blue", "red", "green"),
       lwd = 2)
