explicit_euler <- function(ode, yinit, tstart, tend, h = 0.1) {
  tcurrent <- tstart
  ycurrent <- yinit
  yres <- c(tstart, yinit)
  while (tcurrent < tend) {
    ynew <- ycurrent + h * ode(ycurrent, tcurrent)
    tcurrent <- tcurrent + h
    yres <- rbind(yres, c(tcurrent, ynew))
  }
  return(yres)
}
