## TARGET DIAGRAM
# Based on TargetDiagram from solaR package



function (x, end, ndays, ref = NULL, color = NULL, cex = 0.8, 
          ...) 
{
  stopifnot(class(index(x)) == class(end))
  if ("POSIXct" %in% class(end)) {
    nDays = ndays * 24 * 3600
  }
  else {
    nDays = ndays
  }
  Analisis <- data.frame()
  for (i in 1:length(ndays)) {
    start = end - (nDays[i] - 1)
    if (start < start(x)) 
      start = start(x)
    data.w <- window(x, start = start, end = end)
    AnalisisTemp <- analyzeData(data.w, ref)$err
    AnalisisTemp$ndays <- ndays[i]
    Analisis <- rbind(Analisis, AnalisisTemp)
  }
  Unitfc = factor(Analisis$Unit)
  NDaysfc <- factor(Analisis$ndays)
  Radio <- signif(quantile(Analisis$RMSD)[2:5], 2)
  Circ <- expand.grid(Theta = seq(0, 2 * pi, length = 100), 
                      R = Radio)
  Circ$X <- with(Circ, R * sin(Theta))
  Circ$Y <- with(Circ, R * cos(Theta))
  my.pch = 1:nlevels(Unitfc)
  if (is.null(color)) {
    p <- xyplot(ME ~ RMSDc * sign(DifSD) | ndays, data = Analisis, 
                cex = cex, ..., xlab = expression(sigma["D"] %.% 
                                                    "sign(" * sigma^"*" * ")"), ylab = expression(bar(D)), 
                aspect = "iso", col = "black", strip = strip.custom(strip.levels = c(TRUE, 
                                                                                     TRUE), strip.names = c(TRUE, TRUE)), panel = function(x, 
                                                                                                                                           y, cex = cex, ...) {
                                                                                       panel.text(x, y, labels = Unitfc, cex = cex, 
                                                                                                  ...)
                                                                                       panel.abline(h = 0, v = 0, lty = 2, col = "gray")
                                                                                       for (i in 1:4) {
                                                                                         with(Circ, panel.xyplot(X[R == Radio[i]], Y[R == 
                                                                                                                                       Radio[i]], lty = 2, type = "l", col = "grey"))
                                                                                       }
                                                                                       panel.text(x = Radio, y = 0, labels = signif(Radio, 
                                                                                                                                    1), pos = 4, cex = 0.6, ...)
                                                                                     }, )
  }
  else {
    my.fill = color
    p <- xyplot(ME ~ RMSDc * sign(DifSD), data = Analisis, 
                cex = cex, ..., xlab = expression(sigma["D"] %.% 
                                                    "sign(" * sigma^"*" * ")"), ylab = expression(bar(D)), 
                aspect = "iso", panel = function(x, y, cex = cex, 
                                                 ...) {
                  col = my.fill[NDaysfc]
                  panel.text(x, y, labels = Unitfc, col = col, 
                             cex = cex, ...)
                  panel.abline(h = 0, v = 0, lty = 2, col = "gray")
                  for (i in 1:4) {
                    with(Circ, panel.xyplot(X[R == Radio[i]], Y[R == 
                                                                  Radio[i]], lty = 2, type = "l", col = "grey"))
                  }
                  panel.text(x = Radio, y = 0, labels = signif(Radio, 
                                                               2), pos = 4, cex = 0.6, ...)
                }, key = list(space = "right", adj = 1, title = "ndays", 
                              text = list(levels(NDaysfc)), points = list(pch = 16, 
                                                                          col = my.fill), rep = FALSE))
  }
  print(p)
  result <- list(plot = p, stat = Analisis)
}
<environment: namespace:solaR>