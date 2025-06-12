# plots_3d.R

plot3DGreeks <- function(type, func, S, X, t, r, v, 
                         theta = 30, phi = 30, expand = 0.75, 
                         col = "cyan", ltheta = 120, shade = 0.75, 
                         ticktype = "detailed", cex = 0.6, 
                         main = paste("Black-Scholes", ifelse(type == 0, "Call", "Put"), 
                                      "Option Sensitivity for", simpleCap(func)), ...) {

  greeks3D <- function(S, t, X, r, v) {
    greeks(type, func, S, X, t, r, v)
  }

  z <- outer(S, t, FUN = greeks3D, X, r, v)

  persp(x = S, y = t, z = z, xlab = "S", ylab = "Time", 
        zlab = simpleCap(func), theta = theta, phi = phi, 
        expand = expand, col = col, ltheta = ltheta, shade = shade, 
        ticktype = ticktype, cex = cex, main = main, ...)

  invisible(list(S = S, t = t, Sensitivity = z))
}
