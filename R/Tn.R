Tn <- function(x, y, ...) {
  I_x <- I(x)
  I_y <- I(y)
  I_tilde <- .5 * (I_x + I_y)

  I_x_smooth <- I_smooth(I_x - I_tilde, ...)
  I_y_smooth <- I_smooth(I_y - I_tilde, ...)

  # manual Riemann integration
  N <- nrow(I_x)
  M <- ncol(I_x)

  f <- function(x, y) I_x_smooth(x, y)^2 + I_y_smooth(x, y)^2
  return(riemann_approx(f, 25, 25))
}
