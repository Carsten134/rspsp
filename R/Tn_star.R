Tn_star <- function(x, y,...) {
  I_x <- I(x)
  I_y <- I(y)

  I_tilde <- .5*(I_x + I_y)

  # permuting entries
  N <- nrow(I_x)
  M <- ncol(I_x)

  perm <- matrix(as.numeric(runif(N*M) > .5), nrow = N)
  perm_n <- matrix(as.numeric(perm != 1), nrow = N)
  I_x_rand <- I_x*perm + I_y*perm_n
  I_y_rand <- I_x*perm_n + I_y*perm

  I_x_diff <- I_smooth(I_x_rand - I_tilde,...)
  I_y_diff <- I_smooth(I_y_rand - I_tilde,...)

  # manual Riemann integration
  f <- function(x, y) I_x_diff(x,y)^2 + I_y_diff(x,y)^2

  return(riemann_approx(f, 25, 25))
}
