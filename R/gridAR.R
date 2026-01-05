#' Simulate a lattice AR(1) process
#'
#' Generates one realisation from an autoregressive process on an
#' \eqn{N \times M} lattice by using the Wold-weight deconstruction.
#'
#' @param N Integer number of rows.
#' @param M Integer number of columns.
#' @param phi_1 Horizontal autoregressive coefficient.
#' @param phi_2 Vertical autoregressive coefficient.
#' @param phi_3 Diagonal autoregressive coefficient.
#' @param phi_4 Optional additional diagonal coefficient. If `NULL` or `0` the
#'   quarter-plane kernel is used.
#' @param wold_size Size of the Wold-weight lattice, defaults to 50.
#'
#' @return Numeric matrix with `N` rows and `M` columns.
#' @examples
#' set.seed(1)
#' x <- gridAR(25, 25, 0.8, 0.1, 0.05)
#' image(x)
#' @export
gridAR <- function(N,
                   M,
                   phi_1,
                   phi_2,
                   phi_3,
                   phi_4 = NULL,
                   wold_size = 50) {
  N <- as.integer(N)
  M <- as.integer(M)
  wold_size <- as.integer(wold_size)

  if (is.na(N) || N < 1) stop("N must be a positive integer.")
  if (is.na(M) || M < 1) stop("M must be a positive integer.")
  if (is.na(wold_size) || wold_size < 1) stop("wold_size must be a positive integer.")

  use_half_plane <- !is.null(phi_4) && !isTRUE(all.equal(phi_4, 0))
  kernel <- if (use_half_plane) {
    wold_half_kernel(wold_size, phi_1, phi_2, phi_3, phi_4)
  } else {
    wold_quarter_kernel(wold_size, phi_1, phi_2, phi_3)
  }

  padding <- nrow(kernel) %/% 2
  N_tilde <- N + 2 * padding
  M_tilde <- M + 2 * padding

  eps <- matrix(stats::rnorm(N_tilde * M_tilde), nrow = N_tilde, ncol = M_tilde)
  conv <- convolve2d_circular(eps, kernel)

  row_idx <- seq.int(padding + 1, padding + N)
  col_idx <- seq.int(padding + 1, padding + M)

  conv[row_idx, col_idx, drop = FALSE]
}

# Builds quarter-plane Wold weights to mimic the PlaneSampler kernel.
wold_quarter_kernel <- function(order, phi_1, phi_2, phi_3) {
  weights <- matrix(0, nrow = order, ncol = order)
  weights[, order] <- phi_3 ^ (0:(order - 1))
  weights[1, order:1] <- phi_1 ^ (0:(order - 1))

  if (order > 1) {
    phi_vec <- c(phi_1, phi_2, phi_3)
    inner_cols <- seq_len(order - 1)
    for (i in 2:order) {
      for (j in rev(inner_cols)) {
        neighbors <- c(
          weights[i, j + 1],
          weights[i - 1, j + 1],
          weights[i - 1, j]
        )
        weights[i, j] <- sum(phi_vec * neighbors)
      }
    }
  }

  kernel <- matrix(0, nrow = 2 * order - 1, ncol = 2 * order - 1)
  kernel[order:(2 * order - 1), 1:order] <- weights
  kernel[(2*order-1):1,]
}

# Builds half-plane Wold weights that include the additional diagonal term.
wold_half_kernel <- function(order, phi_1, phi_2, phi_3, phi_4) {
  kernel <- matrix(0, nrow = 2 * order - 1, ncol = 2 * order - 1)
  diag_idx <- seq.int(order, 2 * order - 1)
  kernel[cbind(diag_idx, diag_idx)] <- phi_4 ^ (0:(order - 1))
  kernel[order, order:1] <- phi_1 ^ (0:(order - 1))

  if (order > 1) {
    phi_vec <- c(phi_1, phi_2, phi_3, phi_4)
    for (i in seq.int(order + 1, 2 * order - 1)) {
      cols <- seq_len(i - 1)
      for (j in rev(cols)) {
        east <- kernel[i, j + 1]
        north_east <- kernel[i - 1, j + 1]
        north <- kernel[i - 1, j]
        north_west <- if (j > 1) kernel[i - 1, j - 1] else 0
        kernel[i, j] <- sum(phi_vec * c(east, north_east, north, north_west))
      }
    }
  }

  kernel[(2*order-1):1,]
}
