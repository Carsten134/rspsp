phi_n_star <- function(x, y, B, alpha, print_result = T, ...) {
  Tn_val <- Tn(x, y,...)

  Tn_star_val <- numeric(B)
  T_val <- numeric(B)
  for (i in 1:B) {
    Tn_star_val[i] <- Tn_star(x, y,...)
    T_val[i] <- sum(Tn_star_val[1:i] < Tn_val)/i
  }

  decision <- (T_val[B] > (1-alpha/2) | T_val[B] < alpha/2)
  if (print_result == T) {
    par(mfrow = c(1,2))

    hist(Tn_star_val, main = "Histogram of Tn*",
         xlim = c(min(c(Tn_star_val, Tn_val)), max(c(Tn_star_val, Tn_val))),
         breaks = 30)
    abline(v = Tn_val, col = "red")
    plot(T_val, main = "Trace of T over all randomizations",
         xlab = "b",
         ylab = "T",
         ylim = c(0,1),
         type = "l")
    abline(h = alpha/2, col = "red")
    abline(h = 1-alpha / 2, col = "red")

    print("Test for equality of spatial ACF and spectral density")
    print(paste("Randomized samples: ", B))
    print(paste("Tn: ", Tn_val))
    print(paste("Percentage of Tn > Tn*: ", T_val[B]*100, "%"))
    if (decision) {
      print("Decision: Rejecting H0")
    } else {
      print("Decision: Accepting H0")
    }
  } else {
    return(as.numeric(decision))
  }
}
