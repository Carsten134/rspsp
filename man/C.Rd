% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/C.R
\name{C}
\alias{C}
\title{Covariance function of a grid MA process}
\usage{
C(k, l, K)
}
\arguments{
\item{k}{integer offset on row}

\item{l}{integer offset on column}

\item{K}{Coefficient matrix of "lag" polynomial}
}
\value{
numeric: value of \eqn{C(k,l) = Cov(X_{s+k,t+l},X_{s,t})}
}
\description{
Computes the covariance of a grid MA process: \eqn{Cov(X_{s+k, t+l}, X_{s,t})}
}
