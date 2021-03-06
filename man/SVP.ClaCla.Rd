% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/humidity.R
\name{SVP.ClaCla}
\alias{SVP.ClaCla}
\title{calculate saturation vapor pressure using the Clausius-Clapeyron equation}
\usage{
SVP.ClaCla(t)
}
\arguments{
\item{t}{temperature in Kelvin (K)}
}
\value{
numeric saturation vapor pressure in hectopascal (hPa) or millibar (mb)
}
\description{
calculate saturation vapor pressure \eqn{E_s} at temperature \eqn{t}, using the Clausius-Clapeyron equation.
}
\examples{
T0 # absolute zero in Kelvin (K)
SVP.ClaCla(T0)
}
\references{
Shaman, J., & Kohn, M. (2009). \emph{Absolute humidity modulates influenza survival, transmission, and seasonality}. Proceedings of the National Academy of Sciences, 106(9), 3243-3248.

Wallace, J. M., & Hobbs, P. V. (2006). \emph{Atmospheric science: an introductory survey} (Vol. 92). Academic press.
}
\seealso{
\code{\link{SVP.Murray}}, \code{\link{SVP}}.
}
\author{
Jun Cai (\email{cai-j12@mails.tsinghua.edu.cn}), PhD candidate from
Department of Earth System Science, Tsinghua University
}
