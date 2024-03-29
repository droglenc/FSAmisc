% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TroutDietSL.R
\docType{data}
\name{TroutDietSL}
\alias{TroutDietSL}
\title{Size and diet data for lake and bull trout from Swan Lake, Montana.}
\format{
A data frame with 470 observations on the following 2 variables.
 \describe{
   \item{number}{A unique identification number for each fish.}
   \item{species}{A species code: \code{BLT}=bull trout, \code{LKT}=lake trout.}
   \item{sl}{Standard length (mm).  Only recorded for bull trout.}
   \item{tl}{Total length (mm).  Computed from equation in source for bull trout.}
   \item{weight}{Weight (g)}
   \item{vol.ttl}{Total volume (ml) of prey in the stomach.}
   \item{vol.mysis}{Volume (ml) of \emph{Mysis diluviana} in the stomach.}
   \item{vol.oi}{Volume (ml) of other invertebrates in the stomach.}
   \item{vol.fish}{Volume (ml) of fish in the stomach.}
   \item{num.dipter}{Number of dipterans in the stomach.}
   \item{num.pisidium}{Number of \emph{Pisidium} spp. in the stomach.}
 }
}
\source{
From the \href{http://dx.doi.org/10.3996/012011-JFWM-004.S1}{online supplement} to Guy, C.S., T.E. McMahon, W.A. Fredenberg, C.J. Smith, D.W. Garfield, and B.S. Cox. 2011. \href{http://dx.doi.org/10.3996/012011-JFWM-004}{Diet overlap of top-level predators in recent sympatry: Bull trout and nonnative lake trout.}  Journal of Fish and Wildlife Management 2:183-189.
}
\description{
Size (standard length, total length, and weight) and diet data (volume of three main prey categories) for Bull Trout (\emph{Salvelinus confluentus}) and Lake Trout (\emph{Salvelinus namaycush}) from Swan Lake, Montana, a lake where Lake Trout just recently had become established.
}
\section{Topic(s)}{

 \itemize{
   \item Length-Weigth 
   \item Diet Overlap 
 }
}

\examples{
data(TroutDietSL)
str(TroutDietSL)
head(TroutDietSL)

# The authors added a percent volume for each diet item for each fish
TroutDietSL$pvol.mysis <- TroutDietSL$vol.mysis/TroutDietSL$vol.ttl
TroutDietSL$pvol.oi <- TroutDietSL$vol.oi/TroutDietSL$vol.ttl
TroutDietSL$pvol.fish <- TroutDietSL$vol.fish/TroutDietSL$vol.ttl
# add empty variable
TroutDietSL$empty <- ifelse(TroutDietSL$vol.ttl==0,"YES","NO")
head(TroutDietSL)

}
\concept{'Length-Weight' 'Diet Overlap'}
\keyword{datasets}
