\name{towClass}
\alias{towClass}
\title{ Continuous Plankton Recorder (CPR) Data Analyser }
\description{
This package is designed for the purpose of data analysis and visualisation for the Mediterranean Continuous Plankton Recorder Survey (MedCPR) project.
}
\usage{
  towClass(...)
}
\arguments{
  \item{...}{
    tow.id:      The serial number of the specific tow 
    tow.log:     Data files for the tow (mandatory)
    tow.ais:     Data files for the tow
    tow.ctd:     Data files for the tow
    tow.pci:     Data files for the tow
    silk.start:  Start of silk partitioning
    silk.end:    End of silk partitioning
    time.offset: Time elapsed (in sec) until first data recording
  }
}
    
\value{
  \item{towClass }{All data and methods are wrapped within this S4 class}
}
\details{
    Cool stuff to wait for...
}
\author{ 
Kamil Erguler, Associate Research Scientist, EEWRC, The Cyprus Institute \email{k.erguler@cyi.ac.cy}
}
\note{ The author is indebted to Rana Abu Alhaija for her invaluable contributions to the development and improvement of this package. }


\keyword{cpr}
\keyword{plankton}
\keyword{data}

