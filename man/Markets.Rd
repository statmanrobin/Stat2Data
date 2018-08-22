\name{Markets}
\alias{Markets}
\docType{data}
\title{Markets}
\description{
Daily changes in two stock market indices
}
%\useage{data(Markets)}
\format{
  A dataset with 56 observations on the following 5 variables.
  \tabular{rl}{
    \code{DJIAch} \tab {Change in Dow Jones Industrial Average}\cr
    \code{Date} \tab {Date: 06-Aug-09 to 02-Nov-09}\cr
    \code{Nik225ch} \tab {Change in Nikkei 225 stock average}\cr
    \code{Up} \tab {Indicator for positive Nikkei change}\cr
    \code{lagNik} \tab {Previous day's Nikkei change}\cr
  }
}
\details{
This dataset contains data on daily changes from two stock markets over 56 days from 06-Aug-09 to 02-Nov-09. 
 The Dow Jones Industrial Average is based in New York and the Nikkei 225 is a stock index in Japan. 
}
\source{
Dow Jones Industrial Average:
http://markets.cbsnews.com/cbsnews/quote/historical?Month=11&Symbol=310\%3A998313&Year=2009&Range=12&tag=cbsnewsSectionsArea
Historical Nikkei 225 index:
http://markets.cbsnews.com/cbsnews/quote/historical?Month=11&Symbol=992\%3A1900000035&Year=2009&Range=12&tag=cbsnewsSectionsArea
}

\keyword{datasets}
