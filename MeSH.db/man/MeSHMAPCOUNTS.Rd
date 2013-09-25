\name{MeSHMAPCOUNTS} 

\alias{MeSHMAPCOUNTS}


\title{Number of mapped keys for the maps in package MeSH.db}

\description{
  MeSHMAPCOUNTS provides the "count" for each data in package MeSH.db.
}

\details{  
  This "count" information is precalculated and stored in the
  package annotation DB. This allows some quality control and is used by
  the \code{\link[AnnotationDbi:AnnDbPkg-checker]{checkMAPCOUNTS}}
  function defined in AnnotationDbi to compare and validate different
  methods (like \code{count.mappedkeys(x)} or
  \code{sum(!is.na(as.list(x)))}) for getting the "map count" of a given
  map.
}

\examples{
  select(MeSHMAPCOUNTS, keys=c("MeSHTERM"), columns=c("MAPNAME", "COUNT"))
}

\keyword{datasets}

