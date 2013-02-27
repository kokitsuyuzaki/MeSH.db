\name{MeSHQUALIFIER}
\alias{MeSHQUALIFIER}
\title{Corresponding table of MeSH Term IDs and those of Qualifier IDs}
\description{
This is a corresponding table (dataframe) of MeSH Term identifiers and qualifier identifiers
}

\references{
  \url{http://www.nlm.nih.gov/mesh/filelist.html} 
}
\examples{
  # You can retrive the QUALIFIERID by MESHID
  select(MeSHQUALIFIER, keys=c("D000002", "D000006"), cols=c("QUALIFIERID", "MESHID"), keytype="MESHID")
	
	# You also can retrive the MESHID by QUALIFIERID
	select(MeSHQUALIFIER, keys=c("Q000646", "D000745"), cols=c("QUALIFIERID", "MESHID"), keytype="QUALIFIERID")
	
	}
\keyword{datasets}

