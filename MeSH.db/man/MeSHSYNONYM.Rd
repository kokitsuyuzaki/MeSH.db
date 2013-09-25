\name{MeSHSYNONYM}
\alias{MeSHSYNONYM}
\title{Map from MeSH synonyms to MeSH terms}
\description{
  MeSHSYNONYM is a correspponding table (dataframe) between MeSH synonym and MeSH terms
}
\examples{
	# You can retrive the MESHSYNONYM by MESHID
	select(MeSHSYNONYM, keys=c("D000002", "D000005"), columns=c("MeSHID", "MESHSYNONYM"))

}
\keyword{datasets}

