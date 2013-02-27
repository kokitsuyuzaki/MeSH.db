\name{MeSHKAOR}
\alias{MeSHKAOR}
\title{Annotation corresponding table of MeSH Identifiers of Ancestor-Offspring Relationships in "Humanities" Category} 
\description{
  This data set describes associations between MeSH "Humanities" (K as MeSH Tree Number)
 offspring terms and their ancestor K terms. AOR is an abbreviation of Ancester-Offspring Relationships. The format is a dataframe, corresponding table of the MeSH K offspring terms and those ancestor terms.
}
\details{
  Each offspring MeSH K term is assigned to some ancestor MeSH K terms. Contorary, each ancestor MeSH K term is assigned to some offspring MeSH K terms.
    
  Corresponding tables were based on data provided by: 2013 Medical Subject Headings
  \url{http://www.nlm.nih.gov/mesh/filelist.html}  
  with a date stamp from the source of: 20120907

}
\references{
\url{http://www.ncbi.nlm.nih.gov/mesh/1000075} and
\url{http://www.nlm.nih.gov/mesh/filelist.html} 
}
\examples{
  # "cols" method tells you about what kinds of values you can retrive as columns
  cols(MeSHKAOR)
  # "keytypes" method tells you the kinds of things that can be used as keys
  keytypes(MeSHKAOR)
  # "keys" method is used to retrive all the viable keys of a particular type
  k1 <- head(keys(MeSHKAOR, keytype="ANCESTORMESHID"))
  k1
  k2 <- head(keys(MeSHKAOR, keytype="OFFSPRINGMESHID"))
  k2
  # "select" method extracts data by using values supplied by the other method
  result1 <- select(MeSHKAOR, keys=k1, cols=c("ANCESTORMESHID", "OFFSPRINGMESHID"), keytype="ANCESTORMESHID")
  head(result1)
  result2 <- select(MeSHKAOR, keys=k2, cols=c("ANCESTORMESHID", "OFFSPRINGMESHID"), keytype="OFFSPRINGMESHID")
  head(result2)

}
\keyword{datasets}

