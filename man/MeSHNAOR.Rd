\name{MeSHNAOR}
\alias{MeSHNAOR}
\title{Annotation corresponding table of MeSH Identifiers of Ancestor-Offspring Relationships in "Persons" Category} 
\description{
  This data set describes associations between MeSH "Persons" (N as MeSH Tree Number)
 offspring terms and their ancestor N terms. AOR is an abbreviation of Ancester-Offspring Relationships. The format is a dataframe, corresponding table of the MeSH N offspring terms and those ancestor terms.
}
\details{
  Each offspring MeSH N term is assigned to some ancestor MeSH N terms. Contorary, each ancestor MeSH N term is assigned to some offspring MeSH N terms.
    
  Corresponding tables were based on data provided by: 2013 Medical Subject Headings
  http://www.nlm.nih.gov/mesh/filelist.html  
  with a date stamp from the source of: 20120907

}
\references{
\url{http://www.ncbi.nlm.nih.gov/mesh/1000078} and
\url{http://www.nlm.nih.gov/mesh/filelist.html} 
}
\examples{
  # "columns" method tells you about what kinds of values you can retrive as columns
  columns(MeSHNAOR)
  # "keytypes" method tells you the kinds of things that can be used as keys
  keytypes(MeSHNAOR)
  # "keys" method is used to retrive all the viable keys of a particular type
  k1 <- head(keys(MeSHNAOR, keytype="ANCESTORMESHID"))
  k1
  k2 <- head(keys(MeSHNAOR, keytype="OFFSPRINGMESHID"))
  k2
  # "select" method extracts data by using values supplied by the other method
  result1 <- select(MeSHNAOR, keys=k1, columns=c("ANCESTORMESHID","OFFSPRINGMESHID"), keytype="ANCESTORMESHID")
  head(result1)
  result2 <- select(MeSHNAOR, keys=k2, columns=c("ANCESTORMESHID","OFFSPRINGMESHID"), keytype="OFFSPRINGMESHID")
  head(result2)

}
\keyword{datasets}

