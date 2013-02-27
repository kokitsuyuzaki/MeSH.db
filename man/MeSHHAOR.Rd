\name{MeSHHAOR}
\alias{MeSHHAOR}
\title{Annotation corresponding table of MeSH Identifiers of Ancestor-Offspring Relationships in "Disciplines and Occupations" Category} 
\description{
  This data set describes associations between MeSH "Disciplines and Occupations" (H as MeSH Tree Number)
 offspring terms and their ancestor H terms. AOR is an abbreviation of Ancester-Offspring Relationships. The format is a dataframe, corresponding table of the MeSH H offspring terms and those ancestor terms.
}
\details{
  Each offspring MeSH H term is assigned to some ancestor MeSH H terms. Contorary, each ancestor MeSH H term is assigned to some offspring MeSH H terms.
    
  Corresponding tables were based on data provided by: 2013 Medical Subject Headings
  \url{http://www.nlm.nih.gov/mesh/filelist.html}  
  with a date stamp from the source of: 20120907

}
\references{
\url{http://www.ncbi.nlm.nih.gov/mesh/1000072} and
\url{http://www.nlm.nih.gov/mesh/filelist.html} 
}
\examples{
  # "cols" method tells you about what kinds of values you can retrive as columns
  cols(MeSHHAOR)
  # "keytypes" method tells you the kinds of things that can be used as keys
  keytypes(MeSHHAOR)
  # "keys" method is used to retrive all the viable keys of a particular type
  k1 <- head(keys(MeSHHAOR, keytype="ANCESTORMESHID"))
  k1
  k2 <- head(keys(MeSHHAOR, keytype="OFFSPRINGMESHID"))
  k2
  # "select" method extracts data by using values supplied by the other method
  result1 <- select(MeSHHAOR, keys=k1, cols=c("ANCESTORMESHID", "OFFSPRINGMESHID"), keytype="ANCESTORMESHID")
  head(result1)
  result2 <- select(MeSHHAOR, keys=k2, cols=c("ANCESTORMESHID", "OFFSPRINGMESHID"), keytype="OFFSPRINGMESHID")
  head(result2)

}
\keyword{datasets}

