\name{MeSHFAOR}
\alias{MeSHFAOR}
\title{Annotation corresponding table of MeSH Identifiers of Ancestor-Offspring Relationships in "Psychiatry and Psychology" Category} 
\description{
  This data set describes associations between MeSH "Psychiatry and Psychology" (F as MeSH Tree Number)
 offspring terms and their ancestor F terms. AOR is an abbreviation of Ancester-Offspring Relationships. The format is a dataframe, corresponding table of the MeSH F offspring terms and those ancestor terms.
}
\details{
  Each offspring MeSH F term is assigned to some ancestor MeSH F terms. Contorary, each ancestor MeSH F term is assigned to some offspring MeSH F terms.
    
  Corresponding tables were based on data provided by: 2013 Medical Subject Headings
  \url{http://www.nlm.nih.gov/mesh/filelist.html}  
  with a date stamp from the source of: 20120907

}
\references{
\url{http://www.ncbi.nlm.nih.gov/mesh/1000070} and
\url{http://www.nlm.nih.gov/mesh/filelist.html} 
}
\examples{
  # "columns" method tells you about what kinds of values you can retrive as columns
  columns(MeSHFAOR)
  # "keytypes" method tells you the kinds of things that can be used as keys
  keytypes(MeSHFAOR)
  # "keys" method is used to retrive all the viable keys of a particular type
  k1 <- head(keys(MeSHFAOR, keytype="ANCESTORMESHID"))
  k1
  k2 <- head(keys(MeSHFAOR, keytype="OFFSPRINGMESHID"))
  k2
  # "select" method extracts data by using values supplied by the other method
  result1 <- select(MeSHFAOR, keys=k1, columns=c("ANCESTORMESHID", "OFFSPRINGMESHID"), keytype="ANCESTORMESHID")
  head(result1)
  result2 <- select(MeSHFAOR, keys=k2, columns=c("ANCESTORMESHID", "OFFSPRINGMESHID"), keytype="OFFSPRINGMESHID")
  head(result2)

}
\keyword{datasets}

