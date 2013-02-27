\name{MeSHEAOR}
\alias{MeSHEAOR}
\title{Annotation corresponding table of MeSH Identifiers of Ancestor-Offspring Relationships in "Analytical, Diagnostic and Therapeutic Techniques and Equipment" Category} 
\description{
  This data set describes associations between MeSH "Analytical, Diagnostic and Therapeutic Techniques and Equipment" (E as MeSH Tree Number)
 offspring terms and their ancestor E terms. AOR is an abbreviation of Ancester-Offspring Relationships. The format is a dataframe, corresponding table of the MeSH E offspring terms and those ancestor terms.
}
\details{
  Each offspring MeSH E term is assigned to some ancestor MeSH E terms. Contorary, each ancestor MeSH E term is assigned to some offspring MeSH E terms.
    
  Corresponding tables were based on data provided by: 2013 Medical Subject Headings
  \url{http://www.nlm.nih.gov/mesh/filelist.html}  
  with a date stamp from the source of: 20120907

}
\references{
\url{http://www.ncbi.nlm.nih.gov/mesh/1000069} and
\url{http://www.nlm.nih.gov/mesh/filelist.html} 
}
\examples{
  # "cols" method tells you about what kinds of values you can retrive as columns
  cols(MeSHEAOR)
  # "keytypes" method tells you the kinds of things that can be used as keys
  keytypes(MeSHEAOR)
  # "keys" method is used to retrive all the viable keys of a particular type
  k1 <- head(keys(MeSHEAOR, keytype="ANCESTORMESHID"))
  k1
  k2 <- head(keys(MeSHEAOR, keytype="OFFSPRINGMESHID"))
  k2
  # "select" method extracts data by using values supplied by the other method
  result1 <- select(MeSHEAOR, keys=k1, cols=c("ANCESTORMESHID", "OFFSPRINGMESHID"), keytype="ANCESTORMESHID")
  head(result1)
  result2 <- select(MeSHEAOR, keys=k2, cols=c("ANCESTORMESHID", "OFFSPRINGMESHID"), keytype="OFFSPRINGMESHID")
  head(result2)

}
\keyword{datasets}

