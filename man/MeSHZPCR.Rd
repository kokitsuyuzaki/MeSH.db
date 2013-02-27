\name{MeSHZPCR}
\alias{MeSHZPCR}
\title{Annotation corresponding table of MeSH Identifiers of Parent-Child Relationships in "Geographical Locations" Category} 
\description{
  This data set describes associations between MeSH "Geographical Locations" (Z as MeSH Tree Number)
 child terms and their parent Z terms. PCR is an abbreviation of Parent-Child Relationships. The format is a dataframe, corresponding table of the MeSH Z child terms and those parent terms.
}
\details{
  Each child MeSH Z term is assigned to some ancestor MeSH Z terms. Contorary, each parent MeSH Z term is assigned to some child MeSH Z terms.
    
  Corresponding tables were based on data provided by: 2013 Medical Subject Headings
  \url{http://www.nlm.nih.gov/mesh/filelist.html}  
  with a date stamp from the source of: 20120907

}
\references{
\url{http://www.ncbi.nlm.nih.gov/mesh/1000090} and
\url{http://www.nlm.nih.gov/mesh/filelist.html} 
}
\examples{
  # "cols" method tells you about what kinds of values you can retrive as columns
  cols(MeSHZPCR)
  # "keytypes" method tells you the kinds of things that can be used as keys
  keytypes(MeSHZPCR)
  # "keys" method is used to retrive all the viable keys of a particular type
  k1 <- head(keys(MeSHZPCR, keytype="PARENTMESHID"))
  k1
  k2 <- head(keys(MeSHZPCR, keytype="CHILDMESHID"))
  k2
  # "select" method extracts data by using values supplied by the other method
  result1 <- select(MeSHZPCR, keys=k1, cols=c("PARENTMESHID", "CHILDMESHID"), keytype="PARENTMESHID")
  head(result1)
  result2 <- select(MeSHZPCR, keys=k2, cols=c("PARENTMESHID", "CHILDMESHID"), keytype="CHILDMESHID")
  head(result2)
}
\keyword{datasets}

