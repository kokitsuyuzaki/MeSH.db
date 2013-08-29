\name{MeSHEPCR}
\alias{MeSHEPCR}
\title{Annotation corresponding table of MeSH Identifiers of Parent-Child Relationships in "Analytical, Diagnostic and Therapeutic Techniques and Equipment" Category} 
\description{
  This data set describes associations between MeSH "Analytical, Diagnostic and Therapeutic Techniques and Equipment" (E as MeSH Tree Number)
 child terms and their parent E terms. PCR is an abbreviation of Parent-Child Relationships. The format is a dataframe, corresponding table of the MeSH E child terms and those parent terms.
}
\details{
  Each child MeSH E term is assigned to some ancestor MeSH E terms. Contorary, each parent MeSH E term is assigned to some child MeSH E terms.
    
  Corresponding tables were based on data provided by: 2013 Medical Subject Headings
  \url{http://www.nlm.nih.gov/mesh/filelist.html}  
  with a date stamp from the source of: 20120907

}
\references{
\url{http://www.ncbi.nlm.nih.gov/mesh/1000069} and
\url{http://www.nlm.nih.gov/mesh/filelist.html} 
}
\examples{
  # "columns" method tells you about what kinds of values you can retrive as columns
  columns(MeSHEPCR)
  # "keytypes" method tells you the kinds of things that can be used as keys
  keytypes(MeSHEPCR)
  # "keys" method is used to retrive all the viable keys of a particular type
  k1 <- head(keys(MeSHEPCR, keytype="PARENTMESHID"))
  k1
  k2 <- head(keys(MeSHEPCR, keytype="CHILDMESHID"))
  k2
  # "select" method extracts data by using values supplied by the other method
  result1 <- select(MeSHEPCR, keys=k1, columns=c("PARENTMESHID", "CHILDMESHID"), keytype="PARENTMESHID")
  head(result1)
  result2 <- select(MeSHEPCR, keys=k2, columns=c("PARENTMESHID", "CHILDMESHID"), keytype="CHILDMESHID")
  head(result2)
}
\keyword{datasets}

