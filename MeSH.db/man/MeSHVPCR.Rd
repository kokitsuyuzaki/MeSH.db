\name{MeSHVPCR}
\alias{MeSHVPCR}
\title{Annotation corresponding table of MeSH Identifiers of Parent-Child Relationships in "Publication Type" Category} 
\description{
  This data set describes associations between MeSH "Publication Type" (V as MeSH Tree Number)
 child terms and their parent V terms. PCR is an abbreviation of Parent-Child Relationships. The format is a dataframe, corresponding table of the MeSH V child terms and those parent terms.
}
\details{
  Each child MeSH V term is assigned to some ancestor MeSH V terms. Contorary, each parent MeSH V term is assigned to some child MeSH V terms.
    
  Corresponding tables were based on data provided by: 2013 Medical Subject Headings
  \url{http://www.nlm.nih.gov/mesh/filelist.html}  
  with a date stamp from the source of: 20120907

}
\references{
\url{http://www.ncbi.nlm.nih.gov/mesh/1000086} and
\url{http://www.nlm.nih.gov/mesh/filelist.html} 
}
\examples{
  # "columns" method tells you about what kinds of values you can retrive as columns
  columns(MeSHVPCR)
  # "keytypes" method tells you the kinds of things that can be used as keys
  keytypes(MeSHVPCR)
  # "keys" method is used to retrive all the viable keys of a particular type
  k1 <- head(keys(MeSHVPCR, keytype="PARENTMESHID"))
  k1
  k2 <- head(keys(MeSHVPCR, keytype="CHILDMESHID"))
  k2
  # "select" method extracts data by using values supplied by the other method
  result1 <- select(MeSHVPCR, keys=k1, columns=c("PARENTMESHID","CHILDMESHID"), keytype="PARENTMESHID")
  head(result1)
  result2 <- select(MeSHVPCR, keys=k2, columns=c("PARENTMESHID","CHILDMESHID"), keytype="CHILDMESHID")
  head(result2)
}
\keyword{datasets}

