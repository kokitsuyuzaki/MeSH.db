\name{MeSHTERM}
\alias{MeSHTERM}
\title{Annotation of MeSH Identifiers to MeSH Terms}
\description{
  This data is a dataframe or corresponding table of MeSH ID, MeSH Term and MeSH Category.
}
\details{
    
  Dataframe were based on data provided by: MeSH  
  http://www.nlm.nih.gov/mesh/filelist.html
  with a date stamp from the source of: 20120907

}
\references{
  \url{http://www.nlm.nih.gov/mesh/filelist.html} 
}
\examples{
	# columns returns the columns which you can retrieve in MeSHTERM
	columns(MeSHTERM)
	# keytypes returns the columns which you can specify as keytype option in keys or select function
	keytypes(MeSHTERM)
	# keys returns the value of keytype
	k1 <- keys(MeSHTERM, keytype="MESHID")
	k2 <- keys(MeSHTERM, keytype="MESHTERM")
	k3 <- keys(MeSHTERM, keytype="MESHCATEGORY")
	# select returns the specific rows matching the conditional expression
	result1 <- select(MeSHTERM, keys=k1, columns=c("MESHID","MESHTERM","MESHCATEGORY"), keytype="MESHID")
	result2 <- select(MeSHTERM, keys=k2, columns=c("MESHID","MESHTERM","MESHCATEGORY"), keytype="MESHTERM")
	result3 <- select(MeSHTERM, keys=k3, columns=c("MESHID","MESHTERM","MESHCATEGORY"), keytype="MESHCATEGORY")
}
\keyword{datasets}

