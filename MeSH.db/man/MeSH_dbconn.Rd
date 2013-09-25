
\name{MeSH_dbconn}

\alias{MeSH_dbconn}
\alias{MeSH_dbfile}
\alias{MeSH_dbschema}
\alias{MeSH_dbInfo}


\title{Collect information about the package annotation DB}

\description{
  Some convenience functions for getting a connection object to (or collecting
  information about) the package annotation DB.
}

\usage{
  MeSH_dbconn()
  MeSH_dbfile()
  MeSH_dbschema()
  MeSH_dbInfo()
}

\details{
  \code{MeSH_dbconn} returns a connection object to the
  package annotation DB.  IMPORTANT: Don't call
  \code{\link[DBI:dbConnect]{dbDisconnect}} on the connection object
  returned by \code{MeSH_dbconn} or you will break all the
  \code{\link[AnnotationDbi:AnnDbObj-class]{AnnDbObj}} objects defined
  in this package!

  \code{MeSH_dbfile} returns the path (character string) to the
  package annotation DB (this is an SQLite file).

  \code{MeSH_dbschema} prints the schema definition of the
  package annotation DB.

  \code{MeSH_dbInfo} prints other information about the package
  annotation DB.
}

\value{
  \code{MeSH_dbconn}: a DBIConnection object representing an
  open connection to the package annotation DB.

  \code{MeSH_dbfile}: a character string with the path to the
  package annotation DB.

  \code{MeSH_dbschema}: none (invisible \code{NULL}).

  \code{MeSH_dbInfo}: none (invisible \code{NULL}).
}

\seealso{
  \code{\link[DBI:dbSendQuery]{dbGetQuery}},
  \code{\link[DBI:dbConnect]{dbConnect}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbconn}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbfile}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbschema}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbInfo}}
}

\examples{
  ## Count the number of rows in the "MeSHTERM" table:
  dbGetQuery(MeSH_dbconn(), "SELECT COUNT(*) FROM MeSHTERM;")

  ## The connection object returned by MeSH_dbconn() was
  ## created with:
  dbConnect(SQLite(), dbname=MeSH_dbfile(), cache_size=64000,
  synchronous=0)

  MeSH_dbschema()

  MeSH_dbInfo()
}

\keyword{utilities}
\keyword{datasets}

