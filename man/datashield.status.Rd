% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datashield.status.r
\name{datashield.status}
\alias{datashield.status}
\title{Check Datashield configuration status}
\usage{
datashield.status(logins = NULL, study = NULL, directory = "~/.ssh",
  timeout = 20)
}
\arguments{
\item{logins}{A dataframe table that holds login details. This table holds five elements 
'url' (the opal url), 'user' (the user name or the certificate file path), 'password' (the user password or the private key file path),
'table' (the fully qualified name of the table in opal). An additional column 'identifiers' can be specified for identifiers
mapping (from Opal 2.0).
See also the documentation of the examplar input table \code{logindata} for details of the login 
elements.}

\item{study}{Limit the status inspection to one or more studies which name is specified by this parameter.}

\item{directory}{Where to look for certificate and private key. Default is user's .ssh folder.}

\item{timeout}{Time in seconds after which a request must have been completed. 0 (zero) means it never times out during transfer. Default is 20 seconds.}
}
\value{
A list of various system status
}
\description{
This function allows for clients to check each server for data access, configuration and versions.
}
\details{
Get the status of the table(s), method(s), and packgage(s) in differents Opal servers.
}
\examples{
\dontrun{

#### The below examples illustrate an analysises that use test/simulated data ####

# build your data.frame
server <- c("study1", "study2")
url <- c("https://some.opal.host:8443","https://another.opal.host")
user <- c("user1", "datashield-certificate.pem")
password <- c("user1pwd", "datashield-private.pem")
table <- c("store.Dataset","foo.DS")
logindata <- data.frame(server,url,user,password,table)

# or load the data.frame that contains the login details
data(logindata)

# Example 1: check all servers (default)
datashield.status(logins=logindata)

# Example 2: check a list of named servers
datashield.status(logins=logindata,study=c("study1"))

}

}
\author{
Mbatchou, S.
}
