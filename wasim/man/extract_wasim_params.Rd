\name{extract_wasim_params}
\alias{extract_wasim_params}
\title{ Extract Parameters from WaSiM-Control-File }
\description{
  retrieves desired parameters (currently limited to soil\_model and snow\_model) from WASIM control file
}
\usage{
params=extract_wasim_params(wasim_param_file,param_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wasim_param_file}{ Path to WASIM control file (see details).}
  \item{param_list}{ List of parameters to be extracted from the file.
  Currently supported:(all lowercase)
  m,tkorr,kkorr,kd,hmax,kh,cmelt,t0r,t0,c0
  }
}
\details{
  The ordinary WASIM control file (*.ctrl) may be used as wasim\_param\_file. However, if this files uses internal variables (starting with \$), it is more safe to use the file *.\$\$\$ instead (produced after calling WASIM).
}
\value{
  Dataframe params contains the fields specified in param\_list and their respective values.
}
\references{  }
\author{ Till Francke }
\seealso{  }
\examples{
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ file }
