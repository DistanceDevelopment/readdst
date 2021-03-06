% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_project.R
\name{convert_project}
\alias{convert_project}
\title{Convert a Distance for Windows project to be run in R}
\usage{
convert_project(project)
}
\arguments{
\item{project}{a path to a project (path to the \code{dst} file with "\code{.dst}" removed from the end of the path)}
}
\value{
an object of class \code{\link{converted_distance_analyses}} (if there are analyses defined), an object of class \code{\link{converted_distance_data}} (if no analyses are present in the project). Either way an attribute called \code{"flatfile"} is also returned with a flat version of the data.
}
\description{
Take each analysis in a Distance for Windows project and convert the model definition to an \code{mrds} model, data and data filters are also extracted and associated with the relevant models.
}
\section{Details}{

Only CDS/MCDS/MRDS analyses are supported.

Model names are as they are in Distance for Windows (so if you have nonsensical names in Distance for Windows they will be the same in R).
}

\seealso{
converted_distance_analyses readdst-package
}
\author{
David L Miller
}
