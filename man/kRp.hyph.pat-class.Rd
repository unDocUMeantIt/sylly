% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_class_01_kRp.hyph.pat.R
\docType{class}
\name{kRp.hyph.pat,-class}
\alias{kRp.hyph.pat,-class}
\alias{kRp_hyph_pat}
\alias{kRp.hyph.pat-class}
\title{S4 Class kRp.hyph.pat}
\description{
This class is used for objects that are returned by \code{\link[sylly:read.hyph.pat]{read.hyph.pat}}.
}
\details{
Since this package has been a part of the \code{koRpus} package before,
      you might run into old pattern
files. You will know that this is the case if using them automatically tries to load the \code{koRpus} package.
In these cases,
      you might want to strip the defunct reference to \code{koRpus} by calling the private
function \code{sylly:::koRpus2sylly} which take the path to the old file as its first argument. Be aware that
calling this function will overwrite the old file in-place,
      so you should make a backup first!
}
\section{Slots}{

\describe{
\item{\code{lang}}{A character string,
      naming the language that is assumed for the patterns in this object}

\item{\code{pattern}}{A matrix with three colums:
\describe{
  \item{\code{orig}:}{The unchanged patgen patterns.}
  \item{\code{char}:}{Only the characters used for matching.}
  \item{\code{nums}:}{The hyphenation number code for the pattern.}
}}
}}

\section{Contructor function}{

Should you need to manually generate objects of this class (which should rarely be the case),
      the contructor function 
\code{kRp_hyph_pat(...)} can be used instead of
\code{new("kRp.hyph.pat", ...)}. Whenever possible, stick to
\code{\link[sylly:read.hyph.pat]{read.hyph.pat}}.
}

\keyword{classes}
