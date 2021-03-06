% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/boundary.R
\name{boundary}
\alias{boundary}
\alias{endsqwith}
\alias{full}
\alias{partial}
\alias{sentence}
\alias{startswith}
\alias{word}
\title{Define boundary for pattern matching}
\usage{
boundary(object, type = c("partial", "full", "word", "sentence", "line",
  "starts_with", "ends_with"))

full(object)

partial(object)

word(object)

sentence(object)

startswith(object)

endsqwith(object)
}
\arguments{
\item{object}{target or pattern for search}

\item{type}{character; one of parial (default), full, word, sentence, line,
                       starts_with, ends_with

Sets the options for matching at specified boundaries.

Boundaries may also be supplied to by pattern types; regex, fixed, coll, ,,,
When declared explicitly, these take precedent.

Except when the boundad}
}
\description{
Sets boundary type matching
}
\examples{
# -tk
}
\seealso{
# -tk
}

