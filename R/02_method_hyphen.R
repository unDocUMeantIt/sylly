# Copyright 2010-2017 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package sylly.
#
# sylly is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# sylly is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with sylly.  If not, see <http://www.gnu.org/licenses/>.


#' Automatic hyphenation
#'
#' These methods implement word hyphenation, based on Liang's algorithm.
#'
#' For this to work the function must be told which pattern set it should use to
#' find the right hyphenation spots. If \code{words} is an object that was tokenized and tagged with
#' the \code{koRpus} package, its language definition might be used. Otherwise, in addition to the
#' words to be processed you must specify \code{hyph.pattern}. You have two options: If you
#' want to use one of the built-in language patterns, just set it to the according
#' language abbrevation. As of this version valid choices are:
#' \itemize{
#'  \item {\code{"de"}} {--- German (new spelling, since 1996)}
#'  \item {\code{"de.old"}} {--- German (old spelling, 1901--1996)}
#'  \item {\code{"en"}} {--- English (UK)}
#'  \item {\code{"en.us"}} {--- English (US)}
#'  \item {\code{"es"}} {--- Spanish}
#'  \item {\code{"fr"}} {--- French}
#'  \item {\code{"it"}} {--- Italian}
#'  \item {\code{"ru"}} {--- Russian}
#' }
#' In case you'd rather use your own pattern set, \code{hyph.pattern} can be an
#' object of class \code{kRp.hyph.pat}, alternatively.
#'
#' The built-in hyphenation patterns were derived from the patterns available on CTAN[1]
#' under the terms of the LaTeX Project Public License[2], see \code{\link[sylly:hyph.XX]{hyph.XX}}
#' for detailed information.
#'
#' @param words A character vector with words/tokens to be hyphenated.
#' @param hyph.pattern Either an object of class \code{\link[sylly]{kRp.hyph.pat-class}}, or
#'    a valid character string naming the language of the patterns to be used. See details.
#' @param min.length Integer, number of letters a word must have for considering a hyphenation. \code{hyphen} will
#'    not split words after the first or before the last letter, so values smaller than 4 are not useful.
#' @param rm.hyph Logical, whether appearing hyphens in words should be removed before pattern matching.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#' @param cache Logical. \code{hyphen()} can cache results to speed up the process. If this option is set to \code{TRUE}, the
#'    current cache will be queried and new tokens also be added. Caches are language-specific and reside in an environment,
#'    i.e., they are cleaned at the end of a session. If you want to save these for later use, see the option \code{hyph.cache.file}
#'    in \code{\link[sylly:set.sylly.env]{set.sylly.env}}.
#' @return An object of class \code{\link[sylly]{kRp.hyphen-class}}
#' @keywords hyphenation
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @seealso
#'    \code{\link[sylly:read.hyph.pat]{read.hyph.pat}},
#'    \code{\link[sylly:manage.hyph.pat]{manage.hyph.pat}}
#' @references
#'  Liang, F.M. (1983). \emph{Word Hy-phen-a-tion by Com-put-er}.
#'      Dissertation, Stanford University, Dept. of Computer Science.
#'
#' [1] \url{http://tug.ctan.org/tex-archive/language/hyph-utf8/tex/generic/hyph-utf8/patterns/}
#'
#' [2] \url{http://www.ctan.org/tex-archive/macros/latex/base/lppl.txt}
#' @export
#' @import methods
#' @rdname hyphen-methods
#' @examples
#' \dontrun{
#' hyphen(tagged.text)
#' }

#################################################################
## if this signature changes, check kRp.hyphen.calc() as well! ##
#################################################################
#' @param ... Only used for the method generic.
setGeneric("hyphen", function(words, ...) standardGeneric("hyphen"))

#' @export
#' @aliases hyphen,character-method
#' @rdname hyphen-methods
setMethod("hyphen", signature(words="character"), function(words,
    hyph.pattern=NULL, min.length=4, rm.hyph=TRUE, quiet=FALSE, cache=TRUE){

    results <- kRp.hyphen.calc(words=words, hyph.pattern=hyph.pattern, min.length=min.length,
      rm.hyph=rm.hyph, quiet=quiet, cache=cache)

    return(results)
  }
)
