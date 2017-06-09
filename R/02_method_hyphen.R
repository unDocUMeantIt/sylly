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
#' find the right hyphenation spots. The most straight forward way to add suuport
#' for a particular language during a session is to load  the appropriate language
#' package (e.g., the package \code{sylly.en} for English or \code{sylly.de} for German).
#'
#' After such a package was loaded, you can simply use the language abbreviation as
#' the value for the \code{hyph.pattern} argument (like \code{"en"} for the English
#' pattern set). If \code{words} is an object that was tokenized and tagged with
#' the \code{koRpus} package, its language definition can be used instead, i.e. you
#' don't need to specify \code{hyph.pattern}, \code{hyphen} will pick the language
#' automatically.
#'
#' In case you'd rather use your own pattern set, \code{hyph.pattern} can be an
#' object of class \code{kRp.hyph.pat}, alternatively.
#'
#' @param words Either a character vector with words/tokens to be hyphenated, or any tagged text object generated with the \code{koRpus} package.
#' @param hyph.pattern Either an object of class \code{\link[sylly]{kRp.hyph.pat-class}}, or
#'    a valid character string naming the language of the patterns to be used (must already be loaded, see details).
#' @param min.length Integer, number of letters a word must have for considering a hyphenation. \code{hyphen} will
#'    not split words after the first or before the last letter, so values smaller than 4 are not useful.
#' @param rm.hyph Logical, whether appearing hyphens in words should be removed before pattern matching.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#' @param cache Logical. \code{hyphen()} can cache results to speed up the process. If this option is set to \code{TRUE}, the
#'    current cache will be queried and new tokens also be added. Caches are language-specific and reside in an environment,
#'    i.e., they are cleaned at the end of a session. If you want to save these for later use, see the option \code{hyph.cache.file}
#'    in \code{\link[sylly:set.sylly.env]{set.sylly.env}}.
#' @param as A character string defining the class of the object to be returned. Defaults to \code{"kRp.hyphen"}, but can also be
#'    set to \code{"data.frame"} or \code{"numeric"}, returning only the central \code{data.frame} or the numeric vector of counted syllables,
#'    respectively. For the latter two options, you can alternatively use the shortcut methods \code{hyphen_df} or  \code{hyphen_c}.
#' @return An object of class \code{\link[sylly]{kRp.hyphen-class}}, \code{data.frame} or a numeric vector, depending on the value
#'    of the \code{as} argument.
#' @keywords hyphenation
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @seealso
#'    \code{\link[sylly:read.hyph.pat]{read.hyph.pat}},
#'    \code{\link[sylly:manage.hyph.pat]{manage.hyph.pat}}
#' @references
#'  Liang, F.M. (1983). \emph{Word Hy-phen-a-tion by Com-put-er}.
#'      Dissertation, Stanford University, Dept. of Computer Science.
#' @export
#' @import methods
#' @rdname hyphen-methods
#' @examples
#' \dontrun{
#' library(sylly.en)
#' sampleText <- c("This", "is", "a", "rather", "stupid", "demonstration")
#' hyphen(sampleText, hyph.pattern="en")
#' hyphen_df(sampleText, hyph.pattern="en")
#' hyphen_c(sampleText, hyph.pattern="en")
#'
#' # using a koRpus object
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
    hyph.pattern=NULL, min.length=4, rm.hyph=TRUE, quiet=FALSE, cache=TRUE, as="kRp.hyphen"){

    results <- kRp.hyphen.calc(words=words, hyph.pattern=hyph.pattern, min.length=min.length,
      rm.hyph=rm.hyph, quiet=quiet, cache=cache, as=as)

    return(results)
  }
)

#' @export
#' @aliases hyphen_df,-methods
#' @rdname hyphen-methods
setGeneric("hyphen_df", function(words, ...) standardGeneric("hyphen_df"))
#' @export
#' @aliases hyphen_df,character-method
#' @rdname hyphen-methods
setMethod("hyphen_df", signature(words="character"), function(words,
    hyph.pattern=NULL, min.length=4, rm.hyph=TRUE, quiet=FALSE, cache=TRUE){

    results <- hyphen(words=words, hyph.pattern=hyph.pattern, min.length=min.length,
      rm.hyph=rm.hyph, quiet=quiet, cache=cache, as="data.frame")

    return(results)
  }
)

#' @export
#' @aliases hyphen_c,-methods
#' @rdname hyphen-methods
setGeneric("hyphen_c", function(words, ...) standardGeneric("hyphen_c"))
#' @export
#' @aliases hyphen_c,character-method
#' @rdname hyphen-methods
setMethod("hyphen_c", signature(words="character"), function(words,
    hyph.pattern=NULL, min.length=4, rm.hyph=TRUE, quiet=FALSE, cache=TRUE){

    results <- hyphen(words=words, hyph.pattern=hyph.pattern, min.length=min.length,
      rm.hyph=rm.hyph, quiet=quiet, cache=cache, as="numeric")

    return(results)
  }
)
