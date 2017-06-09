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


#' A function to set information on your sylly environment
#'
#' The function \code{set.sylly.env} can be called before any of the hyphenation functions. It preserves some information
#' on your current session's settings to a hidden environment.
#'
#' To get the contents of the hidden environment, the function \code{\link[sylly:get.sylly.env]{get.sylly.env}}
#' can be used.
#'
#' @param ... Named parameters to set in the sylly environment. Valid arguments are:
#'   \describe{
#'     \item{lang}{ A character string specifying a valid language.}
#'     \item{hyph.cache.file}{ A character string specifying a path to a file to use for storing already hyphenated data, used by \code{\link[sylly:hyphen]{hyphen}}.}
#'     \item{hyph.max.token.length}{ A single number to set the internal cache size for tokens. The value should be set to the longest token to be hyphenated.}
#'   }
#'   To explicitly unset a value again, set it to an empty character string (e.g., \code{lang=""}).
#' @param validate Logical, if \code{TRUE} given paths will be checked for actual availablity, and the function will fail if files can't be found.
#' @return Returns an invisible \code{NULL}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @seealso \code{\link[sylly:get.sylly.env]{get.sylly.env}}
#' @export
#' @examples
#' \dontrun{
#' set.sylly.env(hyph.cache.file="/tmp/cache_file.RData")
#' get.sylly.env(hyph.cache.file=TRUE)
#' }

set.sylly.env <- function(..., validate=TRUE){
  sylly.vars <- list(...)
  # set all desired variables
  lang <- sylly.vars[["lang"]]
  hyph.cache.file <- sylly.vars[["hyph.cache.file"]]
  hyph.max.token.length <- sylly.vars[["hyph.max.token.length"]]
  if (all(is.null(lang), is.null(hyph.cache.file), is.null(hyph.max.token.length))){
    stop(simpleError("You must at least set one (valid) parameter!"))
  } else {}

  if(!is.null(lang)){
    if(identical(lang, "")){
      rm("lang", envir=.sylly.env)
    } else {
      stopifnot(is.character(lang))
      assign("lang", lang, envir=.sylly.env)
    }
  } else {}

  if(!is.null(hyph.cache.file)){
    if(identical(hyph.cache.file, "")){
      rm("hyph.cache.file", envir=.sylly.env)
    } else {
      stopifnot(is.character(hyph.cache.file))
      assign("hyph.cache.file", hyph.cache.file, envir=.sylly.env)
    }
  } else {}

  if(!is.null(hyph.max.token.length)){
    stopifnot(all(is.numeric(hyph.max.token.length), isTRUE(length(hyph.max.token.length) == 1)))
    assign("hyph.max.token.length", hyph.max.token.length, envir=as.environment(.sylly.env))
    # regenerate internal object with all possible patterns of subcharacters for hyphenation
    all.patterns <- explode.letters()
    assign("all.patterns", all.patterns, envir=as.environment(.sylly.env))
  } else {}

  return(invisible(NULL))
}
