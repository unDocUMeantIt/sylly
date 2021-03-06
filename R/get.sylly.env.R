# Copyright 2010-2018 Meik Michalke <meik.michalke@hhu.de>
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


#' Get sylly session settings
#'
#' The function \code{get.sylly.env} returns information on your session environment regarding the sylly package, e.g.
#' whether a cache file should be used, if it was set before using \code{\link[sylly:set.sylly.env]{set.sylly.env}}.
#'
#' For the most part, \code{get.sylly.env} is a convenient wrapper for \code{\link[base:getOption]{getOption}}.
#'
#' @param ... Named parameters to get from the sylly environment. Valid arguments are:
#'   \describe{
#'     \item{lang}{ Logical, whether the set language should be returned.}
#'     \item{hyph.cache.file}{ Logical, whether the set hyphenation cache file for \code{hyphen} should be returned.}
#'     \item{hyph.max.token.length}{ Logical, whether the set maximum token length should be returned.}
#'   }
#' @param errorIfUnset Logical, if \code{TRUE} and the desired property is not set at all, the function will fail with an error message.
#' @return A character string or list, possibly including:
#'  \item{lang}{The specified language}
#'  \item{hyph.cache.file}{The specified hyphenation cache file for \code{hyphen}}
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @seealso \code{\link[sylly:set.sylly.env]{set.sylly.env}}
#' @export
#' @examples
#' set.sylly.env(hyph.cache.file=file.path(tempdir(), "cache_file.RData"))
#' get.sylly.env(hyph.cache.file=TRUE)

get.sylly.env <- function(..., errorIfUnset=TRUE){
  sylly.vars <- list(...)
  # set all desired variables
  lang <- sylly.vars[["lang"]]
  hyph.cache.file <- sylly.vars[["hyph.cache.file"]]
  hyph.max.token.length <- sylly.vars[["hyph.max.token.length"]]
  if (all(is.null(lang), is.null(hyph.cache.file), is.null(hyph.max.token.length))){
    stop(simpleError("You must at least set one (valid) parameter!"))
  } else {}
  if(!all(is.logical(unlist(sylly.vars)))){
    stop(simpleError("You can only use logical values to query parameters!"))
  } else {}

  # get current settings from .Options
  sylly_options <- getOption("sylly", list())
  tt.env <- list()

  if(isTRUE(lang)){
    if("lang" %in% names(sylly_options)){
      tt.env[["lang"]] <- sylly_options[["lang"]]
    } else {
      if(isTRUE(errorIfUnset)){
        stop(simpleError("No language specified!"))
      } else {}
    }
  } else {}

  if(isTRUE(hyph.cache.file)){
    if("hyph.cache.file" %in% names(sylly_options)){
      tt.env[["hyph.cache.file"]] <- sylly_options[["hyph.cache.file"]]
    } else {
      if(isTRUE(errorIfUnset)){
        stop(simpleError("No hyphenation cache file specified!"))
      } else {}
    }
  } else {}

  if(isTRUE(hyph.max.token.length)){
    if("hyph.max.token.length" %in% names(sylly_options)){
      tt.env[["hyph.max.token.length"]] <- sylly_options[["hyph.max.token.length"]]
    } else {
      if(isTRUE(errorIfUnset)){
        stop(simpleError("No maximum word length specified!"))
      } else {}
    }
  } else {}

  if(length(tt.env) == 1){
    tt.env <- tt.env[[1]]
  } else if(length(tt.env) < 1){
    tt.env <- NULL
  } else {}

  return(tt.env)
}
