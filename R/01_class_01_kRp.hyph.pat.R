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

#' S4 Class kRp.hyph.pat
#'
#' This class is used for objects that are returned by \code{\link[sylly:read.hyph.pat]{read.hyph.pat}}.
#' 
#' Since this package has been a part of the \code{koRpus} package before, you might run into old pattern
#' files. You will know that this is the case if using them automatically tries to load the \code{koRpus} package.
#' In these cases, you might want to strip the defunct reference to \code{koRpus} by calling the private
#' function \code{sylly:::koRpus2sylly} which take the path to the old file as its first argument. Be aware that
#' calling this function will overwrite the old file in-place, so you should make a backup first!
#'
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
#' \code{kRp_hyph_pat(...)} can be used instead of
#' \code{new("kRp.hyph.pat", ...)}. Whenever possible, stick to
#' \code{\link[sylly:read.hyph.pat]{read.hyph.pat}}.
#' 
#' @slot lang A character string, naming the language that is assumed for the patterns in this object
#' @slot pattern A matrix with three colums:
#'    \describe{
#'      \item{\code{orig}:}{The unchanged patgen patterns.}
#'      \item{\code{char}:}{Only the characters used for matching.}
#'      \item{\code{nums}:}{The hyphenation number code for the pattern.}
#'    }
#' @name kRp.hyph.pat,-class
#' @aliases kRp.hyph.pat-class
#' @import methods
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @export kRp_hyph_pat
#' @exportClass kRp.hyph.pat
#' @rdname kRp.hyph.pat-class

kRp_hyph_pat <- setClass("kRp.hyph.pat",
    representation=representation(
    lang="character",
    pattern="matrix"),
  prototype(
    lang=character(),
    pattern=matrix(c(character(),character(),character()), ncol=3, dimnames=list(c(), c("orig", "char", "nums"))))
)

setValidity("kRp.hyph.pat", function(object){
  lang <- slot(object, "lang")
  pattern <- slot(object, "pattern")

  pattern.names <- dimnames(pattern)[[2]]

  if(length(lang) > 1){
    stop(simpleError("Invalid object: Slot \"lang\" mustn't have more than one entry."))
  } else {}

  if(!identical(pattern.names, c("orig", "char", "nums"))){
    stop(simpleError("Invalid object: Wrong column names in slot \"pattern\"."))
  } else {}

  return(TRUE)
})

# this internally used S4 class is an optimized version of kRp.hyph.pat,
# using an environment instead of a matrix for the pattern slot for better speed
kRp_hyph_pat_env <- setClass("kRp.hyph.pat.env",
  representation=representation(
    lang="character",
    min.pat="numeric",
    max.pat="numeric",
    pattern="environment"
  ),
  prototype(
    lang=character(),
    min.pat=numeric(),
    max.pat=numeric(),
    pattern=new.env()
  )
)
