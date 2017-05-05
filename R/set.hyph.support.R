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


#' Add support for new languages
#' 
#' You can use this function to add new languages to be used with \code{sylly}.
#' 
#' Language support in this package is designed to be extended easily. You could call it modular,
#' although it's actually more "environemntal", but nevermind.
#' 
#' To add new language support, say for Xyzedish, you basically have to call this function
#' once and provide respective hyphenation patterns. If you would like to re-use this language
#' support, you should consider making it a package.
#' 
#' If it succeeds, it will fill an internal environment with the information you have defined.
#' \code{hyphen} will then know which language patterns are available as data files (which
#' you must provide also). 
#' 
#' You provide the meta data as a named list. It usually has one single entry to tell the new language
#' abbreviation, e.g., \code{set.hyph.support(list("xyz"="xyz"))}. However, this will only work if a)
#' the language support script is a part of the \code{sylly} package itself, and b) the hyphen pattern
#' is located in its \code{data} subdirectory.
#' 
#' For your custom hyphenation patterns to be found automatically, provide it as the value in the named
#' list, e.g., \code{set.hyph.support(list("xyz"=hyph.xyz))}.
#' This will directly add the patterns to \code{sylly}'s environment, so it will be found when
#' hyphenation is requested for language \code{"xyz"}.
#' 
#' If you would like to provide hyphenation as part of a third party language package, you must name the
#' object \code{hyph.<lang>}, save it to your package's \code{data} subdirectory named
#' \code{hyph.<lang>.rda}, and append \code{package="<yourpackage>"} to the named list; e.g.,
#' \code{set.hyph.support(list("xyz"=c("xyz", package="koRpus.lang.xyz"))}. Only then
#' \code{sylly} will look for the pattern object in your package, not its own \code{data} directory.
#' 
#' @section Hyphenation patterns:
#' 
#' To be able to also do syllable count with the newly added language, you should add a hyphenation pattern
#' file as well.
#' Refer to the documentation of read.hyph.pat() to learn how to produce a pattern object from a downloaded
#' hyphenation pattern file. Make sure you use the correct name scheme (e.g. "hyph.xyz.rda") and good
#' compression.
#' 
#' @param value A named list that upholds exactly the structure defined above.
#' @examples
#' \dontrun{
#' set.hyph.support(
#'   list("xyz"="xyz")
#' )
#' }
#' @export
set.hyph.support <- function(value){

  all.kRp.env <- as.list(as.environment(.sylly.env))

  recent.pattern <- all.kRp.env[["langSup"]][["hyphen"]][["supported"]]
  # could be there is no such entries in the environment yet
  if(is.null(recent.pattern)){
    recent.pattern <- list()
  } else {}
  # to be safe do this as a for loop; this should replace older entries
  # but keep all other intact or just add new ones
  for (this.pattern in names(value)){
    if(inherits(value[[this.pattern]], "kRp.hyph.pat")){
      # we got a pattern object, directly add it to the environment
      recent.pattern[[this.pattern]] <- this.pattern
      assign(paste0("hyph.", this.pattern), value[[this.pattern]], envir=as.environment(.sylly.env))
    } else {
      recent.pattern[[this.pattern]] <- value[[this.pattern]]
    }
  }
  all.kRp.env[["langSup"]][["hyphen"]][["supported"]] <- recent.pattern

  list2env(all.kRp.env, envir=as.environment(.sylly.env))
  return(invisible(NULL))
}
