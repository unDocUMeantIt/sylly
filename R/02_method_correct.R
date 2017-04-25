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


#' Correct kRp.hyphen objects
#' 
#' The method \code{correct.hyph} can be used to alter objects of class \code{\link[sylly]{kRp.hyphen-class}}.
#'
#' Although hyphenation should turn out to be rather accurate, the algorithm does ususally produce
#' some errors. If you want to correct for these flaws, this method can be of help, because it might prevent you from
#' introducing new errors. That is, it will do some sanitiy checks before the object is actually manipulated and returned.
#'
#' That is, \code{correct.hyph} checks whether \code{word} and \code{hyphen} are actually hyphenations of the
#' same token before proceeding. If so, it will also recalculate the number of syllables and update the \code{syll}
#' field.
#'
#' If both \code{word} and \code{hyphen} are \code{NULL}, \code{correct.hyph} will try to simply recalculate the syllable count
#' for each word, by counting the hyphenation marks (and adding 1 to the number). This can be usefull if you changed hyphenation
#' some other way, e.g. in a spreadsheet GUI, but don't want to have to correct the syllable count yourself as well.
#'
#' @param obj An object of class \code{\link[sylly]{kRp.hyphen-class}}.
#' @param word A character string, the (possibly incorrectly hyphenated) \code{word} entry to be replaced with \code{hyphen}.
#' @param hyphen A character string, the new manually hyphenated version of \code{word}. Mustn't contain
#'    anything other than characters of \code{word} plus the hyphenation mark \code{"-"}.
#' @param cache Logical, if \code{TRUE}, the given hyphenation will be added to the sessions' hyphenation cache.
#'    Existing entries for the same word will be replaced.
#' @return An object of the same class as \code{obj}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords methods
#' @examples
#' \dontrun{
#' hyphenated.txt <- correct.hyph(hyphenated.txt, "Hilfe", "Hil-fe")
#' }
#' @export
#' @docType methods
#' @rdname correct-methods
setGeneric("correct.hyph", function(obj, word=NULL, hyphen=NULL, cache=TRUE){standardGeneric("correct.hyph")})

#' @docType methods
#' @rdname correct-methods
#' @aliases correct.hyph correct.hyph,kRp.hyphen-method
#' @export
#' @include 01_class_02_kRp.hyphen.R
setMethod("correct.hyph",
    signature(obj="kRp.hyphen"),
    function (obj, word=NULL, hyphen=NULL, cache=TRUE){
      lang <- obj@lang
      local.obj.copy <- obj

      if(all(!is.null(word), !is.null(hyphen))){
        # recount syllables
        new.syll <- sum(grepl("-", unlist(strsplit(hyphen, split="")))) + 1

        matching.rows <- which(local.obj.copy@hyphen[, "word"] == word)
        # any matches at all?
        if(length(matching.rows) == 0){
          warning(paste0("Sorry, no matches for \"", word,"\" in ", substitute(obj), "!"), call.=FALSE)
          return(obj)
        } else {}

        # check if hyphen is actually a hyphenated version of word!
        old.word <- gsub("-", "", word)
        new.word <- gsub("-", "", hyphen)
        if(!identical(old.word, new.word)){
          stop(simpleError(paste0("\"", hyphen, "\" is not a valid hyphenation of \"", old.word, "\"!")))
        } else {}
        local.obj.copy@hyphen[matching.rows, "syll"] <- new.syll
        local.obj.copy@hyphen[matching.rows, "word"] <- hyphen

        # now check the cache
        if(isTRUE(cache)){
          # get current sylly environment
          all.kRp.env.hyph <- mget("hyphenCache", envir=as.environment(.sylly.env), ifnotfound=list(NULL))[["hyphenCache"]]
          recent.cache <- all.kRp.env.hyph[[lang]]
          # could be there is no such entries in the environment yet
          if(is.null(recent.cache)){
            recent.cache <- new.env()
          } else {}
          token <- gsub("-", "", word)
          recent.cache[[token]] <- list(syll=new.syll, word=hyphen)
          # write back the changes
          all.kRp.env.hyph[[lang]] <- recent.cache
          assign("hyphenCache", all.kRp.env.hyph, envir=as.environment(.sylly.env))
        } else {}

      } else if(is.null(word) & is.null(hyphen)){
        new.syll <- as.numeric(sapply(local.obj.copy@hyphen$word, function(word){sum(grepl("-", unlist(strsplit(word, split="")))) + 1}))
        matching.rows <- which(local.obj.copy@hyphen$syll != new.syll)
        # any mathes at all?
        if(length(matching.rows) == 0){
          message("Nothing was changed!")
          return(obj)
        } else {}
        # recount syllables
        local.obj.copy@hyphen[matching.rows, "syll"] <- new.syll[matching.rows]
      } else {
        stop(simpleError(paste("Either \"word\" or \"hyphen\" is missing!")))
      }

      # update descriptive statistics
      new.num.syll <- sum(local.obj.copy@hyphen$syll, na.rm=TRUE)
      new.syll.distrib <- value.distribs(local.obj.copy@hyphen$syll)
      new.syll.uniq.distrib <- value.distribs(unique(local.obj.copy@hyphen)$syll)
      new.avg.syll.word <- mean(local.obj.copy@hyphen$syll, na.rm=TRUE)
      new.syll.per100 <- new.avg.syll.word * 100

      local.obj.copy@desc <- list(
        num.syll=new.num.syll,
        syll.distrib=new.syll.distrib,
        syll.uniq.distrib=new.syll.uniq.distrib,
        avg.syll.word=new.avg.syll.word,
        syll.per100=new.syll.per100
      )

      cat("Changed\n\n")
      print(obj@hyphen[matching.rows, ])
      cat("\n  into\n\n")
      print(local.obj.copy@hyphen[matching.rows, ])

      return(local.obj.copy)
    }
)
