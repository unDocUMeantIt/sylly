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


#' Summary method for sylly objects
#'
#' Summary method for S4 objects of class \code{\link[sylly]{kRp.hyphen-class}}.
#'
#' @param object An object of class \code{kRp.hyphen}.
#' @seealso
#'    \code{\link[sylly]{kRp.hyphen-class}}
#' @keywords methods
#' @examples
#' \dontrun{
#' summary(hyphen(tagged.text))
#' }
#' @rdname summary-methods
#' @include 01_class_02_kRp.hyphen.R
#' @aliases summary summary,kRp.hyphen-method
#' @export
#' @docType methods
setMethod("summary", signature(object="kRp.hyphen"), function(object){
  desc <- slot(object, "desc")
  
  distrib <- object@desc[["syll.distrib"]]
  distrib.eqnp <- round(t(distrib[c("num","pct"),]))
  rownames(distrib.eqnp) <- paste0("   = ", rownames(distrib.eqnp))
  distrib.gtnp <- round(t(distrib[c("cum.inv","pct.inv"),]))
  colnames(distrib.gtnp) <- c("num","pct")
  rownames(distrib.gtnp) <- paste0("   > ", rownames(distrib.gtnp))
  distrib.lenp <- round(t(distrib[c("cum.sum","cum.pct"),]))
  colnames(distrib.lenp) <- c("num","pct")
  rownames(distrib.lenp) <- paste0("  <= ", rownames(distrib.lenp))
  
  cat(
  "\n  Syllables: ", desc[["num.syll"]], " (", round(desc[["avg.syll.word"]], digits=2), " per word)\n\n",
  "  Distribution of syllables:\n\n",
  sep="")

  print(distrib.eqnp)
  cat("\n")
  print(distrib.gtnp)
  cat("\n")
  print(distrib.lenp)
  
  return(invisible(t(distrib)))
})
