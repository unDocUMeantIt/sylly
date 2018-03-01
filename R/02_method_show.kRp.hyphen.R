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


#' Show method for sylly objects
#'
#' Show method for S4 objects of class \code{\link[sylly:kRp.hyphen-class]{kRp.hyphen}}.
#'
#' @param object An object of class \code{kRp.hyphen}.
#' @aliases show show,kRp.hyphen-method
#' @seealso
#'    \code{\link[sylly:kRp.hyphen-class]{kRp.hyphen}}
#' @keywords methods
#' @export
#' @docType methods
#' @rdname show-methods
#' @include 01_class_02_kRp.hyphen.R
#' @examples
#' \dontrun{
#'   hyphen(tagged.text)
#' }
setMethod("show", signature(object="kRp.hyphen"), function(object){
  hyph <- slot(object, "hyphen")
  if(nrow(hyph) > 15){
    middle <- data.frame(syll=NA, word="[...]    ", row.names="", stringsAsFactors=FALSE)
    show.mtx <- rbind(head(hyph), middle, tail(hyph))
    show(show.mtx)
  } else {
    show(hyph)
  }
})
