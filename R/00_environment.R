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

## setting up the internal environment

# empty environment for hyphenation information
.sylly.env <- new.env()

# non-exported function to generate internal objects when the package gets loaded
.onLoad <- function(...) {
  # we're safe for words up to 50 characters for hyphenation; if longer
  # words are found, the cache will be adjusted automatically by
  # the private function explode.word()
  if(is.null(getOption("sylly"))){
    # case one: no sylly options at all, that's easy
    options(sylly=list(hyph.max.token.length=50L))
  } else {
    sylly_options <- getOption("sylly", list())
    if(is.null(sylly_options[["hyph.max.token.length"]])){
      # case two, we have options, but hyph.max.token.length is not set
      sylly_options[["hyph.max.token.length"]] <- 50L
      options(sylly=sylly_options)
    } else {
      if(!all(
        is.numeric(sylly_options[["hyph.max.token.length"]]),
        isTRUE(length(sylly_options[["hyph.max.token.length"]]) == 1))
      ){
        # case three, hyph.max.token.length is set but invalid
        simpleError("check your environment: 'sylly$hyph.max.token.length' must be a single numeric value!")
      } else {}
    }
  }
  # generate internal object with all possible patterns of subcharacters
  # for hyphenation, to speed up the process
  all.patterns <- explode.letters()
  assign("all.patterns", all.patterns, envir=as.environment(.sylly.env))
}
