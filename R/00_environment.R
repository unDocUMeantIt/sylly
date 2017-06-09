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

## setting up the internal environment

# empty environment for hyphenation information
.sylly.env <- new.env()

# non-exported function to generate internal objects when the package gets loaded
.onLoad <- function(...) {
  # we're safe for words up to 50 characters for hyphenation; if longer
  # words are found, the cache will be adjusted automatically by
  # the private function explode.word()
  hyph.max.token.length <- 50L
  assign("hyph.max.token.length", hyph.max.token.length, envir=as.environment(.sylly.env))
  # generate internal object with all possible patterns of subcharacters
  # for hyphenation, to speed up the process
  all.patterns <- explode.letters()
  assign("all.patterns", all.patterns, envir=as.environment(.sylly.env))
}
