# Copyright 2017-2018 Meik Michalke <meik.michalke@hhu.de>
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

# these private functions are used to generate basic structures for
# packages that add support for new languages

## function write_doc_file()
# returns a character string or writes it to the specified file
write_doc_file <- function(
  doc,
  file,
  dir=NULL,
  overwrite=FALSE
){
  stopifnot(dir.exists(dir))
  doc_file <- file.path(dir, file)
  stopifnot(any(!file.exists(doc_file), isTRUE(overwrite)))
  message(paste0("writing file: ", doc_file, " ..."))
  cat(doc, file=doc_file)
  return(invisible(NULL))
}
## end function write_doc_file()


## function check_alt_lang()
check_alt_lang <- function(alt_lang){
    stopifnot(is.list(alt_lang))
    for(this_alt_lang in alt_lang){
      # very basic check that this is a correctly constructed entry
      stopifnot(all(isTRUE(length(this_alt_lang) == 3), names(this_alt_lang) %in% c("lang", "lang_name", "desc")))
    }
    return(TRUE)
} ## end function check_alt_lang()


## function copyright_notice()
copyright_notice <- function(
  lang,      # e.g., "nl"
  author="Meik Michalke",
  email="meik.michalke@hhu.de",
  year=format(Sys.Date(), "%Y")
){
  cpr <- paste0(
    "# Copyright ", year, " ", author, " <", email, ">\n",
    "#\n",
    "# This file is part of the R package sylly.", lang, ".\n",
    "#\n",
    "# sylly.", lang, " is free software: you can redistribute it and/or modify\n",
    "# it under the terms of the GNU General Public License as published by\n",
    "# the Free Software Foundation, either version 3 of the License, or\n",
    "# (at your option) any later version.\n",
    "#\n",
    "# sylly.", lang, " is distributed in the hope that it will be useful,\n",
    "# but WITHOUT ANY WARRANTY; without even the implied warranty of\n",
    "# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n",
    "# GNU General Public License for more details.\n",
    "#\n",
    "# You should have received a copy of the GNU General Public License\n",
    "# along with sylly.", lang, ".  If not, see <http://www.gnu.org/licenses/>.\n\n"
  )
  return(cpr)
} ## end function copyright_notice()


## function doc_hyph_support()
# generate hyph.support-<lang>.R file
doc_hyph_support <- function(
  lang,           # e.g., "nl"
  lang_name,      # e.g., "Dutch"
  desc=paste0(lang_name, " hyphenation patterns"), # description for the docs
  alt_lang=NULL,  # optional list of character vectors with elements 'lang', 'lang_name' and
                  # 'desc' for additional patterns in this package
  author="Meik Michalke",
  email="meik.michalke@hhu.de",
  year=format(Sys.Date(), "%Y"),
  dir=NULL,   # if not NULL, writes results directly to dir/hyph.<lang>-data.R
  overwrite=FALSE
){
  # check for alternative patterns
  alt_docs <- value_def <- ""
  if(!is.null(alt_lang)){
    check_alt_lang(alt_lang)
    for(this_alt_lang in alt_lang){
      alt_docs <- paste0(alt_docs,
        "#'   \\item {\\code{\"", this_alt_lang["lang"], "\"}} {--- ", this_alt_lang["desc"], "}\n"
      )
      value_def <- paste0(value_def,
        "      \"", this_alt_lang["lang"], "\"=c(\"", this_alt_lang["lang"], "\", package=\"sylly.", lang, "\")\n"
      )
    }
  }
  doc <- paste0(
    copyright_notice(lang=lang, author=author, email=email, year=year),
    "#' Language support for ", lang_name, "\n",
    "#' \n",
    "#' This function adds support for ", lang_name, " to the sylly package. You should not\n",
    "#' need to call it manually, as that is done automatically when this package is\n",
    "#' being loaded.\n",
    "#' \n",
    "#' In particular, a new set of hyphenation patterns is being added (see \\code{\\link{hyph.", lang, "}}).\n",
    "#' To use the patterns with \\code{\\link[sylly:hyphen]{hyphen}}, simply use the abbreviation:\n",
    "#' \\itemize{\n",
    "#'   \\item {\\code{\"", lang, "\"}} {--- ", desc, "}\n",
    alt_docs,
    "#' }\n",
    "#'\n",
    "#' @seealso\n",
    "#'    \\code{\\link[sylly:hyphen]{hyphen}},\n",
    "#'    \\code{\\link[sylly:set.hyph.support]{set.hyph.support}}\n",
    "#' @importFrom sylly set.hyph.support\n",
    "#' @export\n",
    "hyph.support.", lang, " <- function() {\n",
    "  # tell sylly where to find hyphenation patterns (see ?set.hyph.support for details)\n",
    "  sylly::set.hyph.support(\n",
    "    value=list(\n",
    "      \"", lang, "\"=c(\"", lang, "\", package=\"sylly.", lang, "\")\n",
    value_def,
    "    )\n",
    "  )\n",
    "}\n\n",
    "# this internal, non-exported function causes the language support to be\n",
    "# properly added when the package gets loaded\n",
    ".onAttach <- function(...) {\n",
    "  hyph.support.", lang, "()\n",
    "}\n"
  )
  
  if(!is.null(dir)){
    write_doc_file(
      doc=doc,
      file=paste0("hyph.support-", lang, ".R"),
      dir=dir,
      overwrite=overwrite
    )
    return(invisible(NULL))
  } else {
    return(doc)
  }
} ## end function doc_hyph_support()


## function doc_data()
# generate hyph.<lang>-data.R file
doc_data <- function(
  lang,           # e.g., "nl"
  lang_name,      # e.g., "Dutch"
  alt_lang=NULL,  # optional list of character vectors with elements 'lang', 'lang_name' and
                  # 'desc' for additional patterns in this package
  author="Meik Michalke",
  email="meik.michalke@hhu.de",
  year=format(Sys.Date(), "%Y"),
  dir=NULL,       # if not NULL, writes results directly to dir/hyph.<lang>-data.R
  example=c("This", "is", "a", "rather", "stupid", "demonstration"),
  overwrite=FALSE
){
  aliases <- paste0("hyph.", lang)
  if(!is.null(alt_lang)){
    check_alt_lang(alt_lang)
    for(this_alt_lang in alt_lang){
      aliases <- paste0(aliases, ", hyph.", this_alt_lang["lang"])
    }
  }
  doc <- paste0(
    copyright_notice(lang=lang, author=author, email=email, year=year),
    "#' Hyphenation patterns for ", lang_name, "\n",
    "#' \n",
    "#' Hyphenation patterns for ", lang_name, " to be used by \\code{\\link[sylly:hyphen]{hyphen}}.\n",
    "#' These data objects are not really intended to be used directly, but rather to be consulted\n",
    "#' by the \\code{hyphen()} function without further user interaction.\n",
    "#'\n",
    "#' @format The \\code{pattern} slot of each hyphenation pattern object has three colums:\n",
    "#'    \\describe{\n",
    "#'      \\item{\\code{orig}}{The original pattern in patgen style format.}\n",
    "#'      \\item{\\code{char}}{Only the character elements of the pattern which can be matched to parts of an actual word.}\n",
    "#'      \\item{\\code{nums}}{A code of digits defining the possibility to split syllables at respective places in this pattern.}\n",
    "#'    }\n",
    "#'\n",
    "#' @source The patterns (as they are present in the \\code{\"orig\"} column described above) were originally provided\n",
    "#' by the LaTeX developers[1], under the terms of the LaTeX Project Public License[2].\n",
    "#' Refer to Liang (1983) for a detailed explaination.\n",
    "#' From these original patterns the values in the remaining columns were created using\n",
    "#' \\code{\\link[sylly:read.hyph.pat]{read.hyph.pat}}.\n",
    "#'\n",
    "#' In case any changes to the patterns were necessary to be used in this package, they are\n",
    "#' documented in the ChangeLog for the sources package. The unchanged original patterns can be found under [1].\n\n",
    "#' @aliases ", aliases, "\n",
    "#' @docType data\n",
    "#' @keywords datasets\n",
    "#' @name hyph.", lang, "\n",
    "#' @usage hyph.", lang, "\n",
    "#' @seealso\n",
    "#'    \\code{\\link[sylly:read.hyph.pat]{read.hyph.pat}},\n",
    "#'    \\code{\\link[sylly:manage.hyph.pat]{manage.hyph.pat}}\n",
    "#' @references\n",
    "#' Liang, F.M. (1983). \\emph{Word Hy-phen-a-tion by Com-put-er}.\n",
    "#'   Dissertation, Stanford University, Dept. of Computer Science.\n",
    "#'\n",
    "#' [1] \\url{http://tug.ctan.org/tex-archive/language/hyph-utf8/tex/generic/hyph-utf8/patterns/}\n",
    "#'\n",
    "#' [2] \\url{http://www.ctan.org/tex-archive/macros/latex/base/lppl.txt}\n",
    "#' @examples\n",
    "#' library(sylly.", lang, ")\n",
    "#' sampleText <- c(\"", paste0(example, collapse="\", \""),"\")\n",
    "#' hyphen(sampleText, hyph.pattern=\"", lang, "\")\n",
    "NULL\n"
  )
  
  if(!is.null(dir)){
    write_doc_file(
      doc=doc,
      file=paste0("hyph.", lang, "-data.R"),
      dir=dir,
      overwrite=overwrite
    )
    return(invisible(NULL))
  } else {
    return(doc)
  }
} ## end function doc_data()

## function doc_readme()
# generate README.md file
doc_readme <- function(
  lang,      # e.g., "nl"
  lang_name, # e.g., "Dutch"
  author="Meik Michalke",
  email="meik.michalke@hhu.de",
  year=format(Sys.Date(), "%Y"),
  github_user="unDocUMeantIt",
  dir=NULL,  # if not NULL, writes results directly to dir/README.md
  overwrite=FALSE
){
  doc <- paste0(
    "# sylly.", lang, "\n\n",
    "Adds support for the ", lang_name, " language to the [sylly](https://github.com/unDocUMeantIt/sylly) package.\n\n",
    "## Installation\n\n",
    "### Installation from the official l10n repository\n\n",
    "The latest stable release can be installed directly from the project's own repository:\n\n",
    "```r\n",
    "install.packages(\n",
    "  \"sylly.", lang, "\"\n",
    "  repo=c(\n",
    "    getOption(\"repos\"),\n",
    "    l10n=\"https://undocumeantit.github.io/repos/l10n\"\n",
    "  )\n",
    ")\n",
    "```\n\n",
    "To automatically get updates, consider [adding the repository to your R configuration](https://undocumeantit.github.io/repos).\n",
    "You might also want to subscribe to the package's [RSS feed](https://undocumeantit.github.io/repos/l10n/pckg/sylly.", lang,
    "/RSS.xml) to get notified of new releases.\n\n",
    "If you're running a Debian based operating system, you might be interested in the\n",
    "[precompiled *.deb packages](https://undocumeantit.github.io/repos/l10n/pckg/sylly.", lang, "/deb_repo.html).\n\n",
    "### Installation via GitHub\n\n",
    "To install the package directly from GitHub, you can use `install_github()` from the",
    " [devtools](https://github.com/hadley/devtools) package:\n\n",
    "```r\n",
    "library(devtools)\n",
    "install_github(\"", github_user, "/sylly.", lang, "\") # stable release\n",
    "install_github(\"", github_user, "/sylly.", lang, "\", ref=\"develop\") # development version\n",
    "```\n\n",
    "## Contributing\n\n",
    "To ask for help, report bugs, suggest feature improvements, or discuss the global\n",
    "development of the package, please use the [issue tracker](https://github.com/", github_user, "/sylly.", lang, "/issues).\n\n",
    "### Branches\n\n",
    "Please note that all development happens in the `develop` branch. Pull requests against the `master`\n",
    "branch will be rejected, as it is reserved for the current stable release.\n\n",
    "## License\n\n",
    "sylly.", lang, " Copyright (C) ", year, " ", author, " <", email, ">\n\n",
    "sylly.", lang, " is free software: you can redistribute it and/or modify\n",
    "it under the terms of the GNU General Public License as published by\n",
    "the Free Software Foundation, either version 3 of the License, or\n",
    "(at your option) any later version.\n\n",
    "sylly.", lang, " is distributed in the hope that it will be useful,\n",
    "but WITHOUT ANY WARRANTY; without even the implied warranty of\n",
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n",
    "GNU General Public License for more details.\n\n",
    "You should have received a copy of the license with the\n",
    "source package as the file COPYING or LICENSE.\n",
    "If not, see [<https://www.gnu.org/licenses/>](https://www.gnu.org/licenses).\n"
  )
  
  if(!is.null(dir)){
    write_doc_file(
      doc=doc,
      file="README.md",
      dir=dir,
      overwrite=overwrite
    )
    return(invisible(NULL))
  } else {
    return(doc)
  }
} ## end function doc_readme()


## function doc_changelog()
doc_changelog <- function(
  lang,      # e.g., "nl"
  version="0.1-1",
  date=format(Sys.Date(), "%Y-%m-%d"),
  dir=NULL,  # if not NULL, writes results directly to dir/ChangeLog
  overwrite=FALSE
){
  doc <- paste0(
    "ChangeLog for package sylly.", lang, "\n\n",
    "changes in version ", version, " (", date, ")\n",
    "changed:\n",
    "  - initial public release\n"
  )

  if(!is.null(dir)){
    write_doc_file(
      doc=doc,
      file="ChangeLog",
      dir=dir,
      overwrite=overwrite
    )
    return(invisible(NULL))
  } else {
    return(doc)
  }
} ## end function doc_changelog()


## function sylly_langpack_skeleton()
# combines all generator functions into one single call
# it's a function outside of the langpack_skeleton() method (see below)
# to be able to call it without the actual data file
sylly_langpack_skeleton <- function(
  lang,      # e.g., "nl"
  lang_name, # e.g., "Dutch"
  desc=paste0(lang_name, " hyphenation patterns"), # description for the docs
  alt_lang=NULL,  # optional list of character vectors with elements 'lang', 'lang_name' and
                  # 'desc' for additional patterns in this package
  author="Meik Michalke",
  email="meik.michalke@hhu.de",
  year=format(Sys.Date(), "%Y"),
  date=format(Sys.Date(), "%Y-%m-%d"),
  version="0.1-1",
  github_user="unDocUMeantIt",
  example=c("This", "is", "a", "rather", "stupid", "demonstration"),
  dir=NULL,  # if not NULL, writes results directly to dir
  overwrite=FALSE
){
  if(!is.null(dir)){
    stopifnot(dir.exists(dir))
    main_dir <- file.path(dir, paste0("sylly.", lang))
    R_dir <- file.path(main_dir, "R")
    data_dir <- file.path(main_dir, "data")
    for(this_dir in c(R_dir, data_dir)){
      if(!dir.exists(this_dir)){
        message(paste0("creating directory: ", this_dir, " ..."))
        dir.create(this_dir, recursive=TRUE)
      } else {}
    }
  } else {
    warning("'dir' not set, nothing to do!")
    main_dir <- R_dir <- NULL
  }
  doc_hyph_support(
    lang=lang,
    lang_name=lang_name,
    desc=desc,
    alt_lang=alt_lang,
    author=author,
    email=email,
    year=year,
    dir=R_dir,
    overwrite=overwrite
  )
  doc_data(
    lang=lang,
    lang_name=lang_name,
    alt_lang=alt_lang,
    author=author,
    email=email,
    year=year,
    dir=R_dir,
    example=example,
    overwrite=overwrite
  )
  doc_changelog(
    lang=lang,
    version=version,
    date=date,
    dir=main_dir,
    overwrite=overwrite
  )
  doc_readme(
    lang=lang,
    lang_name=lang_name,
    author=author,
    email=email,
    year=year,
    github_user=github_user,
    dir=main_dir,
    overwrite=overwrite
  )
} ## end function sylly_langpack_skeleton()


## function write_hyph_rda()
write_hyph_rda <- function(hyph_pattern, dir, lang, pkg_lang=lang){
  # let's make sure the object is named accordingly
  hyph_name <- paste0("hyph.", lang)
  intEnv <- new.env()
  intEnv[[hyph_name]] <- hyph_pattern
  hyph_file <- file.path(dir, paste0("sylly.", pkg_lang), "data", paste0(hyph_name, ".rda"))
  message(paste0("writing file: ", hyph_file, " ..."))
  save(
    list=hyph_name,
    file=hyph_file,
    envir=intEnv,
    compress="xz",
    compression_level=-9
  )
} ## end function write_hyph_rda()


## method sylly_langpack()
setGeneric("sylly_langpack", function(hyph.pattern, ...) standardGeneric("sylly_langpack"))
# further down the line, this method also takes the hyphen pattern object
# and places it in the package
setMethod("sylly_langpack", signature(hyph.pattern="kRp.hyph.pat"),
  function(
    hyph.pattern, 
    lang,           # e.g., "nl"
    lang_name,      # e.g., "Dutch"
    dir,
    desc=paste0(lang_name, " hyphenation patterns"), # description for the docs
    alt_lang=NULL,  # optional list of character vectors with elements 'lang', 'lang_name' and
                    # 'desc' for additional patterns in this package
    alt_hyph_pattern=NULL,  # optional named list of objects of class kRp.hyph.pat to also include in the package
                            # name of each element must be the language abbreviation for this partidular pattern,
                            # e.g. list(de.old=hyph.de.old)
    author="Meik Michalke",
    email="meik.michalke@hhu.de",
    year=format(Sys.Date(), "%Y"),
    date=format(Sys.Date(), "%Y-%m-%d"),
    version="0.1-1",
    github_user="unDocUMeantIt",
    example=c("This", "is", "a", "rather", "stupid", "demonstration"),
    overwrite=FALSE
  ){
    # just make sure we can write something to a proper place
    stopifnot(dir.exists(dir))

    # first generate the directory structure and additional files
    sylly_langpack_skeleton(
      lang=lang,
      lang_name=lang_name,
      desc=desc,
      alt_lang=alt_lang,
      author=author,
      email=email,
      year=year,
      date=date,
      version=version,
      github_user=github_user,
      example=example,
      dir=dir,
      overwrite=overwrite
    )
    
    # finally save the pattern object
    write_hyph_rda(
      hyph_pattern=hyph.pattern,
      dir=dir,
      lang=lang
    )
    if(!is.null(alt_hyph_pattern)){
      stopifnot(is.list(alt_hyph_pattern))
      stopifnot(all(sapply(alt_hyph_pattern, function(this_pat){inherits(this_pat, "kRp.hyph.pat")})))
      alt_hyph_pat_lang <- names(alt_hyph_pattern)
      stopifnot(isTRUE(length(alt_hyph_pat_lang) == length(alt_hyph_pattern)))
      sapply(seq_along(alt_hyph_pattern),
        function(this_pat_num){
          write_hyph_rda(
            hyph_pattern=alt_hyph_pattern[[this_pat_num]],
            dir=dir,
            lang=alt_hyph_pat_lang[[this_pat_num]],
            pkg_lang=lang
          )
        }
      )
    } else {}

    message("you can now roxygenize the package!")
    
    return(invisible(NULL))
  }
) ## end method sylly_langpack()
