# Copyright 2010-2020 Meik Michalke <meik.michalke@hhu.de>
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

#' @importFrom stats setNames
#' @importFrom utils data file_test head modifyList setTxtProgressBar tail txtProgressBar

## function koRpus2sylly()
# transitional internal function to transform hyphen pattern data, that was
# saved using koRpus, into sylly data. the problem with the old files is that
# the S4 objects were saved with an attribute refering to the koRpus package,
# making it impossible to use sylly *without* also loading koRpus automatically
# - path: full path to an old *.rda file
koRpus2sylly <- function(path, replaceFile=TRUE){
  intEnv <- new.env()
  path <- normalizePath(path, mustWork=TRUE)
  load(path, envir=intEnv)
  hyphPatName <- ls(envir=intEnv)
  hyphPat <- intEnv[[hyphPatName]]
  currentPackage <- attr(class(hyphPat), "package")
  if(identical(currentPackage, "koRpus")){
    message(paste0("converting ", hyphPatName, " from koRpus to sylly"))
    attr(class(hyphPat), "package") <- "sylly"
    intEnv[[hyphPatName]] <- hyphPat
    if(isTRUE(replaceFile)){
      save(
        list=hyphPatName,
        envir=intEnv,
        file=path,
        compress="xz",
        compression_level=-9
      )
    }
  } else {
    message(paste0(hyphPatName, " is not a koRpus object (package: ", currentPackage, ")"))
  }
  return(invisible(NULL))
} ## end function koRpus2sylly()


## function explode.letters()
# all possible values of .word.:
# 6 x 1: . w o r d .
#        1 2 3 4 5 6
# 5 x 2: .w wo or rd d.
#        12 23 34 45 56
# 4 x 3: .wo wor ord rd.
#        123 234 345 456
# 3 x 4: .wor word ord.
#        1234 2345 3456
# 2 x 5: .word word.
#        12345 23456
# 1 x 6: .word.
#        123456
#
explode.letters <- function(max.word.length=get.sylly.env(hyph.max.token.length=TRUE)){
  result <- lapply(
    1:max.word.length,
    function(wl){
      lapply(
        1:wl,
        function(sl){
          lapply(
            1:(wl-sl+1),
            function(x){
              x:(x+sl-1)
            }
          )
        }
      )
    }
  )
  return(result)
} ## end function explode.letters()


## function explode.word()
# using the provided patterns, split an actual word into its subpatterns
# - min.pattern/max.pattern: set the minimum and maximum length of available
#   patterns, makes no sense to split further in the first place
explode.word <- function(word, min.pattern=2L, max.pattern=5L){
  word.length <- nchar(word)
  hyph.max.token.length <- get.sylly.env(hyph.max.token.length=TRUE)
  if(word.length > hyph.max.token.length){
    # update internal pattern cache to cope with longer words
    set.sylly.env(hyph.max.token.length=word.length + 5)
  } else {}
  if(word.length <= min.pattern){
    result <- data.frame(frag=word, on=1L, off=min(word.length, min.pattern))
  } else {
    all.patterns <- mget("all.patterns", envir=as.environment(.sylly.env), ifnotfound=list(NULL))[["all.patterns"]]
    result <- matrix(unlist(vapply(
      unlist(all.patterns[[word.length]][min.pattern:min(word.length, max.pattern)], recursive=FALSE),
      function(lttrs){
        return(
          # already include a dummy 'match' row
          c(
            substr(word, lttrs[1], max(lttrs)), # frag
            lttrs[1],                           # on
            max(lttrs),                         # off
            NA                                  # match
          )
        )
      },
      FUN.VALUE=c(frag="", on=0, off=0, match=NA),
      USE.NAMES=FALSE
    )), nrow=4L, dimnames=list(c("frag","on","off","match"),NULL))
  }
  return(result)
} ## function explode.word()


## function get.hyph.cache()
get.hyph.cache <- function(lang){
  # don't try anything while cache is locked
  locked <- mget("hyphenCacheLock", envir=as.environment(.sylly.env), ifnotfound=list(hyphenCacheLock=FALSE))[["hyphenCacheLock"]]
  while(isTRUE(locked)){
    Sys.sleep(0.5)
    locked <- mget("hyphenCacheLock", envir=as.environment(.sylly.env), ifnotfound=list(hyphenCacheLock=FALSE))[["hyphenCacheLock"]]
  }
  # simply get cache from current sylly environment
  # returns NULL if none exists
  if(isTRUE(lang == "all")){
    return(mget("hyphenCache", envir=as.environment(.sylly.env), ifnotfound=list(NULL))[["hyphenCache"]])
  } else {
    return(mget("hyphenCache", envir=as.environment(.sylly.env), ifnotfound=list(NULL))[["hyphenCache"]][[lang]])
  }
}
## end function get.hyph.cache()


## function check.hyph.cache()
# called by hyphen(), returns either the cached entry, or NULL
# - missing: if TRUE, all elements of token are looked up at once, and
#   a vector of all missing tokens is returned, or NULL
# - multiple: if TRUE, token must be a character vector, result is a data.frame
check.hyph.cache <- function(lang, token, cache=get.hyph.cache(lang=lang), missing=FALSE, multiple=FALSE){
  result <- NULL
  if(is.null(cache)){
    # no cache, no hit...
    if(isTRUE(missing)){
      result <- token
    } else {}
  } else {
    if(isTRUE(missing)){
      missing.tokens <- token[!token %in% names(cache)]
      if(length(missing.tokens) > 0){
        result <- missing.tokens
      } else {}
    } else if(isTRUE(multiple)){
      # update fields with data from cache if available
      result <- as.data.frame(t(
        vapply(
          token,
          function(tk){
            inCache <- cache[[tk]]
            if(is.null(inCache)){
              return(c(syll=1, word=tk))
            } else {
              return(c(syll=inCache[["syll"]], word=inCache[["word"]]))
            }
          },
          FUN.VALUE=c(syll=0, word=""),
          USE.NAMES=FALSE
        )
      ), stringsAsFactors=FALSE)
      colnames(result) <- c("syll", "word")
      result[["syll"]] <- as.numeric(result[["syll"]])
    } else {
      # check if this word was hyphenated before;
      # will be NULL if not found
      result <- cache[[token]]
    }
  }
  return(result)
} ## end function check.hyph.cache()


## function set.hyph.cache()
# writes (probably new) cache data back to the environment
# - append: a named list of new data (name is the token, value another named list
#           containing "syll" and "word", respectively)
set.hyph.cache <- function(lang, append=NULL, cache=get.hyph.cache(lang=lang)){
  # don't try anything while cache is locked
  locked <- mget("hyphenCacheLock", envir=as.environment(.sylly.env), ifnotfound=list(hyphenCacheLock=FALSE))[["hyphenCacheLock"]]
  while(isTRUE(locked)){
    Sys.sleep(0.1)
    locked <- mget("hyphenCacheLock", envir=as.environment(.sylly.env), ifnotfound=list(hyphenCacheLock=FALSE))[["hyphenCacheLock"]]
  }
  # now *we* lock the cache
  assign("hyphenCacheLock", list(hyphenCacheLock=TRUE), pos=as.environment(.sylly.env))
  all.kRp.env.hyph <- mget("hyphenCache", envir=as.environment(.sylly.env), ifnotfound=list(NULL))[["hyphenCache"]]
  if(is.null(all.kRp.env.hyph)){
    all.kRp.env.hyph <- new.env()
  } else {}
  # append result to cache
  if(!is.null(append)){
    # could be there is no cache yet
    if(is.null(cache)){
      cache <- new.env()
    } else {}
    # using arbitrary character stuff for names might fail
      try(
        # 'all.names=TRUE' is needed to not lose all tokens that begin with a dot!
        cache <- list2env(as.list(append, all.names=TRUE), envir=cache)
#         cache <- as.environment(modifyList(as.list(cache, all.names=TRUE), as.list(append, all.names=TRUE)))
      )
  } else {
    if(is.null(cache)){
      # hm, if both is null, don't do anything
      assign("hyphenCacheLock", list(hyphenCacheLock=FALSE), pos=as.environment(.sylly.env))
      return(invisible(NULL))
    } else {}
  }

  if(isTRUE(lang == "all")){
    all.kRp.env.hyph <- cache
  } else {
    all.kRp.env.hyph[[lang]] <- cache
  }
  assign("hyphenCache", all.kRp.env.hyph, envir=as.environment(.sylly.env))
  # unlock cache
  assign("hyphenCacheLock", list(hyphenCacheLock=FALSE), pos=as.environment(.sylly.env))
  return(invisible(NULL))
} ## end function set.hyph.cache()


## function read.hyph.cache.file()
# reads a dumped chace file, if "file" is not NULL and does exist
read.hyph.cache.file <- function(lang, file=get.sylly.env(hyph.cache.file=TRUE, errorIfUnset=FALSE), quiet=FALSE){
  if(is.null(file)){
    return(invisible(NULL))
  } else {}

  cache.file.path <- normalizePath(file, mustWork=FALSE)
  if(!file.exists(cache.file.path)){
    # if the file is not there yet, create one
    write.hyph.cache.file(quiet=quiet)
  } else {}
  # only reload the file if it changed or wasn't loaded at all yet
  cacheFileInfo.new <- file.mtime(cache.file.path)
  cacheFileInfo.old <- mget("hyphenCacheFile", envir=as.environment(.sylly.env), ifnotfound=list(NULL))[["hyphenCacheFile"]]
  if(identical(cacheFileInfo.new, cacheFileInfo.old[[lang]])){
    # file doesn't seem to have changed
    return(invisible(NULL))
  } else if(is.null(cacheFileInfo.old)){
    # this must be the first time we try to read the file
    cacheFileInfo.old <- list()
  } else {}
  # set sylly.hyph.cache to NULL to suppress R CMD check warning
  sylly.hyph.cache <- NULL
  load(cache.file.path)
  # data will be checked by set.hyph.cache(), so no need to worry here
  # but the loaded data must contain an environment named "sylly.hyph.cache"
  if(is.null(sylly.hyph.cache)){
    stop(simpleError("The cache file you provided does not contain sylly-ready hyphenation data!"))
  } else {
    # cache format changed with 0.10-2, make sure we're good
    if(is.data.frame(sylly.hyph.cache)){
      warning("Cache file format has changed, trying conversion. If you run into errors, delete your old cache files!", call.=FALSE)
      sylly.hyph.cache <- setNames(
        object=lapply(
          seq_along(sylly.hyph.cache[["token"]]),
          function(n){
            return(
              list(
                syll=as.numeric(sylly.hyph.cache[n,"syll"]),
                word=as.character(sylly.hyph.cache[n,"word"])
              )
            )
          }
        ),
        nm=sylly.hyph.cache[["token"]]
      )
    } else {}
  }
  # set new file data to prevent from reloading if unchanged
  cacheFileInfo.old[[lang]] <- cacheFileInfo.new
  assign("hyphenCacheFile", cacheFileInfo.old, envir=as.environment(.sylly.env))

  # write loaded data to environment
  set.hyph.cache(lang="all", append=sylly.hyph.cache, cache=get.hyph.cache(lang="all"))

  return(invisible(NULL))
} ## end function read.hyph.cache.file()


## function write.hyph.cache.file()
# dumps cache data into a file, if "file" is not NULL. if it doesn't exist, it will be created
write.hyph.cache.file <- function(file=get.sylly.env(hyph.cache.file=TRUE, errorIfUnset=FALSE), quiet=FALSE){
  if(is.null(file)){
    return(invisible(NULL))
  } else {}

  cache.file.path <- normalizePath(file, mustWork=FALSE)
  if(!file.exists(cache.file.path)){
    if(!isTRUE(quiet)){
      message(paste0("Cache file does not exist and will be created:\n  ", cache.file.path))
    } else {}
  } else {}

  sylly.hyph.cache <- get.hyph.cache(lang="all")
  # if there is no cache yet, make it an empty environment
  if(is.null(sylly.hyph.cache)){
    sylly.hyph.cache <- list()
  } else {}
  save(sylly.hyph.cache, file=cache.file.path)

  return(invisible(NULL))
} ## end function write.hyph.cache.file()


## function hyphen.word()
# this helper function is being called by kRp.hyphen.calc(), see below
hyphen.word <- function(
    word,
    hyph.pattern=NULL,
    min.length=4L,
    rm.hyph=TRUE,
    min.pattern=2L,
    max.pattern=5L,
    as.cache=FALSE
  ){
    # consider min length of word?
    if(nchar(word) < min.length){
      if(isTRUE(as.cache)){
        return(list(syll=1, word=word))
      } else {
        return(c(syll=1, word=word))
      }
    } else {}
    word.orig <- word
    ## remove hyphens in word
    if(isTRUE(rm.hyph)){
      word <- gsub("-", "", word)
    } else {}
    # non-letters like leading dots confuse the algorithm. we'll remove any non-alphabetic character
    word <- gsub("[^\\p{L}]+", "", word, perl=TRUE)
    # if this removed all of the token, guess we're finished
    if (identical(word, "")){
      if(isTRUE(as.cache)){
        return(list(syll=1, word=word.orig))
      } else {
        return(c(syll=1, word=word.orig))
      }
    } else {}
    ## convert to lowercase
    word <- tolower(word)
    ## transform "word" to ".word."
    word.dotted <- paste0(".", word, ".")
    word.length <- nchar(word.dotted)

    # create word fragments ".wo", ".wor"... "rd."
    matched.patterns <- explode.word(word.dotted, min.pattern=min.pattern, max.pattern=max.pattern)
    # find all matching patterns of the word fragments
    matched.patterns["match",] <- vapply(
      matched.patterns["frag",],
      function(f){
        in.patterns <- slot(hyph.pattern, "pattern")[[f]]
        return(ifelse(is.null(in.patterns), "", in.patterns[["nums"]]))
      },
      FUN.VALUE="",
      USE.NAMES=FALSE
    )
    # now let's add the found matches and find the maximum
    matched.pat.index <- !"" == matched.patterns["match",]
    if(sum(matched.pat.index) > 0){
      pattern.matrix <- vapply(
        which(matched.pat.index),
        function(got.match){
          word.on <- max(1, (as.numeric(matched.patterns["on",got.match]) - 1))
          word.off <- as.numeric(matched.patterns["off",got.match])
          match.num.code <- unlist(strsplit(matched.patterns["match",got.match], split=""))
          return(c(rep(0, word.on - 1), match.num.code, rep(0, word.length - word.off)))
        },
        FUN.VALUE=rep("", word.length),
        USE.NAMES=FALSE
      )
      # this is the vector with the max values for the dotted word
      pattern.max <- as.numeric(apply(pattern.matrix, 1, max))

      # we'll never hyphenate before a word...
      pattern.max <- pattern.max[-c(1, length(pattern.max))]
      # ... never after the first or before the last letter
      pattern.max[c(1,length(pattern.max)-1,length(pattern.max))] <- 0

      # filter odd positions (count syllables)
      possible.hyphs <- (pattern.max %% 2) != 0
      syllables <- sum(possible.hyphs) + 1
      # recreate word with hyphens
      add.hyphen <- which(possible.hyphs)
      if(isTRUE(rm.hyph)){
        hyph.word <- unlist(strsplit(gsub("-", "", word.orig), split=""))
      } else {
        hyph.word <- unlist(strsplit(word.orig, split=""))
      }
      hyph.word[add.hyphen] <- paste0(hyph.word[add.hyphen], "-")
      # in cases where previous hyphenations were already removed and here returned,
      # don't return double them up
      hyph.word <- gsub("-+", "-", paste(hyph.word, collapse=""))
      if(isTRUE(as.cache)){
        hyph.result <- list(syll=syllables, word=hyph.word)
      } else {
        hyph.result <- c(syll=syllables, word=hyph.word)
      }
    } else {
      ## no hyphenation
      if(isTRUE(as.cache)){
        hyph.result <- list(syll=1, word=word.orig)
      } else {
        hyph.result <- c(syll=1, word=word.orig)
      }
    }
    # this will return *three* elements if as.cache is TRUE
    return(hyph.result)
} ## end function hyphen.word()


# this internal function does the real hyphenations,
# so it's mostly called by hyphen()

########################################################
## if this signature changes, check hyphen() as well! ##
########################################################

# min.length is set to 4 because we'll never hyphenate after the first of before the last letter, so
# words with three letters or less cannot be hyphenated
kRp.hyphen.calc <- function(
  words,
  hyph.pattern=NULL,
  min.length=4L,
  rm.hyph=TRUE,
  quiet=FALSE,
  cache=TRUE,
  lang=NULL,
  as="kRp.hyphen" # can also be "data.frame" or "numeric" to return simpler objects.
                  # "numeric" only returns the numeric results for each word/token
){

  stopifnot(is.character(words))

  # to avoid needless NOTEs from R CMD check
  token <- NULL
  # set a variable to check if we changed the data at all, to later skip the writing back part if possible
  # this is only relevant if cache=TRUE, but needs to be present for a check later on
  writeBackCache <- new.env()
  assign("changed", FALSE, envir=writeBackCache)

  # check for hyphenation pattern.
  if(is.null(hyph.pattern)){
    if(!is.null(lang)){
      # this way the text object defines pattern language
      hyph.pattern <- load.hyph.pattern(lang)
    } else {
      stop(simpleError("No language definition available. Set \"hyph.pattern\"!"))
    }
  } else {
    if(!inherits(hyph.pattern, "kRp.hyph.pat")){
      # the internal function load.hyph.pattern() will return what we need
      hyph.pattern <- load.hyph.pattern(hyph.pattern)
    } else {
      # optimize the pattern object
      hyph.pattern <- optimize.hyph.pattern(hyph.pattern)
    }
    # the other way: take language from hyph.pattern
    # overwrites lang in tagged.text
    lang <- slot(hyph.pattern, "lang")
  }
  if(isTRUE(cache)){
    # check if cached hyphenation data has been set with set.sylly.env().
    # if so, the data will directly be coped to sylly's environment
    read.hyph.cache.file(lang=lang, file=get.sylly.env(hyph.cache.file=TRUE, errorIfUnset=FALSE), quiet=quiet)
  } else {}

  if(!isTRUE(quiet)){
    # feed back the hypenation we're using
    message(paste0("Hyphenation (language: ", lang, ")"))
  } else {}

  # min-lenth and max-length of patterns
  min.pat <- slot(hyph.pattern, "min.pat")
  max.pat <- slot(hyph.pattern, "max.pat")

  ## main loop
  # build a vector with all possible word fragments
  # check for matches of the fragment vector in the pattern db
  uniqueWords <- unique(words)

  if(!isTRUE(quiet)){
    # counter to get some feedback
    .iter.counter <- new.env()
    assign("counter", 1, envir=.iter.counter)
  } else {}

  if(isTRUE(cache)){
    # the fastest way to fill the cache is to first check what types are missing,
    # hyphenate them and append them to cache in *one* go and *then* fetch results
    # for the actual hyphenation all from cache; omit uncachable tokens in the first place
    typesMissingInCache  <- check.hyph.cache(lang=lang, token=uniqueWords[nchar(uniqueWords) >= min.length], missing=TRUE)
    if(length(typesMissingInCache) > 0){
      if(!isTRUE(quiet)){
        # give some feedback, so we know the machine didn't just freeze...
        prgBar <- txtProgressBar(min=0, max=length(typesMissingInCache), style=3)
      } else {}
      typesMissingHyphenated <- setNames(
        object=lapply(
          typesMissingInCache,
          function(nw){
            if(!isTRUE(quiet)){
              # update prograss bar
              iteration.counter <- get("counter", envir=.iter.counter)
              setTxtProgressBar(prgBar, iteration.counter)
              assign("counter", iteration.counter + 1, envir=.iter.counter)
            } else {}
            return(hyphen.word(
              nw,
              hyph.pattern=hyph.pattern,
              min.length=min.length,
              rm.hyph=rm.hyph,
              min.pattern=min.pat,
              max.pattern=max.pat,
              as.cache=TRUE
            ))
          }
        ),
        nm=typesMissingInCache
      )
      typesMissingHyphenated[["syll"]] <- as.numeric(typesMissingHyphenated[["syll"]])
      set.hyph.cache(lang=lang, append=typesMissingHyphenated)
      assign("changed", TRUE, envir=writeBackCache)
      if(!isTRUE(quiet)){
        # close prograss bar
        close(prgBar)
      } else {}
    } else {}
    # fetch results from cache in one go
    hyph.df <- check.hyph.cache(lang=lang, token=words, multiple=TRUE)
  } else {
    if(!isTRUE(quiet)){
      # give some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=length(uniqueWords), style=3)
    } else {}
    hyphenate.results <- t(vapply(
      uniqueWords,
      FUN=function(nw){
        if(!isTRUE(quiet)){
          # update prograss bar
          iteration.counter <- get("counter", envir=.iter.counter)
          setTxtProgressBar(prgBar, iteration.counter)
          assign("counter", iteration.counter + 1, envir=.iter.counter)
        } else {}
        return(
          hyphen.word(
            nw,
            hyph.pattern=hyph.pattern,
            min.length=min.length,
            rm.hyph=rm.hyph,
            min.pattern=min.pat,
            max.pattern=max.pat,
            as.cache=FALSE
          )
        )
      },
      FUN.VALUE=c(syll=0, word=""),
      USE.NAMES=TRUE
    ))
    hyph.df <- as.data.frame(hyphenate.results[as.character(words), , drop=FALSE], stringsAsFactors=FALSE)
    colnames(hyph.df) <- c("syll","word")
    rownames(hyph.df) <- NULL
    hyph.df[["syll"]] <- as.numeric(hyph.df[["syll"]])
    if(!isTRUE(quiet)){
      # close prograss bar
      close(prgBar)
    } else {}
  }

  ## compute descriptive statistics
  num.syll <- sum(hyph.df$syll, na.rm=TRUE)
  syll.distrib <- value.distribs(hyph.df$syll)
  syll.uniq.distrib <- value.distribs(unique(hyph.df)$syll)
  avg.syll.word <- mean(hyph.df$syll, na.rm=TRUE)
  syll.per100 <- avg.syll.word * 100

  desc.stat.res <- list(
    num.syll=num.syll,
    syll.distrib=syll.distrib,
    syll.uniq.distrib=syll.uniq.distrib,
    avg.syll.word=avg.syll.word,
    syll.per100=syll.per100
  )

  if(all(isTRUE(cache), isTRUE(get("changed", envir=writeBackCache)))){
    # check if cached hyphenation data has been set with set.sylly.env().
    # if so and if we added to it here, the current data will be written back to that file
    write.hyph.cache.file(file=get.sylly.env(hyph.cache.file=TRUE, errorIfUnset=FALSE), quiet=quiet)
  } else {}

  if(identical(as, "kRp.hyphen")){
    results <- kRp_hyphen(lang=lang, desc=desc.stat.res, hyphen=hyph.df)
  } else if(identical(as, "data.frame")){
    results <- hyph.df
  } else if(identical(as, "numeric")){
    results <- hyph.df[["syll"]]
  } else {
    stop(simpleError("'as' must be one of \"kRp.hyphen\", \"data.frame\", or \"numeric\"!"))
  }

  return(results)
} ## end function kRp.hyphen.calc()


## function optimize.hyph.pattern()
# replaces the pattern matrix with a hashed environment
optimize.hyph.pattern <- function(hyph.pat){
  pattern.matrix <- slot(hyph.pat, "pattern")
  pattern.list <- setNames(
    object=lapply(
      seq_along(pattern.matrix[,"char"]),
      function(n){
        return(
          list(
            # nums must remain character to keep leading zeroes!
            nums=as.character(pattern.matrix[n,"nums"]),
            orig=as.character(pattern.matrix[n,"orig"])
          )
        )
      }
    ),
    nm=pattern.matrix[,"char"]
  )
  # "kRp.hyph.pat.env" is an internal class, defined in 01_class_01_kRp.hyph.pat.R
  new.hyph.pat <- kRp_hyph_pat_env(
    lang=slot(hyph.pat, "lang"),
    min.pat=min(nchar(pattern.matrix[,"char"])),
    max.pat=max(nchar(pattern.matrix[,"char"])),
    pattern=as.environment(pattern.list)
  )
  return(new.hyph.pat)
} ## end function optimize.hyph.pattern()


## function load.hyph.pattern()
load.hyph.pattern <- function(lang){
  # to avoid needless NOTEs from R CMD check
  hyph.pat <- NULL

  lang <- is.supported.lang(lang, support="hyphen")
  # check for additional package information, in case we're
  # importing hyphen patterns from a third party package
  lang.names <- names(lang) %in% "package"
  if(length(lang) > 1 & any(lang.names)){
    hyph.package <- lang[["package"]]
    lang <- lang[!lang.names][[1]]
  } else {
    hyph.package <- "sylly"
  }
  # we'll populate the internal environment with optimized patterns
  if(!exists(paste0("hyph.", lang), envir=as.environment(.sylly.env), inherits=FALSE)){
    # we'll load the hyphen pattern, get it here and check its format
    # this way packages can carry both old and new pattern formats
    data(list=paste0("hyph.", lang), package=hyph.package, envir=as.environment(.sylly.env))
    hyph.pat.optim <- get(paste0("hyph.", lang), envir=as.environment(.sylly.env))
    if(!inherits(hyph.pat.optim, "kRp.hyph.pat.env")){
      # optimization is only needed for packages with old pattern objects
      # this should not be an issue, as it's quite fast and happens only once a pattern set is loaded
      hyph.pat.optim <- optimize.hyph.pattern(hyph.pat.optim)
      # replace hyph.XX with optimized object
      assign(paste0("hyph.", lang), hyph.pat.optim, envir=as.environment(.sylly.env))
    } else {}
  } else {
    hyph.pat.optim <- get(paste0("hyph.", lang), envir=as.environment(.sylly.env))
  }
  # return optimized patterns
  return(hyph.pat.optim)
} ## end function load.hyph.pattern()


## function is.supported.lang()
# determins if a language is supported, and returns the correct identifier
is.supported.lang <- function(lang.ident, support="hyphen"){
  if(identical(support, "hyphen")){
    hyphen.supported <- as.list(as.environment(.sylly.env))[["langSup"]][["hyphen"]][["supported"]]
    if(any(names(hyphen.supported) == lang.ident)){
      res.ident <- hyphen.supported[[lang.ident]]
    } else {
      stop(simpleError(paste0("Unknown language: \"", lang.ident, "\".\nPlease provide a valid hyphenation pattern!")))
    }
  } else {}

  return(res.ident)
} ## end function is.supported.lang()


## function value.distribs()
value.distribs <- function(value.vect, omit.missings=TRUE){
  vector.length <- length(unlist(value.vect))
  vector.summary <- summary(as.factor(value.vect))

  # try to fill up missing values with 0, e.g., found no words with 5 characters
  if(!isTRUE(omit.missings)){
    if(!is.numeric(value.vect)){
      # makes only sense for numeric values
      stop(simpleError("value.distribs(): Impute missings is only valid for numeric vectors!"))
    } else {}
    present.values <- as.numeric(names(vector.summary))
    max.value <- max(present.values)
    missing.values <- c(1:max.value)[!c(1:max.value) %in% present.values]
    append.to.summary <- rep(0, length(missing.values))
    names(append.to.summary) <- as.character(missing.values)
    vector.summary <- c(vector.summary, append.to.summary)
    # finally sort the outcome
    vector.summary <- vector.summary[order(as.numeric(names(vector.summary)))]
  } else {}

  # add cumulative values
  vect.summ.cum <- cumsum(vector.summary)
  # inverse 
  vect.summ.inv <- rbind(vector.summary, vect.summ.cum, vector.length - vect.summ.cum)
  # add percentages
  vect.summ.cum.pct <- rbind(vect.summ.inv, (vect.summ.inv * 100 / vector.length))
  dimnames(vect.summ.cum.pct)[[1]] <- c("num", "cum.sum", "cum.inv", "pct", "cum.pct", "pct.inv")
  return(vect.summ.cum.pct)
} ## end function value.distribs()


## function check.file()
# helper function for file checks
check.file <- function(filename, mode="exist", stopOnFail=TRUE){

  ret.value <- FALSE

  if(identical(mode, "exist") | identical(mode, "exec")){
    if(as.logical(file_test("-f", filename))){
      ret.value <- TRUE
    } else {
      if(isTRUE(stopOnFail)){
        stop(simpleError(paste("Specified file cannot be found:\n", filename)))
      } else {}
      ret.value <- FALSE
    }
  } else {}

  if(identical(mode, "exec")){
    if(as.logical(file_test("-x", filename))){
      ret.value <- TRUE
    } else {
      if(isTRUE(stopOnFail)){
        stop(simpleError(paste("Specified file cannot be executed:\n", filename)))
      } else {}
      ret.value <- FALSE
    }
  } else {}

  if(identical(mode, "dir")){
    if(as.logical(file_test("-d", filename))){
      ret.value <- TRUE
    } else {
      if(isTRUE(stopOnFail)){
        stop(simpleError(paste("Specified directory cannot be found:\n", filename)))
      } else {}
      ret.value <- FALSE
    }
  } else {}

  return(ret.value)
} ## end function check.file()


## function check_lang_packages()
# checks what sylly.** packages are currently installed or loaded
# returns a named list with a list for each installed package, providing
# entries named "available", "installed", "loaded", and "title"
# availabe: also check for all available packages in 'repos'
# available.only: omit all installed packages which cannot be found in 'repos'
#' @importFrom utils available.packages packageDescription
check_lang_packages <- function(available=FALSE, repos="https://undocumeantit.github.io/repos/l10n/", available.only=FALSE, pattern="^sylly\\.[[:alpha:]]+$"){
  ### this function should be kept close to identical to the respective function
  ### in the 'sylly' package, except for the pattern
  result <- list()
  if(isTRUE(available)){
    available_packages <- utils::available.packages(repos=repos)
    available_koRpus_lang <- grepl(pattern, available_packages[,"Package"])
    supported_lang <- unique(available_packages[available_koRpus_lang,"Package"])
  } else {
    available_koRpus_lang <- FALSE
    supported_lang <- NULL
  }

  loaded_packages <- loadedNamespaces()
  loaded_koRpus_lang <- grepl(pattern, loaded_packages)
  installed_packages <- unique(dir(.libPaths()))
  installed_koRpus_lang <- grepl(pattern, installed_packages)

  have_koRpus_lang <- any(installed_koRpus_lang, available_koRpus_lang)

  if(isTRUE(have_koRpus_lang)){
    if(isTRUE(available.only)){
      all_packages <- supported_lang
    } else {
      all_packages <- unique(c(installed_packages[installed_koRpus_lang], supported_lang))
    }
    for (this_package in all_packages){
      result[[this_package]] <- list(available=NA, installed=FALSE, loaded=FALSE, title="(unknown)")
      if(all(isTRUE(available), any(supported_lang == this_package))){
        result[[this_package]][["available"]] <- TRUE
      } else {}
      if(any(installed_packages[installed_koRpus_lang] == this_package)){
        result[[this_package]][["installed"]] <- TRUE
        this_package_index <- which.min(!installed_packages %in% this_package)
        result[[this_package]][["title"]] <- utils::packageDescription(installed_packages[this_package_index])[["Title"]]
      } else {}
      if(any(loaded_packages[loaded_koRpus_lang] == this_package)){
        result[[this_package]][["loaded"]] <- TRUE
      } else {}
    }
  } else {}
  
  return(result)
} ## end function check_lang_packages()
