# Copyright 2010-2018 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package koRpus.lang.fr.
#
# koRpus.lang.fr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# koRpus.lang.fr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with koRpus.lang.fr.  If not, see <http://www.gnu.org/licenses/>.


# this is an internal file providing language support.
# please refer to inst/README.languages for details

#' Language support for French
#' 
#' This function adds support for French to the koRpus package. You should not
#' need to call it manually, as that is done automatically when this package is
#' being loaded.
#' 
#' In particular, this function adds the following:
#' \itemize{
#'  \item \code{lang}: The additional language "fr" to be used with koRpus
#'  \item \code{treetag}: The additional preset "fr", implemented according to the respective TreeTagger[1] script
#'  \item \code{POS tags}: An additional set of tags, implemented using the documentation for the corresponding
#'    TreeTagger parameter set[2]
#' }
#' Hyphenation patterns are provided by means of the \code{\link[sylly.fr:hyph.support.fr]{sylly.fr}} package.
#'
#' @param ... Optional arguments for \code{\link[koRpus:set.lang.support]{set.lang.support}}.
#' @references
#' [1] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}
#'
#' [2] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/french-tagset.html}
#' @export
#' @importFrom koRpus set.lang.support
#' @examples
#' \dontrun{
#' lang.support.fr()
#' }

lang.support.fr <- function(...) {
  koRpus::set.lang.support("treetag",
    list("fr"=list(
      ## preset: "fr"
      # tags utf-8 encoded text files
      # Alexandre Brulet added this French section
      lang="fr",
      encoding="UTF-8",
      preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
        TT.abbrev      <- file.path(TT.lib, "french-abbreviations-utf8")
        if(isTRUE(unix.OS)){
          # preset for unix systems
          return(
            list(
              TT.tokenizer      = file.path(TT.cmd, "utf8-tokenize.perl"),
              TT.tagger         = file.path(TT.bin, "tree-tagger"),
              TT.abbrev         = TT.abbrev,
              TT.params         = file.path(TT.lib, "french-utf8.par"),
              TT.lexicon        = c(),
              TT.lookup         = c(),
              TT.filter         = c(),

              TT.tknz.opts      = paste("-f -a", TT.abbrev),
              TT.lookup.command = c(),
              TT.filter.command = c()
            )
          )
        } else {
          # preset for windows systems
          return(
            list(
              TT.tokenizer      = file.path(TT.cmd, "utf8-tokenize.perl"),
              TT.tagger         = file.path(TT.bin, "tree-tagger.exe"),
              TT.abbrev         = TT.abbrev,
              TT.params         = file.path(TT.lib, "french-utf8.par"),
              TT.lexicon        = c(),
              TT.lookup         = c(),
              TT.filter         = c(),

              TT.tknz.opts      = paste("-f -a", TT.abbrev),
              TT.lookup.command = c(),
              TT.filter.command = c()
            )
          )
        }
      })
    ),
    ...
  )

  koRpus::set.lang.support("kRp.POS.tags",
    ## tag and class definitions
    # fr -- french
    list("fr"=list(
      tag.class.def.words=matrix(c(
        "ABR","abbreviation","abreviation",
        "ADJ","adjective","adjective",
        "ADV","adverb","adverb",
        "DET:ART","article","article",
        "DET:POS","pronoun","possessive pronoun (ma, ta, ...)",
        "INT","interjection","interjection",
        "KON","conjunction","conjunction",
        "NAM","name","proper name",
        "NOM","noun","noun",
        "NUM","numeral","numeral",
        "PRO","pronoun","pronoun",
        "PRO:DEM","pronoun","demonstrative pronoun",
        "PRO:IND","pronoun","indefinite pronoun",
        "PRO:PER","pronoun","personal pronoun",
        "PRO:POS","pronoun","possessive pronoun (mien, tien, ...)",
        "PRO:REL","pronoun","relative pronoun",
        "PRP","preposition","preposition",
        "PRP:det","preposition","preposition plus article (au,du,aux,des)",
        "SYM","symbol","symbol",
        "VER:cond","verb","verb conditional",
        "VER:futu","verb","verb futur",
        "VER:impe","verb","verb imperative",
        "VER:impf","verb","verb imperfect",
        "VER:infi","verb","verb infinitive",
        "VER:pper","verb","verb past participle",
        "VER:ppre","verb","verb present participle",
        "VER:pres","verb","verb present",
        "VER:simp","verb","verb simple past",
        "VER:subi","verb","verb subjunctive imperfect",
        "VER:subp","verb","verb subjunctive present"
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
      tag.class.def.punct=matrix(c(
        ",", "comma", "Comma", # not in guidelines
        "(", "punctuation", "Opening bracket", # not in guidelines
        ")", "punctuation", "Closing bracket", # not in guidelines
        ":", "punctuation", "Punctuation", # not in guidelines
        "``", "punctuation", "Quote", # not in guidelines
        "''", "punctuation", "End quote", # not in guidelines
        "#", "punctuation", "Punctuation", # not in guidelines
        "$", "punctuation", "Punctuation", # not in guidelines
        "PUN","punctuation","punctuation",
        "PUN:cit","punctuation","punctuation citation"
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
      tag.class.def.sentc=matrix(c(
        "SENT", "fullstop", "Sentence ending punctuation" # not in guidelines
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
      )
    ),
    ...
  )
}

# this internal, non-exported function causes the language support to be
# properly added when the package gets loaded
#' @importFrom sylly.fr hyph.support.fr
.onAttach <- function(...) {
  lang.support.fr()
  sylly.fr::hyph.support.fr()
}
