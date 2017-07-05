#' Corpus Punctuation Stripping Function
#' 
#' This function strips punctuation from a corpus object.
#' @param corpus Corpus object.
#' @author Sophie C Haynes, \email{thesophienator96@@gmail.com}
#' @keywords tm textmine punctuation
#' tm::tm_map()
#' @export
#' @examples
#' punctStrip(text.corpus)

punctStrip <- function(corpus){
    # base funtion to replace identified punctuation with space
    toSpace <- content_transformer(function (x , pattern ) {
     gsub(pattern, " ", x)
     })    
    corpus <- tm_map(corpus, toSpace, "\\/")
    corpus <- tm_map(corpus, toSpace, "\\,")
    corpus <- tm_map(corpus, toSpace, "\\.")
    corpus <- tm_map(corpus, toSpace, '\\"')
    corpus <- tm_map(corpus, toSpace, "\\-")
    corpus <- tm_map(corpus, toSpace, "\\:") 
    corpus <- tm_map(corpus, toSpace, "\\;") 
    corpus <- tm_map(corpus, toSpace, "\\(")
    corpus <- tm_map(corpus, toSpace, "\\)")
    corpus <- tm_map(corpus, toSpace, "\\#")
    corpus <- tm_map(corpus, toSpace, "\\%")
    corpus <- tm_map(corpus, toSpace, "\\°")
    corpus <- tm_map(corpus, toSpace, "\\ x")
    corpus <- tm_map(corpus, toSpace, "\\{")
    corpus <- tm_map(corpus, toSpace, "\\}")
    corpus <- tm_map(corpus, toSpace, "\\!")
    corpus <- tm_map(corpus, toSpace, "\\^")
    corpus <- tm_map(corpus, toSpace, "\\&")
    corpus <- tm_map(corpus, toSpace, "\\*")
    corpus <- tm_map(corpus, toSpace, "\\_")
    corpus <- tm_map(corpus, toSpace, "\\•")
    corpus <- tm_map(corpus, toSpace, "\\º")
    corpus <- tm_map(corpus, toSpace, "\\–")
    corpus <- tm_map(corpus, toSpace, "\\`")
    corpus <- tm_map(corpus, toSpace, "\\]")
    corpus <- tm_map(corpus, toSpace, "\\[")
    corpus <- tm_map(corpus, toSpace, "\\?")
    corpus <- tm_map(corpus, toSpace, "\\@")
    corpus <- tm_map(corpus, toSpace, ">")
    corpus <- tm_map(corpus, toSpace, "<")
    corpus <- tm_map(corpus, toSpace, "\\~")
    corpus <- tm_map(corpus, toSpace, "\\\n")
    corpus <- tm_map(corpus, toSpace, "\\ x ")
    corpus <- tm_map(corpus, toSpace, "\\?")
    corpus <- tm_map(corpus, toSpace, "\\'")
    corpus <- tm_map(corpus, toSpace, "\\“")
    corpus <- tm_map(corpus, toSpace, "\\”")
    corpus <- tm_map(corpus, toSpace, "\\‘")
    corpus <- tm_map(corpus, toSpace, "\\’")
    corpus <- tm_map(corpus, toSpace, "\\¼")
    corpus <- tm_map(corpus, toSpace, "\\½")
    corpus <- tm_map(corpus, toSpace, "\\¾")
    corpus <- tm_map(corpus, toSpace, "\\³")
    corpus <- tm_map(corpus, toSpace, "\\²")
    corpus <- tm_map(corpus, toSpace, "\\\\")
    corpus <- tm_map(corpus, toSpace, "\\+")
    corpus <- tm_map(corpus, toSpace, "\\=")
    corpus <- tm_map(corpus, stripWhitespace)
}