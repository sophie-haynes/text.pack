#' Corpus Punctuation Stripping Function
#' 
#' This function strips punctuation from a corpus object. It will also
#' remove all numerics and make the corpus lowercase.
#' @param corpus  Corpus object.
#' @author Sophie C Haynes, \email{thesophienator96@@gmail.com}
#' @keywords tm textmine punctuation strip nlp
#' tm::tm_map()
#' @export
#' @examples
#' punctStrip(text.corpus)

punctStrip <- function(corpus){
	# base function to strip numbers
	corpus <- tm_map(corpus, removeNumbers)
	# base function to remove basic punctuation
	corpus <- tm_map(corpus, removePunctuation)
	# base function to make text lower case
	corpus <- tm_map(corpus, content_transformer(tolower))
    # base funtion to replace identified punctuation with space
    toSpace <- content_transformer(function (x , pattern ) {
     gsub(pattern, " ", x)
     })
    punctList <- c("\\/","\\,","\\.",'\\"',"\\-","\\:","\\;","\\(","\\)","\\#",
              "\\%","\\°","\\ x","\\{","\\}","\\!","\\^","\\&","\\*","\\_",
              "\\•","\\º","\\–","\\`","\\]","\\[","\\?","\\@",">","<","\\~",
               "\\\n","\\ x ","\\?","\\'","\\“","\\”","\\‘","\\’","\\¼",
               "\\½","\\¾","\\³","\\²","\\\\","\\+","\\=")    
    for(x in 1:length(punctList)){
    	tm_map(corpus,toSpace,punctList[x])
    }
    corpus <- tm_map(corpus, stripWhitespace)
}