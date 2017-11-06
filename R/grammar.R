#' Spell Correcting Function
#' 
#' This function will stem and correct all spelling in the corpus using 
#' the TreeTag tool, the koRpus package and the stringr package. The 
#' function will return a data.table.
#' @param corpus  Corpus object.
#' @return data.table  Data table containing the original and grammar 
#' cleaned corpus text
#' @author Sophie C Haynes, \email{thesophienator96@@gmail.com}
#' @keywords treetag nlp spelling stemming
#' korPus::treetag()
#' korRus::taggedText()
#' stringr::str_replace_all()
#' data.table::data.table()
#' @export
#' @examples
#' grammar(text.corpus)

grammar <- function(corpus){
  # convert corpus to data table object
  text.table <- data.table(
    text = sapply(corpus, paste, collapse = " "),
    lemma = NA
    )

	#Loop through each line
  for(i in 1:nrow(text.table)){
    #Tokenise line
    words <- as.vector(strsplit(as.character(text.table[i,])," ")[[1]])
    #Treetag each word
    tagged <- suppressWarnings(treetag(
        format="obj",
        words, 
        treetagger="manual",
        lang="en", 
        TT.options=list(
            path="~/TreeTagger",
            preset="en")                    
    ))
    #Make new string from lemma
    string <- paste(c(taggedText(tagged)$lemma),collapse=" ")
    #clean unknowns from string
    string <- str_replace_all(string,"<unknown>","")
    #Replace with tagged
    text.table$lemma[i] = string
  }
  text.table
  #ßreturn(text.table)
}