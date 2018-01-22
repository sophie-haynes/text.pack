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
        text = sapply(corpus, as.character)
    )
    colnames(text.table) = c("text")
    #Loop through each line
    for(i in 1:nrow(text.table)){
        tryCatch({
            #Tokenise line
            words <- as.vector(strsplit(as.character(
                text.table[i,])," ")[[1]])
            #Treetag each word
            tagged <- suppressWarnings(treetag(
                format="obj", words, 
                treetagger="manual", lang="en", 
                TT.options=list(
                    path="~/TreeTagger",
                    preset="en")
            ))
            table = taggedText(tagged)
            table$new = NA
            for(j in 1:nrow(table)){
                if(table[j,]$lemma == "<unknown>"){
                    table[j,]$new = table[j,]$token
                } else{
                    table[j,]$new = table[j,]$lemma
                }
            }
            #Make new string from cleaned text
            string <- paste(c(table$new),collapse=" ")
        }, 
        warning = function(w){
            print("Warning")
            print(i)
            print(paste("\nMY_WARNING:  ",w))
        }, 
        error = function(e){
            print("Oh no! I had an error!")
            print(i)
            print(paste(e))
            string = words
        },
        finally = {
            text.table$text[i] = string
        })
    }
    text.table
}