# read data
if (1==1){ ### debug
print ("loading data")
words <- read.table("data/en_US_words.txt")
ngrams2 <- read.table("data/en_US_ngrams2.txt")
ngrams3 <- read.table("data/en_US_ngrams3.txt")
context <- readLines("data/en_US_context.txt")

words <- words[order(words$testCounts, decreasing = T),]
ngrams2 <- ngrams2[order(ngrams2$ngrams2Counts, decreasing = T),]
ngrams3 <- ngrams3[order(ngrams3$ngrams3Counts, decreasing = T),]
print("data loaded.")
}

caption <<- ""
rareSentence <<- {}
rareNumbers <<- {}
updateCounter<<-0

makeCaption <- function(input){
  return (input$caption)
}

makeTable <- function(input){
  partialWordFlag <- T
  punctuationFlag <- F
  hasNextFlag <- F
  
  nextNextWord <- " "
  sentence <- ""
  if (length(input$caption) > 0) sentence <- input$caption
  #print (sentence)
  if (nchar(sentence)==0){ # skip ngrams if nothing entered
    myNGram <- ""
    nextWords <- as.character(words$testUnique)
  }else{
    # handle punctuation and spaces
    sentence <- gsub("([[:punct:]])", " ", sentence)
    if (substring(sentence,nchar(sentence)-1,nchar(sentence)) == "  ") 
      punctuationFlag <- T
    if (substring(sentence, nchar(sentence))==" ") partialWordFlag <- F
    #print(partialWordFlag)
    sentence <- strsplit(sentence, " ")[[1]]
    
    # make ngrams 
    myNGram <- sentence[max((length(sentence)-2),1):length(sentence)]
    if (partialWordFlag){ # special handling for rare prediction
      blah <- 2
    }
    else{
      caption <<- input$caption
      blah <- 3
    }
    if (punctuationFlag) myNGram <- c(myNGram, " ")
    
    # rare prediction
    nextWords <- {}
    if (length(myNGram) >= 3){
      # intersection method
      a <- grep(myNGram[1], context)
      b <- grep(myNGram[2], context)
      c <- grep(myNGram[3], context)
      nextNums <- intersect(intersect(a, b),c)
      if (length(nextNums)==0)
        nextNums <- union(union (intersect (a, b), intersect (b, c)), 
                          intersect(a,c))
      if (length (nextNums)==0)
        nextNums <- union(union(a, b), c)
      
      nextWords <- unlist(strsplit(as.character(context[nextNums]), " "))
      if ((length(nextWords) > 0)){
        if ((length(rareNumbers)!=1) | length(intersect(nextWords, rareSentence) > 0)){
          rareSentence <<- nextWords
          rareNumbers <<- nextNums
        }
      }
                           
      # rare ngram method
      nGramString <- sprintf("^%s %s %s",myNGram[1], myNGram[2], myNGram[3])
      nextWords <- grep (nGramString, 
                         ngrams3$ngrams3Unique)
      if (length(nextWords) == 1){
        rareNumbers <<- nextWords[1]
        nextWords <- context[grep(nGramString, context)]
        if (length(nextWords) > 0) rareSentence <<- strsplit(nextWords," ")[[1]]
        rareNGrams <- paste(rareSentence[1:(length(rareSentence)-2)], 
                            rareSentence[2:(length(rareSentence)-1)])
        rareNGrams <- paste(rareNGrams, rareSentence[3:length(rareSentence)])
        nextNextWord <- rareSentence[grep(nGramString, rareNGrams) + blah]
      }else if (length(rareNumbers) != 1){
        
      }
      if (length(nextNextWord)==0) nextNextWord <- " "
      if (is.na(nextNextWord)) nextNextWord <- " "
      if (!partialWordFlag) myNGram <- myNGram[2:length(myNGram)]
    }
    nextWords <- {}
    if (partialWordFlag){ # partial word at the end
      
      if (length(myNGram) > 2){
        nextWords <- as.character(ngrams3$ngrams3Unique[
          grep (sprintf("^%s %s %s",myNGram[1], myNGram[2], myNGram[3]), 
                ngrams3$ngrams3Unique)])
        if (length(nextWords) >= input$obs) hasNextFlag <- T
        myNGram <- myNGram [(length(myNGram) - 1):length(myNGram)]
      }
      if ((length(myNGram) > 1) & hasNextFlag == F){
        nextWords <- c(nextWords, as.character(ngrams2$ngrams2Unique[
          grep (sprintf("^%s %s",myNGram[1], myNGram[2]), ngrams2$ngrams2Unique)]))
        myNGram <- myNGram [length(myNGram)]
      }
      if (length(nextWords) > 0){
        nextWords <- strsplit(nextWords, " ")
        nextWords <- unique(unlist(lapply(nextWords,function (x) x[length(x)])))
      }
      # (nextWords)
      if (length(nextWords) >= input$obs) hasNextFlag <- T
      if (hasNextFlag == F){
        nextWords <- unique(c(nextWords,as.character(words$testUnique[
          grep (sprintf("^%s",myNGram[1]), words$testUnique)])))
      }
    }else{ # whole word at the end
      if (length(myNGram) > 1){
        nextWords <- as.character(ngrams3$ngrams3Unique[
          grep (sprintf("^%s %s ",myNGram[1], myNGram[2]), ngrams3$ngrams3Unique)])
        if (length(nextWords) >= input$obs) hasNextFlag <- T
        myNGram <- myNGram [length(myNGram)]
      }
      if (hasNextFlag == F){
        nextWords <- c(nextWords, as.character(ngrams2$ngrams2Unique[
          grep (sprintf("^%s ",myNGram[1]), ngrams2$ngrams2Unique)]))
      }
      if (length(nextWords) > 0){
        nextWords <- strsplit(nextWords, " ")
        nextWords <- unique(unlist(lapply(nextWords,function (x) x[length(x)])))
      }
      if (length(nextWords) >= input$obs) hasNextFlag <- T
      if (hasNextFlag == F){
        nextWords <- unique(c(nextWords,as.character(words$testUnique)))
        #print (nextWords)
      }
    }
  }
  nextWords <- c(intersect(nextWords, rareSentence), 
                 setdiff(nextWords, rareSentence))
  # (nextNextWord)
  if (nextNextWord != " ")
    nextWords <- c(nextNextWord, setdiff (nextWords, nextNextWord))
  prediction <- nextWords[1:max(1,min(input$obs,length(nextWords)))]
  # THIS SHOULDN'T BE WRONG, BUT IT IS. IT ACTS LIKE A GREP, BUT I'M NOT SURE WHY
#  if(is.element(myNGram[1],prediction) & myNGram[1]!="") 
#    prediction <- gsub(myNGram[1],".",prediction)
  return(prediction)
}