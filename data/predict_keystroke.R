predict_keystroke <- function (sentence){
  # load our data
  words<- read.table("words_2a.txt")
  ngrams2<- read.table("ngrams2_2a.txt")
  ngrams3<- read.table("ngrams3_2a.txt")
  words<-words[order(words$testCounts, decreasing = T),]
  ngrams2<-ngrams2[order(ngrams2$ngrams2Counts, decreasing = T),]
  ngrams3<-ngrams3[order(ngrams3$ngrams3Counts, decreasing = T),]
  print ("Data loaded.")
  
  # predict words in sentence
  ### sentence <- gsub("([[:punct:]])", " \\1 ", sentence)
  ### TESTING! Eliminate punctuation for this one!
  sentence <- gsub("[[:punct:]]", "", sentence)
  # sentence <- toupper (sentence)
  sentence <- strsplit(sentence, " ")[[1]]
  hasWord <- F # at first, there's no digram because there's only one word.
  keySavings <- {}
  for (i in 1:(length(sentence)-1)){
    myNGram <- sentence [max(i-1,1):i]
    myWord <- sentence[i + 1]
    for (j in 1:nchar(myWord)){
      rankCounter <- 0
      if (hasWord){
        test<-as.character(ngrams3$ngrams3Unique[grep (sprintf("^%s %s %s",myNGram[1], myNGram[2],
                                                               substring(myWord, 1, j)),
                                                       ngrams2$ngrams2Unique)])
        myRank <- grep (sentence[(i-1):(i+1)], test)
        hasWord <- (length(myRank) > 0)
        rankCounter <- rankCounter + length(test)
      }
      if(!hasWord ){
        test<-as.character(ngrams2$ngrams2Unique[grep (sprintf("^%s %s",myNGram[1], 
                                                               substring(myWord, 1, j)),
                                                       ngrams2$ngrams2Unique)])
        myRank <- grep (sentence[i:(i+1)], test)
        hasWord <- (length(myRank) > 0)
        rankCounter <- rankCounter + length(test)
      }
      if(!hasWord){
        myRank <- grep (sprintf ("^%s", substring(myWord, 1, j)), words)
        hasWord <- (length(myRank) > 0)
        rankCounter <- rankCounter + length(words)
      }
      if (rankCounter <= 5) break
    }
    keySavings <- c(keySavings, 1 - j/nchar(myWord))
    print (c(i, 1 - j/nchar(myWord)))
    
  }
  return (keySavings)
}

### NEXT STEPS: ###
# 1a. Statistical evaluation. Should we be case-sensitive or not? Should we include punctuation or not?
#       ANS: Case-sensitive, do not use punctuation
# 1b. Once the evaluation is completed, make datasets for foreign languages. 
#       TODO
# 2. How rare are "rare words"? What is the cutoff?
#   Ex:
#     > length(wordCdf[wordCdf < 0.5])
#     [1] 47
#     > length(wordCdf[wordCdf < 0.75])
#     [1] 992
#   Perform these test throughout
#     ANS: 96 <0.5, 1471 <0.75
# 2a. Context selection. Do a second pass creating a rare-word db
#     TODO: Using >0.5 rare words, because >0.75 don't quite seem rare enough.
# 2b. Part of speech tagging for common words
#     IFFY.