#########
# PURPOSE:      Score polarity using qdap dictionaries. 
#########


score_polarity <- function(id_vector
                           , text_vector
                           , polarity_dict
                           , grouping_var=NULL
                           , constrain=FALSE
                           , amplifierweight=0.8
                           , questionweight=0
                           , nbefore=4
                           , nafter=2
                           , digits=3) {
  library(qdap)
  library(data.table)
  library(hash)
  
  #if a word or phrase found in the text vector matches a word or phrase in the polarity dictionary,
  #then replace spaces with "xxx" so that the words are kept together for scoring purposes
  
  #input text cleaning
  v <- tolower(as.character(gsub("[][!#$%()*,.:;<=>@^_`|~.{}]", "",text_vector)))
  v <- gsub("[-]"," ",v)
  v <- replace_symbol(v)
  v <- clean(v)
  v <- replace_ordinal(v)
  v <- replace_number(v,remove=TRUE)
  print("Input text cleaned.")
  
  #create sentiment frame with ngrams
  key <- sentiment_frame(as.character(polarity_dict$x), "", polarity_dict$y)
  
  #run polarity function
  p<- polarity(v, grouping.var = NULL,
               polarity.frame = key, constrain = FALSE,
               negators = qdapDictionaries::negation.words,
               amplifiers = qdapDictionaries::amplification.words,
               deamplifiers = qdapDictionaries::deamplification.words,
               question.weight = questionweight, amplifier.weight = amplifierweight, n.before = nbefore,
               n.after = nafter, rm.incomplete = FALSE, digits = digits)
  print("Polarity algorithm run completed.")
  
  dfqq <- data.frame(cbind("ID"=id_vector,p$all))
  
  #create text sentiment column: neutral, positive, or negative
  dfqq$polarity[is.na(dfqq$polarity)] <- 0
  dfqq$sentiment <- ifelse(dfqq$polarity==0,"neutral",ifelse(dfqq$polarity<0,"negative","positive"))
  
  #flag lowest 2%
  dfqq$verynegative <- ifelse(dfqq$polarity<=quantile(dfqq$polarity,0.02),1,0)
  
  dfqq$text.var <- as.character(gsub("xxx"," ",dfqq$text.var))
  
  print("Polarity scores data frame created.")
  
  print(paste("Average polarity score across all responses: ",round(p$group$ave.polarity,2)))
  
  return(dfqq)
}