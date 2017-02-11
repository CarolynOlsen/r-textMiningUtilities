#########
# PURPOSE:      Using MPQA Opinion Corpus, score sentiment, with basic handling 
#               for negations ("don't like" is different than "like").
#########

#read in Weibe et al (2005) subjectivity lexicon from CSV file
#English language
subj_lexicon_en <- read.csv("en_subjectivity_lexicon.csv")

#define overall polarity function

score.subj.polarity <- function(sentences, .progress='none') {
  
  #load libraries
  require(plyr)
  require(stringr)
  
  #define a list of negations
  negations <- c("not","isnt","dont","cant","doesnt","didnt","wont","havent")
  
  #score the sentences
  scores = laply(sentences, function(sentence) {
  
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
  
    #compare to words in subjectivity lexicon
    pos.avg.matches <- match(words
                         , subj_lexicon_en$writtenForm[subj_lexicon_en$polarity=="positive"&subj_lexicon_en$strength=="average"])
    pos.strong.matches <- match(words
                         , subj_lexicon_en$writtenForm[subj_lexicon_en$polarity=="positive"&subj_lexicon_en$strength=="strong"])
    neg.avg.matches <- match(words
                             , subj_lexicon_en$writtenForm[subj_lexicon_en$polarity=="negative"&subj_lexicon_en$strength=="average"])
    neg.strong.matches <- match(words
                                , subj_lexicon_en$writtenForm[subj_lexicon_en$polarity=="negative"&subj_lexicon_en$strength=="strong"])
    negation.matches <- match(words,negations)
    
    #match() returns the position of the matched term; we want TRUE/FALSE
    pos.avg.matches <- !is.na(pos.avg.matches)
    pos.strong.matches <- !is.na(pos.strong.matches)
    neg.avg.matches <- !is.na(neg.avg.matches)
    neg.strong.matches <- !is.na(neg.strong.matches)
    negation.matches <- !is.na(negation.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score <- (sum(pos.avg.matches) + 2*sum(pos.strong.matches) 
              - sum(neg.avg.matches) - 2*sum(neg.strong.matches) - sum(negation.matches))
    
    return(list(
      score=score
      ,positive.cnt=sum(pos.avg.matches) + 2*sum(pos.strong.matches)
      ,negative.cnt=sum(neg.avg.matches) + 2*sum(neg.strong.matches) 
      ,negation.cnt=sum(negation.matches)
      ,type=ifelse(score>0,"positive",ifelse(score<0,"negative","neutral"))
      ,mixed=ifelse((sum(pos.avg.matches) + 2*sum(pos.strong.matches) - sum(negation.matches))>0
                    && (sum(neg.avg.matches) + 2*sum(neg.strong.matches))>0,1,0)
    )
    )
  },.progress=.progress )
  
  scores.df = data.frame(score=scores$score
                         ,type=scores$type
                         ,positive.cnt=scores$positive.cnt
                         ,negative.cnt=scores$negative.cnt
                         ,negation.cnt=scores$negation.cnt
                         ,mixed=scores$mixed
                         ,text=sentences)
  return(scores.df)
  
}
