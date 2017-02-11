##########
# PURPOSE:      Using the Apache OpenNLP API (https://opennlp.apache.org/), 
#               annotate a body of text.
# DESCRIPTION:  There are two functions created by this script. 
#               entities() extracts entities from an AnnotatedPlainTextDocument.
#               annotate_record() extracts entities from a single string.
# INPUTS:       in the entities function, values for kind are: "person", "location", "organization"
##########

library(rJava)
library(NLP)
library(openNLP)
library(magrittr)
library(RWeka)
library(gtools)

# define function to extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  #define annotation functions pipeline
  word_ann <- Maxent_Word_Token_Annotator()
  sent_ann <- Maxent_Sent_Token_Annotator()
  person_ann <- Maxent_Entity_Annotator(kind = "person")
  location_ann <- Maxent_Entity_Annotator(kind = "location")
  organization_ann <- Maxent_Entity_Annotator(kind = "organization")
  pipeline <- list(sent_ann
                   , word_ann
                   , person_ann
                   , location_ann
                   , organization_ann
                   )
  
  #entities function
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    tryCatch(s[a[k == kind]]
             ,error = function(e) {})
  } else {
    s[a[a$type == "entity"]]
  }
}

#define function to extract entities from a single string
annotate_record <- function(doc) {
  #define annotation functions pipeline
  word_ann <- Maxent_Word_Token_Annotator()
  sent_ann <- Maxent_Sent_Token_Annotator()
  person_ann <- Maxent_Entity_Annotator(kind = "person")
  location_ann <- Maxent_Entity_Annotator(kind = "location")
  organization_ann <- Maxent_Entity_Annotator(kind = "organization")
  pipeline <- list(sent_ann
                   , word_ann
                   , person_ann
                   , location_ann
                   , organization_ann
                   )
  
  #annotate function
  txt <- String(doc) #cast to string
  annotations <- annotate(doc, pipeline)
  txt_doc <- AnnotatedPlainTextDocument(doc,annotations)
  persons <- entities(txt_doc, kind = "person")
  locations <- entities(txt_doc, kind = "location")
  organizations <- entities(txt_doc, kind = "organization")
  annotations_df <- t(as.data.frame(c("persons"=persons)))
  if ("persons" %in% colnames(annotations_df)) {
    colnames(annotations_df)[colnames(annotations_df)=="persons"] <- "persons1"
  }
  return(annotations_df)
}