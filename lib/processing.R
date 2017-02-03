
#### function update.tables ####
# this function takes as an input a tibble and the list of presidents to keep and returns 
# .. the subset table
update.tables <- function(arg1, arg2){
  
  arg1$document <- substr(arg1$document, 6, nchar(arg1$document)-4)
  names(arg1)[names(arg1)=="term"] <- "word"
  arg1$president <- substr(arg1$document, 0, nchar(arg1$document)-2)
  arg1$term <- substr(arg1$document, nchar(arg1$document), 
                          nchar(arg1$document))
  arg1 <- arg1[arg1$president %in% as.list(arg2), ]
  
  return(arg1)
}

##### funtion flow ####
# this function takes as an input the name of the president and the corpus and return 
# .. a sentiment score for each section of his speech (the number of 
# .. sections has eben defined to 20)
flow <- function(corpus, president, term){
  # process the text
  text <- paste0("inaug",president,"-",term,".txt")
  speech <- data_frame(text = corpus[[text]]$content)
  speech <- unnest_tokens(speech, word, text)
  speech$order <- c(1:nrow(speech))
  nb.words <- nrow(speech)
  
  # upload sentiments
  speech <- inner_join(speech,
                       get_sentiments("nrc"),
                       by=c("word"="word"))
  speech$count <- 1
  speech <- spread(speech, sentiment, count, fill=0)
  speech <- mutate(speech, sentiment = positive - negative)
  speech <- group_by(speech, order, word, sentiment)
  
  # compute sections
  nb.sections = 20
  speech$section <- floor(speech$order*nb.sections/nb.words)
  
  # compute sentiment score per section
  speech.flow <- summarise(group_by(speech, section), score = sum(sentiment))
  return(speech.flow)
}