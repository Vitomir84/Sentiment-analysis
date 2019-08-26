#Sentiment analysis

#Otvaranje naloga na http://apps.twitter.com.
#Uzima se api, secret api, access token, secret token

ckey <- HJIAUkCJkRYuvr7G354aFUsPJ
skey <- e1yHRCElXgNMO3d9vVwh4MTdylALl9rIRgkpPNhZaWGfqzc2xU
token <- 464636983-ZXEtDfTD9NekqmbqKMoqpts6Tn5AORK1pNE00Pf6
sectoken <- FxyZCv5kNl1ZtlBU7OSOzNulUYCUEAt7ssr0rnE3vCXT9

install.packages('tm',repos='http://cran.us.r-project.org')
install.packages('twitteR',repos='http://cran.us.r-project.org')
install.packages('wordcloud',repos='http://cran.us.r-project.org')
install.packages('RColorBrewer',repos='http://cran.us.r-project.org')
install.packages('e1017',repos='http://cran.us.r-project.org')
install.packages('class',repos='http://cran.us.r-project.org')

library(tm)
library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(class)

#Povezivanje na twitter

setup_twitter_oauth("HJIAUkCJkRYuvr7G354aFUsPJ", "e1yHRCElXgNMO3d9vVwh4MTdylALl9rIRgkpPNhZaWGfqzc2xU", access_token = "464636983-ZXEtDfTD9NekqmbqKMoqpts6Tn5AORK1pNE00Pf6", access_secret =  "FxyZCv5kNl1ZtlBU7OSOzNulUYCUEAt7ssr0rnE3vCXT9")

#Pretraživanje određene reči na tw - može da se bira broj tweetova i jezik

poezija <- searchTwitter("sns", n=1000, lang = "sr")
head(poezija)

#Kreiramo sentiment analizu - želimo da vidimo koliko četiri kompanije u svojim tvitovima koriste pozitivne
# i negativne reči i da na osnovu toga napravimo sentiment analizu. To radimo na osnovu postojećih rečnika
#pozitivnih i negativnih reči, gde onda prebrojavamo ove reči u njihovim tvitovima. 

#ubacujemo pozitivne i negativne reči

pos.words <- readLines("C:/Users/vjovanovic/Desktop/R Udemy/Vezbe udemy/reci za sentiment analizu/positive_words.txt")
neg.words <- readLines("C:/Users/vjovanovic/Desktop/R Udemy/Vezbe udemy/reci za sentiment analizu/negative_words.txt")
head(pos.words)

library(stringr)
library(plyr)



score.sentiment1 = function(sentences, pos.words, neg.words, .progress='none')
  
{
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
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
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
  
}

#skupljanje tvitova za svaku kompaniju

novartis <- searchTwitter("#novartis", n=900, lang = "en")
roche <- searchTwitter("#roche", n=900, lang = "en")
bayer<- searchTwitter("#bayer", n=900, lang = "en")


#uzimanje teksta

novartis_txt <- sapply(novartis, function(x) x$getText())
roche_txt <- sapply(roche, function(x) x$getText())
bayer_txt <- sapply(bayer, function(x) x$getText())

#broj tvitova

nd <- c(length(pepsi_txt), length(cocacola_txt), length(google_txt))
head(nd)

#pridruživanje tekstova

company <- c(pepsi_txt, cocacola_txt, google_txt)
head(company)

#primenjujemo gore kreiranu funkciju score_sentiment


scores <- score.sentiment1(company, pos.words, neg.words, .progress = "text")
head(scores)


#dodajemo varijable u bazu
scores$company <- factor(rep(c("pepsi", "cocacola", "google"), nd))
scores$very.pos <- as.numeric(scores$score >= 2)
scores$very.neg <- as.numeric(scores$score <= -2)

#koliko je veoma positivnih i negativnih tvitova

numpos <- sum(scores$very.pos)
numneg <- sum(scores$very.neg)

ukupan_skor <- 100*numpos/(numpos+numneg)

#graf
library(ggplot2)
ggplot(scores, aes(score)) + geom_bar(fill="red", color="black") + facet_wrap(~company) + xlab("Sentiment score") + ylab("number of tweets")
