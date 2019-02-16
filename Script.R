getwd()
setwd("C:/Users/user/Desktop/Master TIc/S2/Sociologie de l_internet/SMI-TP")
install.packages("twitteR")
install.packages("ROAuth")
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("RColorBrewer")
install.packages("plyr")
install.packages("syuzhet")

library(tm)
library(twitteR)
library(ROAuth)
library(wordcloud)
library(SnowballC)
library(plyr)
library(syuzhet)

consumer_key <- "TBKrfnZg68eAk8yC7M2KgOPwg"
consumer_secret <- "Ihas78xvDzEJXV0NR7Yr1HsoPytw8nHHUUrxiqx6fjXeIEUjZ5"
access_token <-	"744020890791321610-aObwkPZf5vLq7775T7bbf6DdZfVuXhE"
access_secret <-"GUkaWvzwFO7sQVtBAwzyBMprqExCecvF6cdxq1tNsetDN"

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem") #downloads the certificate

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

cred <- OAuthFactory$new(consumerKey=consumer_key, 
       consumerSecret= consumer_secret,
       requestURL = 'https://api.twitter.com/oauth/request_token',
       accessURL = 'https://api.twitter.com/oauth/access_token',
       authURL = 'https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")

--------------------------------------------------------------------


#copier la liste dans une structure data.frame 
df <- twListToDF(OPPOF7Tweets)	

#dimensions 
print(dim(df))
print(df[1:100,c('created','screenName','isRetweet','retweeted','retweetCount','statusSource')])

#sauvegarde de la structure data.frame dans un fichier textes
write.table(df,"tweets.txt",sep="\t",quote=F)

#comptage du nombre de message par auteurs 
comptage <- table(df$screenName)

#tri décroissant 
comptage <- sort(comptage,decreasing=TRUE) 

#affichage des 10 premiers 
print(comptage[1:50])

#non redondonce 
print(length(unique(df$screenName)))

#nous utilisons la variable comptage définie précédemment 
barplot(comptage [comptage >= 5], las = 2,cex.names=0.7,col="cornsilk")

#liste des messages originaux
 id_originaux <- which(!df$isRetweet) 

#nombre de messages originaux 
print(length(id_originaux))

#comptage du nombre de message par auteurs 
comptage_bis <- table(df$screenName[id_originaux]) 

#tri décroissant 
comptage_bis <- sort(comptage_bis,decreasing=TRUE) 

#graphique de ceux qui ont plus de 5 (inclus) messages originaux
barplot(comptage_bis [comptage_bis >= 5], las = 2,cex.names=0.7, col = "tan")

##numéro des messages qui sont des retweets 
idRetweets <- which(df$isRetweet) 

#Savoir le type de device
OPPOF7Tweets[[1]]$statusSource

#vecteur du compteur de retweet 
#pour les messages retweetés 
nombre_retweets <- df$retweetCount[idRetweets] 

#index de tri décroissant selon le nombre 
index <- order(nombre_retweets,decreasing=TRUE)

#récupération du data.frame trié selon le nombre de retweets 
#on ne travaille que sur les retweets (df$isRetweet) 
dfRetweet <- df[df$isRetweet,][index,] 

#première occurrence de chaque exemplaire de tweet 
first <- !duplicated(dfRetweet$text) 

#affichage des $2$ premiers éléments 
print(dfRetweet$text[first][1:2])

#data.frame correspondant aux premières occurrences 
dfFirst <- dfRetweet[first,] 

#graphique du nombre de retweets des messages les plus populaires 
barplot(dfFirst$retweetCount[1:15], names.arg= dfFirst$id[1:15],las = 2,cex.names=0.7)

#afficher l'histogramme des fréquences du nombre de retweets. 
hist(dfFirst$retweetCount,main="Histogramme",col="slategray2",xlab="Nombre de retweets")

#data.frame avec les messages uniques 
#les premières occurrences sont récupérées 
dfUnique <- df[!duplicated(df$text),]

#nombre de tweets concernés 
print(nrow(dfUnique))

#vecteur avec les messages 
messages <- dfUnique$text 

#taille du vecteur - vérification 
print(length(messages))

-------------

devices <-(df$statusSource) 
print(devices)

retrait du saut de ligne \n 
msgClean1 <- gsub("\n"," ",devices) 

#retrait des URL 
msgClean1 <- gsub('http\\S+\\s*',"",msgClean1) 
msgClean1 <- gsub('a href\\S+\\s*',"",msgClean1)
msgClean1 <- gsub('a href\\S+\\s*',"",msgClean1)
msgClean1 <- gsub('<*',"",msgClean1)
msgClean1 <- gsub('/a>*',"",msgClean1)

#retrait des espaces en trop 
msgClean1 <- gsub("\\s+"," ",msgClean1) 

#retrait des "\" 
msgClean1 <- gsub("[\\]","",msgClean1) 

#retrait des espaces en fin de texte 
msgClean1 <- gsub("\\s*$","",msgClean1) 

#harmonisation de la casse - tout mettre en minuscule 
msgClean1 <- tolower(msgClean1) 

#retrait des accents 
msgClean1 <- gsub("[éèê]","e",msgClean1) 
msgClean1 <- gsub("[àâ]","a",msgClean1) 
msgClean1 <- gsub("[ùû]","u",msgClean1)

#retrait de l'indicateur de retweet 
msgClean <- gsub("rt ","",msgClean) 

print(msgClean1)
print(msgClean)
barplot(dfFirst$devices[1:15], names.arg= dfFirst$id[1:15],las = 2,cex.names=0.7)

comptage1 <- table(df$statusSource)

#tri décroissant 
comptage1 <- sort(comptage1,decreasing=TRUE) 

#affichage des 10 premiers 
print(comptage1[1:50])

#non redondonce 
print(length(unique(df$screenName)))

#nous utilisons la variable comptage définie précédemment 
barplot(comptage1 [comptage1 >= 5], las = 2,cex.names=0.7,col="cornsilk")


#retrait du saut de ligne \n 
msgClean <- gsub("\n"," ",messages) 

#retrait des URL 
msgClean <- gsub('http\\S+\\s*',"",msgClean) 

#retrait des espaces en trop 
msgClean <- gsub("\\s+"," ",msgClean) 

#retrait des "\" 
msgClean <- gsub("[\\]","",msgClean) 

#retrait des espaces en fin de texte 
msgClean <- gsub("\\s*$","",msgClean) 

#harmonisation de la casse - tout mettre en minuscule 
msgClean <- tolower(msgClean) 

#retrait des accents 
msgClean <- gsub("[éèê]","e",msgClean) 
msgClean <- gsub("[àâ]","a",msgClean) 
msgClean <- gsub("[ùû]","u",msgClean)

#retrait de l'indicateur de retweet 
msgClean <- gsub("rt ","",msgClean) 

#vérification - affichage du document 8 
print(msgClean[8])

#enlever les doublons 
msgClean <- msgClean[!duplicated(msgClean)] 

#nombre de messages 
print(length(msgClean))
print(msgClean)

-----------------------------Analyse des sentiments-------

word.df <- as.vector(msgClean)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(msgClean, emotion.df) 

head(emotion.df2)
emotion.df


sent.value <- get_sentiment(word.df)

most.positive <- word.df[sent.value == max(sent.value)]

most.positive


most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

positive.tweets <- word.df[sent.value > 0]

head(positive.tweets)


negative.tweets <- word.df[sent.value < 0] 
head(negative.tweets)


neutral.tweets <- word.df[sent.value == 0] 
head(neutral.tweets)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
 
head(category_senti)

category_senti2 <- cbind(OPPOF7Tweets,category_senti,senti)
head(category_senti2)
tweets category_senti senti

table(category_senti)
barplot( table(category_senti), las = 2,cex.names=0.7,col="cornsilk")
c category_senti

------------------------------

 

#récupérer l'ensemble des mots délimtés par des ESPACE 
all_mots <- unlist(strsplit(msgClean," ")) 
all_mots
#un mot est un hashtag s'il débute par # 
signature_hashtag <- regexpr("^#[[:alnum:]_]*",all_mots)

#récupérer l'ensemble des thèmes désignés par un "#" dans les messages 
liste_hashtags <- regmatches(all_mots,signature_hashtag) 

#nombre de hashtags recensés 
print(length(liste_hashtags))

#nombre d'apparition de chaque hashtag 
nb_hashtags <- table(liste_hashtags) 

#tri selon la fréquence décroissante 
tri_nb_hashtags <- sort(nb_hashtags,decreasing=TRUE) 

#affichage des 10 hastags les plus fréquents 
print(tri_nb_hashtags[1:10])

#affichage sauf #OPPOF7
wordcloud(names(tri_nb_hashtags)[-1],tri_nb_hashtags[-1],scale=c(3,.5),colors=brewer.pal(6, "Dark2"))

#liste des hashtags contenant le théme dominant et leur nombre d'appartition 
hashtags_realchampion <- nb_hashtags[grep("realchampion",names(nb_hashtags))] 
print(sort(hashtags_realchampion,decreasing=TRUE))

#Que l'on peut mettre sous forme graphique : 
barplot(sort(hashtags_realchampion,decreasing=TRUE),las=2,cex.names=0.7,col="dodgerblue")

#un mot désigne un individu (pseudo) s'il débute par @ 
signature_individu <- regexpr("^@[[:alnum:]_]*",all_mots) 

#récupérer l'ensemble des thèmes désignés par un "#" dans les messages 
liste_individus <- regmatches(all_mots,signature_individu) 

#nombre de hashtags recensés 
print(length(liste_individus))
print(liste_individus)
#Ici également, nous pouvons repérer les noms d'auteurs qui apparaissent le plus fréquemment dans les messages. 
#nombre d'apparition des individus 
nb_individus <- table(liste_individus)

#tri selon la fréquence décroissante 
tri_nb_individus <- sort(nb_individus,decreasing=TRUE) 

#affichage des 10 auteurs les plus fréquents 
print(tri_nb_individus[1:15])

mat1<-findFreqTerms(mdt,lowfreq=2)
mat1
sentiment = score.sentiment(mat1,pos.mat1)

-----------------------------------------

#retrait des hashtags 
msgCleanBis <- gsub("#[[:alnum:]_]*( |:|$)","",msgClean) 
#retrait des pseudos 
msgCleanBis <- gsub("@[[:alnum:]_]*( |:|$)","",msgCleanBis) 
#vérification avec le document n°8 
print(msgCleanBis[8])
#importation de la libraire 
library(tm) 
## Loading required package: NLP 
#transformation de la liste des tweets en un format interne 
corpus <- Corpus(VectorSource(msgCleanBis)) 
print(corpus)

#retrait des ponctuations 
corpus <- tm_map(corpus,removePunctuation) 
#retrait des nombres 
corpus <- tm_map(corpus,removeNumbers) 
#retrait des stopwords (mots outils) 
corpus <- tm_map(corpus,removeWords,stopwords("french")) 
#retirer les espaces en trop (s'il en reste encore) 
corpus <- tm_map(corpus,stripWhitespace) 
#vérification avec le document n°8 
print(corpus[[8]]$content)
#création de la MDT à partir du corpus 
mdt <- DocumentTermMatrix(corpus,control=list(weighting=weightBin)) 
print(mdt)

#termes apparaissant au moins 60 fois 
print(findFreqTerms(mdt,30))

m <- as.matrix(mdt) 
print(dim(m))

#frequence des mots 
freqMots <- colSums(m)
#tri par ordre décroissant 
freqMots <- sort(freqMots,decreasing=TRUE)
#affichage des 10 mots les plus fréquents 
print(freqMots[1:50])

#termes n'apparaissant qu'une fois 
print(length(which(freqMots<=2)))
#ne conserver que les termes apparaissant plus de 2 fois dans la matrice 
mClean <- m[,colSums(m) > 2] 
print(dim(mClean))
#affichage sauf #presidentielle 2017 
wordcloud(colnames(mClean),colSums(mClean),min.freq=10,scale=c(2,.5),colors=brewer.pal(6, "Dark2"))
install.packages("arules")
library(arules)
#paramètres de l'extraction des itemsets 
parametres <- list(supp=0.01,minlen=4, maxlen=5,target="frequent itemsets") 
#extraction des itemsets 
itemsets <- apriori(mClean,parameter=parametres)
#affichage 
inspect(itemsets)
mdt
--------------------------------------------------------------------------
library(stringr)
str_split(corpus, pattern="\\s+")
mat <- str_split(corpus, pattern="\\s+")
mat
class(mat)

libraryinstall.packages("shiny")
library(shiny)
runApp("Front End/")