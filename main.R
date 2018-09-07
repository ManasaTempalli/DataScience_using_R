#loading libraries
library(tm)
library(tidytext)
library(topicmodels)
library(stringr)
library(wordcloud)
library(SnowballC) 
library(RColorBrewer) 
library(ggplot2)

sink("C:/Users/MANASA/Downloads/RFinalProject_Manasa_Tempalli/console_out.txt")
#Source
source('C:/Users/MANASA/Downloads/RFinalProject_Manasa_Tempalli/dataLoading.R')
source('C:/Users/MANASA/Downloads/RFinalProject_Manasa_Tempalli/plots.R')



#Function for dividing the text into articles
path='AJA_Factiva-20180306-2258.txt'
alldata=create_text(path)

#Cleaning Data
given=alldata
new=gsub(pattern='\\W',replace=" ",given)
new=gsub(pattern='\\d',replace=" ",new)
new=tolower(new)
new=removeWords(new,stopwords())
new=removeWords(new,c('al','jazeera','copyright','ajazen','english','march','february'))
new=gsub(pattern='\\b[A-z]\\b{1}',replace=' ',new)
new=stripWhitespace(new)


korp=Corpus(VectorSource(new))

dtm=DocumentTermMatrix(korp)

tdm =TermDocumentMatrix(korp)

tdm_matrix=as.matrix(tdm)
dtm_matrix=as.matrix(dtm)

#calculate frequency of occurences
df=as.data.frame(tdm_matrix)

#Plot Function
colnames(df)=c('frequency')
freq=df$frequency
df$words=rownames(df)

#number of terms
total_no_terms= sum(freq)

#generating CSV file with frequencies
df_descend=df[order(df$frequency,decreasing = TRUE),]
rownames(df_descend)=1:length(df$frequency)
head(df_descend)
write.csv(df_descend, file = "frequency.csv")

#Plotting
colnames(df)=c('frequency')
df$terms=rownames(df)
png("Black.png")
plot_word(df$terms,df$frequency,70)
dev.off()
png("Color.png")
plot_wordc(df$terms,df$frequency,70)
dev.off()

png("Plot_all.png")
plot_all(df$terms,df$frequency,100)
dev.off()
#_____________________________________________________________________
#______________________________________________________________________

#Use the following parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 5
findFreqTerms(dtm, lowfreq = 20)
#Run LDA using Gibbs sampling
ldaoutput <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

##write out results

ldaoutput.topics <- as.matrix(topics(ldaoutput))
write.csv(ldaoutput.topics,file=paste("LDA",k,"DocsToTopics.csv"))

ldaoutput.terms <- as.matrix(terms(ldaoutput,6))
write.csv(ldaoutput.terms,file=paste("LDA",k,"TopicsToTerms.csv"))

sink()