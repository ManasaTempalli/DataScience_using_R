
plot_all=function(terms,freq,min_freq){
  
  df_new=data.frame(term=terms,frequency=freq)
  
  rownames(df_new)=df_new$term
  
  hist_terms=rownames(subset(df_new,df_new$frequency>min_freq))
  
  hist_freq=subset(df_new,df_new$frequency>min_freq)$frequency
  wof=data.frame(word=hist_terms,freq=hist_freq)
  
  
  
  p1=ggplot(data=wof,aes(word, freq)) +
    geom_bar(stat="identity", fill="darkred", colour="blue") +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  
  p1
}

plot_word=function(terms,freq,min_freq_wordcloud){
  
  p2=wordcloud(terms,freq,min.freq = min_freq_wordcloud,random.order=FALSE,scale=c(3,0.5))
  p2
}

plot_wordc=function(terms,freq,min_freq_wordcloud){
  p3=wordcloud(terms,freq,min.freq = min_freq_wordcloud,random.order=FALSE,scale=c(3,0.25),colors=brewer.pal(6, "Dark2"))
  p3
}

