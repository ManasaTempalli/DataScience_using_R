
create_text=function(path){
  
  textfile_each_line <- readLines(path)#Convert text into lines
  head(textfile_each_line)
  text=paste(readLines('AJA_Factiva-20180306-2258.txt'),collapse=' ')#Wrap into a single string
  
  
  if (grepl("NYT", text)) {
    # remove the first couple of lines (header from Factiva)
    alldata <- sub("Factiva\r\n\r\n\r\n", "", text)
    split.word <- "Document NYT(.*)"
  } else if (grepl("WSJ", text)) {
    # remove the first couple of lines (header from Factiva)
    alldata <- sub("Factiva\r\r", "", text)
    split.word <- "Document J(.*)"
  } else if (grepl("AJA", text)) {
    # remove the first couple of lines (header from Factiva)
    alldata <- sub("Factiva\r\n\r\n\r\n", "", text)
    split.word <- "Document AJAZEN(.*)" 
  } else if (grepl("GRDN", text)){
    # remove the first couple of lines (header from Factiva)
    alldata <- sub("Factiva\r\n\r\n\r\n", "", text)
    split.word <- "Document GRDN(.*)" 	
  } else {
    # remove the first couple of lines (header from Factiva)
    alldata <- sub("Factiva\r\n\r\n\r\n", "", text)
    split.word <- "Document T(.*)" 
  }
  # split the input data into individual articles using the splitword (hint: at this point there should 72 elements)
  
  
  list_alldata_splitted <- strwrap(alldata, width = 0.9 * getOption("width"), indent = 0,exdent = 0, prefix = "", simplify = TRUE)
  return (alldata)
}
