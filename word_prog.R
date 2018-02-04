word_prog <- function(){
	  library(devtools)
	  library(rword2vec)
	  setwd("/Users/senichol/Desktop/Data_Inc/Data_Jan_2018")
	  data <- read.csv("hate_speech/data/labeled_data.csv")
	  train=data$tweet
	  train=tolower(train)
	  train=gsub("[[:punct:]]", "", train)
	  write(train,"text_data.txt")
	  model=word2vec(train_file = "text_data.txt",output_file ="model1.bin",layer1_size = 300,min_count = 40,num_threads = 4,window = 10,sample = 0.001,binary=1)
	  bin_to_txt("model1.bin","model1text.txt")
	  
	  dist=distance(file_name = "model1.bin",search_word = "bitch",num = 10)
	  dist2=distance(file_name = "model1.bin",search_word = "ass",num = 10)

	  pdf(file="Plot1.pdf")	  
	  plot(data$bitch, data$ass, na.rm=TRUE, xlim = c(.93, 1.01), ylim = c(.86, 1.02), xlab = "Bitch", ylab = "Ass")
	  text(data$bitch, data$ass+.015, data$X.1)
	  dev.off()

	  ## model=word2vec(train_file = "hate_speech/data/labeled_data.csv",output_file = "vec.bin",binary=0)
	  ###/Library/Frameworks/R.framework
}