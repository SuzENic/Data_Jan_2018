word_prog <- function(){
	  library(devtools)
	  library(rword2vec)
	  setwd("/Users/senichol/Desktop/Data_Inc/Data_Jan_2018")
	  data <- read.csv("hate_speech/data/labeled_data.csv") # get the imput corpus
	  train=data$tweet					# make the training input
	  train=tolower(train)					# make everything lower case because he and He are the same.
	  train=gsub("[[:punct:]]", "", train)			# punctuation will not be matched so take it out
	  write(train,"text_data.txt")				#put training in a text file
	  							# run word2vec and generate the model
	  model=word2vec(train_file = "text_data.txt",output_file ="model1.bin",layer1_size = 300,min_count = 40,num_threads = 4,window = 10,sample = 0.001,binary=1)
	  bin_to_txt("model1.bin","model1text.txt")		  #output model to text file
	  
	  dist=distance(file_name = "model1.bin",search_word = "bitch",num = 10)	#calculate nearest neighbors for 2 words
	  dist2=distance(file_name = "model1.bin",search_word = "ass",num = 10)

	  pdf(file="Plot1.pdf")								#scatter plot of closeness
	  plot(data$bitch, data$ass, na.rm=TRUE, xlim = c(.93, 1.01), ylim = c(.86, 1.02), xlab = "Bitch", ylab = "Ass", main = "Cos Distances Between Words")
	  text(data$bitch, data$ass+.015, data$X.1)
	  dev.off()

	  pdf(file="Plot2.pdf")								#Cluster diagram  
	  dist=distance(file_name = "model1.bin",search_word = "bitch",num = 40)
	  hdist <- matrix(1:40, ncol = 1)
	  rownames(hdist) <- dist$word
	  hdist[,1] <- as.numeric(levels(dist[,2]))[dist[,2]]
	  hc2 = hclust(dist(hdist))
	  plot(hc2, main = "Cluster Dendrogram for Bitch")
	  dev.off()

	  ### this is where I will test what I have so far, look for similarities in tweets. 
	  ### This is where I will test models to look for improvements in sematics. 
	      	      	      ### They will include, but nto be limited to...
			      ### logistic regression
			      ### Principal Component Analysis
			      ### Tensor Flow
	  data2=as.data.frame(read.table("model1.txt",skip=1, fill = TRUE)) ### gives me the raw vectors

	  data <- read.csv("hate_speech/data/labeled_data.csv")
	  #tw1<- "bad bitches is the only thing that i like"
	  for (m in 1:length(data$tweet)) {
	  	  tw1<- data[m,]$tweet
		  tw1=tolower(tw1)
		  tw1 = gsub("[[:punct:]]", "", tw1) 		  
	  	  tw1 <- strsplit(tw1, " ")
		  	  	# data2[which(data2$V1==tw1[[1]][1])]
				# for (i in (1:9)) {tw2[i] <- rbind(data2[which(data2$V1==tw1[[1]][i])]) }
	  	  for (i in (1:length(tw1[[1]]))){
	       		j <- which(data2$V1==tw1[[1]][i])
			tw2 <- matrix(0.0, length(tw1[[1]]), 300)
		        if (length(j) != 0) {
		       		for (k in (1:300)){
		       	   	    tw2[i, k]<- (data2[j, k])
		       		}
		        }
		  }	  
	  	  tw3[m,] <- apply(tw2,2,mean)
		  tw3[m,1] <- data[m,]$class
	  }
	  write.table(tw3,"text_cats.txt")

	  tw3=as.data.frame(read.table("text_cats.txt", fill = TRUE))
	  colnames(tw3) <- c("Type", paste0("V", 2:300)) 
	  tw3 <- as.data.frame(tw3)
	  tw3$Type <- as.factor(tw3$Type)
	  source("myplclust.R")
	  distMat <- dist(tw3[,2:300])
	  hclustering <- hclust(distMat)
	  myplclust(hclustering, lab.col = unclass(tw3$Type))

train<- tw3[1:20000,]
 test<- tw3[20001:24783,]
model = multinom(Type~.,data=train)
fitting_results <- predict(model,newdata=test,type='response')
}