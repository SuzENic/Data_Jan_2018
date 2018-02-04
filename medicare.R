medicare <- function() {
	  Patient_Experience = read.csv(file = "Physician_Compare_2015_Group_Public_Reporting_-_Patient_Experience.csv", sep = ",")
	  Performance_Scores = read.csv(file = "Physician_Compare_2015_Group_Public_Reporting___Performance_Scores.csv", sep = ",")
	  Individual = read.csv(file = "Physician_Compare_2015_Individual_EP_Public_Reporting___Performance_Scores.csv", sep = ",")
	  National = read.csv(file = "Physician_Compare_National_Downloadable_File.csv", sep = ",")
	  #names(Patient_Experience)
	  #names(Performance_Scores)
	  #names(Individual)
	  #names(National)

	  options(digits=10)

	  length(unique(National$NPI))# find numer of unique identifiers #part 1

	  ratio <- table(National$Gender, National$Credential) # find the ratio of Female to Male practitioners #part 2
	  u <- is.finite(ratio[1,]/ratio[2,]) # get rid of bad values 
	  max(ratio[1,u]/ratio[2,u]) # find the max

	  v <- as.data.frame(table(Individual$NPI)) #part 3
	  q <-subset(v, v[,2] >= 10)
	  m<- as.factor(Individual$NPI)
	  n <- (m %in% q[,1])
	  p <- with(Individual, tapply(Measure.Performance.Rate[n], NPI[n], mean))
	  sd(p)

	  combo <- merge(x = National, y = Individual, by = "NPI", all = TRUE)# part 4
	  MD_only <-subset(combo, combo$Credential == "MD")
	  NP_only <-subset(combo, combo$Credential == "NP")
	  NP_mean <- mean(NP_only$Measure.Performance.Rate, na.rm=TRUE)
	  MD_mean <- mean(MD_only$Measure.Performance.Rate, na.rm=TRUE)
	  MD_mean - NP_mean

	  r <- table(National$Gender) # part 5
	  r[2]/r[1]

	  cuts <- table( National$Hospital.affiliation.CCN.1, National$State)# part 6
	  #looking and seeing which states had a low number to see if any were small enough to be under the 10 thtreshold
	  MP_only <-subset(National, National$State == "MP") 
	  #only one small enough to check. All others bigger by at least an order of magnitude
	  unique(MP_only$Hospital.affiliation.CCN.1)
	  unique(MP_only$Hospital.affiliation.CCN.2)
	  unique(MP_only$Hospital.affiliation.CCN.3)
	  unique(MP_only$Hospital.affiliation.CCN.4)
	  unique(MP_only$Hospital.affiliation.CCN.5) #combo results in 14 different hospital IDs

	  w <- as.data.frame(table(MD_only$NPI))# part 7
	  names(w) <- c("NPI", "Freq")
	  n <-merge(w, MD_only, by = "NPI")
	  m <-subset(n, n$Freq > 10 )
	  q <-subset(m, 1973 <= m$Graduation.year )
	  u <-subset(q, q$Graduation.year <= 2003 )
	  MD_cut_mean <- with(u, tapply(Measure.Performance.Rate, Graduation.year, na.rm=TRUE))
	  MD_lm <- lm(MD_cut_mean~Graduation.year, data = u)
	  summary(MD_lm)
	  
          w1 <- as.data.frame(table(NP_only$NPI))# part 8
          names(w1) <- c("NPI", "Freq")
          n1 <-merge(w1, NP_only, by = "NPI")
          m1 <-subset(n1, n1$Freq > 10 )
          q1 <-subset(m1, 1973 <= m1$Graduation.year )
          u1 <-subset(q1, q1$Graduation.year <= 2003 )
          NP_cut_mean <- with(u1, tapply(Measure.Performance.Rate, Graduation.year, na.rm=TRUE))
          NP_lm <- lm(NP_cut_mean~Graduation.year, data = u1)
	  t.test(MD_cut_mean,NP_cut_mean, var.equal=TRUE, paired=FALSE)
}