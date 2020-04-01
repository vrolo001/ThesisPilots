library(haven)
library(tidyverse)
#set working directory to be whatever file your data is stored in
data <- read_spss("Pilot1RAWDATA.sav")

### REMOVING PARTICIPANTS BASED ON EXCLUSION CRITERIA ###
    #1: if participants answered Yes to having their data withdrawn after debrief
        #form aka if varaible withdraw = 1
    #2: if participants failed ANY of the attention checks
        #sexmoral_39 should equal 1
        #otherrel_D should equal 1
    #3: if participants failed 2 or more of the True/False reading comprehension test
        #all answers are True-True-False-True regardless of condition
        #names for varaibles are: read1 read2 read3 read4 read1.0 read2.0 read3.0 read4.0
                                 #read1.1 read2.1 read3.1 read4.1
    #4: if demographics don't go in line with hypotheses requirements (i.e., participant not a member of the broader
         #UK community, and participant is non heterosexual and/or non cisgender)
    #5: if own SOI and female SOI are +/-3 SDs from the mean after computing constructs


all(data$sex == data$gender)                   #command yields TRUE meaning all participants are cisgender, no need to
                                               #filter by whether sex matches their gender. We had no international students
                                               #so no need to worry about immigration variable for our purposes

data <- data %>%
  mutate(withdraw = ifelse(is.na(withdraw), 2, withdraw)) %>%  #we change NAs to 2s because those are the
  filter(withdraw == 2 & sexmoral_39 == 1 & otherrel_D == 1 &  #responses that we include in the analyses
         sexorien == 1)                                        #2nd line is attention checks & asking to withdraw
                                                               #3rd line is demograpchics

data <- data %>%
  group_by(condition) %>%
  mutate(read1_c = ifelse(read1 == 1, 1, 0),                  #if answer for Q1 is T, give 1 point, if not give 0
         read2_c = ifelse(read2 == 1, 1, 0),                  #if answer for Q2 is T, give 1 point, if not give 0
         read3_c = ifelse(read3 == 2, 1, 0),                  #if answer for Q3 is F, give 1 point, if not give 0
         read4_c = ifelse(read4 == 1, 1, 0),                  #if answer for Q4 is T, give 1 point, if not give 0
         rscore_c = read1_c + read2_c + read3_c + read4_c,    #adding reading comprehension scores of control condition
         read1_l = ifelse(read1.0 == 1, 1, 0),                #repeat procedure for the other conditions              
         read2_l = ifelse(read2.0 == 1, 1,0),
         read3_l = ifelse(read3.0 == 2, 1, 0),
         read4_l = ifelse(read4.0 == 1, 1, 0),
         rscore_l = read1_l + read2_l + read3_l + read4_l,
         read1_h = ifelse(read1.1 == 1, 1, 0),
         read2_h = ifelse(read2.1 == 1, 1, 0),
         read3_h = ifelse(read3.1 == 2, 1, 0),
         read4_h = ifelse(read4.1 == 1, 1, 0),
         rscore_h = read1_h + read2_h + read3_h+ read4_h,
         ) %>%
  ungroup()

data <- data %>%
  filter(rscore_c > 2 | is.na(rscore_c),       #you need to include missing values for the summation of scores
         rscore_l > 2 | is.na(rscore_l),       #because participants in the high contidion, for example, will have
         rscore_h > 2 | is.na(rscore_h))       #their scores in the control condition (rscore_c) as NA since they
                                               #never completed these questions



### COMPUTING THE CONSTRUCTS ###
  #1: Reverse code 3rd item of participant attitude SOI and female attitude SOI
  #2: Compute means for following constructs:
     #a) Participant SOI: including SOI behaviours, attitudes, and desires
     #b) Perceived female SOI: including SOI behaviours, attitudes, and desires
     #c) Sexual morality: 7 pillars being unfaithfulness, short-term mating, coercion, outgroup sex,
        #romantic sex, homosexuality, and atypical sex
     #d) Self religiosity
     #e) "Other" religiosity: the degree to which participants think others should be more religious


#reverse code item soiatt_3, fematt_3
  #way 1, did not work but interesting concept, although needs another package that takes up space memory
install.packages("psych")
library(psych)
data <- reverse.code(keys = c("soiatt_3", "fematt_3"), items = data, mini = 1, max = 9) #error keeps appearing, possible haven and psych don't mix

  #way 2, more practical, does not require another package. Add +1 to the max score possible of scale and subtract the variable
data <- data %>%
  mutate(soiatt_3R = 10-soiatt_3, fematt_3R = 10-fematt_3 )

#Scoring constructs

data$soibhv <- rowMeans(data[, 22:24], na.rm = TRUE)
data$soiatt <- rowMeans(data[, c(25:26, 200)], na.rm = TRUE)
data$soides <- rowMeans(data[, 28:30], na.rm = TRUE)
data$soitot <- rowMeans(data[, c(22:26, 200, 28:30)], na.rm = TRUE)
data$fembhv <- rowMeans(data[, 94:96], na.rm = TRUE)
data$fematt <- rowMeans(data[, c(97:98, 201)], na.rm = TRUE)
data$femdes <- rowMeans(data[, 100:102], na.rm = TRUE)
data$femtot <- rowMeans(data[, c(94:98, 201, 100:102)], na.rm = TRUE)
data$unfaith <- rowMeans(data[, c(103, 110, 117, 124, 131, 138)], na.rm = TRUE)
data$stm    <- rowMeans(data[, c(104, 111, 118, 125, 132, 139)], na.rm = TRUE)
data$coerce <- rowMeans(data[, c(105, 112, 119, 126, 133)], na.rm = TRUE)
data$outgrp <- rowMeans(data[, c(106, 113, 120, 127, 134)], na.rm = TRUE)
data$romant <- rowMeans(data[, c(107, 114, 121, 128, 135, 140)], na.rm = TRUE)
data$homo   <- rowMeans(data[, c(108, 115, 122, 129, 136)], na.rm = TRUE)
data$atypic <- rowMeans(data[, c(109, 116, 123, 130, 137)], na.rm = TRUE)
data$relself <- rowMeans(data[, 146:151], na.rm = TRUE)
data$othrrel <- rowMeans(data[, 141:145], na.rm = TRUE)
  
### Removing outliers


### Save new data file
write.csv(data, "Pilot1 Clean Data R")
