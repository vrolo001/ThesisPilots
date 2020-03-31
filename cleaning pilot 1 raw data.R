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
    #4: if dempgraphics don't go in line with hypotheses requirements (i.e., participant not a member of the broader
         #UK community, and participant is non heterosexual and/or non cisgender)


data <- data %>%
  mutate(withdraw = ifelse(is.na(withdraw), 2, withdraw)) %>%  #we change NAs to 2s because those are the
  filter(withdraw == 2 & sexmoral_39 == 1 & otherrel_D == 1)   #responses that we include in the analyses

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

all(data$sex == data$gender)                   #command yields TRUE meaning all participants are cisgender, no need to
                                               #filter by whether their sex matches their gender

data <- data %>%
  filter(sexorien == 1)                       #hetero participants only, for immigration, we had no internationals



### COMPUTING THE CONSTRUCTS ###

#reverse code item soiatt_3, fematt_3
  #way 1, did not work but interesting concept, although needs another package that takes up space memory
install.packages("psych")
library(psych)
data <- reverse.code(keys = c("soiatt_3", "fematt_3"), items = data, mini = 1, max = 9) #error keeps appearing, possible haven and psych don't mix

  #way 2, more practical, does not require another package. Add +1 to the max score possible of scale and subtract the variable
data <- data %>%
  mutate(soiatt_3R = 10-soiatt_3, fematt_3R = 10-fematt_3 )

#Adding participant SOI, perceived female SOI, sexual morality, self-religiosity, other-religiosity)

data <- data %>%
  mutate(soibhv = (soibhv_1 + soibhv_2 + soibhv_3)/3,
         soiatt = (soiatt_1 + soiatt_2 + soiatt_3R)/3,
         soides = (soides_1 + soides_2 + soides_3)/3,
         fembhv = (fembhvr1 + fembhvr2 + fembhvr3)/3,
         fematt = (fematt_1 + fematt_2 + fematt_3R)/3,
         femdes = (femdes_1 + femdes_2 + femdes_3)/3)


