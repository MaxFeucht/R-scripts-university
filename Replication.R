################################################################
## Script for direct Replication Hugenberg & Bodenhausen 2003 ##
################################################################

library(dplyr)
library(ggplot2)

setwd("/Users/maxfeucht/Documents/Uni/Projektseminar/Replikationsprojekt/Datenanalyse/Main Analysis")

df <- data.frame(read.delim("Monkey1.dat", header=TRUE))
df2 <- data.frame(read.delim("Monkey2.dat", header=TRUE))

df2$subject <- as.factor(as.character(df2$subject))

df <- rbind(df,df2)

vidtask <- df[,c(4,17,19)]



###################################
#### Implicit Association Task#####
###################################

##Script according to:
#Röhner, J., & Thoss, P. J. A tutorial on how to compute traditional IAT effects with R.

iat_raw <- read.delim("data/iat_data.dat", 
                      header = TRUE,
                      sep = "\t",
                      fill = TRUE,
                      colClasses = rep("numeric", 7))


#preprocessrawdata−−−−

iat_1 <- iat_raw %>%
  filter (
  #drop the first two trials of each block accordingto the conventional procedure
  trialnum > 2) %>%
  mutate(
    #recodelatencies
    latency = if_else(latency < 300, 300,if_else(latency > 3000, 3000, latency)),
    #naturallogarithmof respective latency
    ln_lat = log(latency)
    ) %>%
  #calculate mean for natural logarithm of latencies
  group_by(id, order, test, blocknum) %>%
  summarise(M_ln_lat = mean(ln_lat)) %>%
  ungroup()


#prepare calculation−−−−

dd <- iat_1 %>% 
  select(id, order) %>% 
  unique()

##Hier aufpassen, ob block 4 und 7 wirklich die Inkompatiblen sind!!
for (i in c(4, 7)) {
  iat_b <- iat_1 %>%
    filter(blocknum == i) %>%
    select(-c(test, blocknum))
  names(iat_b) <- c("id", "order", paste0("M_ln_lat_", i))
    dd <- full_join(dd, iat_b, by = c("id", "order"))}


# calculate score−−−−

iat_out <- dd %>%
  mutate(
    #C1 Conventional logarithmic test)
    C1 = round(case_when(order == 1 ~ (M_ln_lat_7 - M_ln_lat_4),order == 2 ~ (M_ln_lat_4 - M_ln_lat_7)), 2)
    ) %>%
  select(id,C1)

#write outputtofile−−−−
write.table(iat_out,paste0("output/C1-", Sys.Date(), ".dat"), 
            sep = "\t", 
            quote = FALSE,
            row.names = FALSE)




###########################
####Stereotype measure#####
###########################


##dataframe for stereotype measure
stereo <- df[which(df$blockcode == 'traits_in' | df$blockcode == 'traits_out'),c(4,9,10,15)]

##listing inverted items
invert <- c('d_gesetzestreu',
            'd_besonnen',
            'd_ungefaehrlich',
            'd_beitragend',
            'd_zivilisiert',
            'd_traditionell',
            'd_tolerant',
            'd_friedfertig',
            'd_geschlechtergerecht',
            'd_harmlos',
            'm_gesetzestreu',
            'm_besonnen',
            'm_ungefaehrlich',
            'm_beitragend',
            'm_zivilisiert',
            'm_traditionell',
            'm_tolerant',
            'm_friedfertig',
            'm_geschlechtergerecht',
            'm_harmlos')


#Transforming inverted items
stereo[which(stereo$trialcode %in% invert),4] <- 100 - stereo[which(stereo$trialcode %in% invert),4]

#mean response for in- and outgroup stereotypes
stereo <- stereo %>%
  group_by(subject, blockcode) %>%
  mutate(meanscore = mean(response)) 


##diagnostic ratio
stereo <- stereo %>%
  group_by(subject) %>%
  mutate(diagratio = rep((meanscore[blockcode == 'traits_out'] / meanscore[blockcode == 'traits_in']),2))



####Demographics####:

##Response = 57 --> Leertaste
##Response = 45 --> "x"
