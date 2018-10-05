library(dplyr)
library(ggplot2)

#organize and clean data

# #Set working directory
# setwd("C:/Users/Daniel/surfdrive/R/shiny_apps/detect_the_effect/data")
# 
# #Read excel files in folder
# files <- list.files(pattern = "\\.csv$")
# 
# #read in all seperate data files into a single list
# #gives warning that can be ignored (last line not empty)
# datalist = lapply(files, function(x) read.table(x, header = F)) 
# 
# #determine max length to fill dataframe
# max_length <- max(unlist(lapply(datalist,length)))
# 
# #fill dataframe
# df_filled <- lapply(datalist,function(x) {
#   ans <- rep(NA,length=max_length);
#   ans[1:length(x)]<- x;
#   return(ans)
#   })
# #combine lists into a dataframe
# all_data <- as.data.frame(do.call(rbind, df_filled))
# 
# #Unlist and turn into dataframe
# all_data <- as.data.frame(apply(all_data,2,as.numeric))
# colnames(all_data)[1:15] <- c("ID", "length", "judgement", "effect_size", "effect_size_direction", "true_mean1", "true_mean2", "obs_mean_1", "obs_mean2", "obs_mean_dif", "df", "tvalue", "pwaardes", "obs_power", "d")
# 
# 
# #Set working directory
# setwd("C:/Users/Daniel/surfdrive/R/shiny_apps/detect_the_effect/data2")
# 
# #Read excel files in folder
# files <- list.files(pattern = "\\.csv$")
# 
# #read in all seperate data files into a single list
# #gives warning that can be ignored (last line not empty)
# datalist = lapply(files, function(x) read.table(x, header = F)) 
# 
# #determine max length to fill dataframe
# max_length <- max(unlist(lapply(datalist,length)))
# 
# #fill dataframe
# df_filled <- lapply(datalist,function(x) {
#   ans <- rep(NA,length=max_length);
#   ans[1:length(x)]<- x;
#   return(ans)
# })
# #combine lists into a dataframe
# all_data_2 <- as.data.frame(do.call(rbind, df_filled))
# 
# #Unlist and turn into dataframe
# all_data_2 <- as.data.frame(apply(all_data_2,2,as.numeric))
# all_data_2 <- all_data_2[seq(2,30,2)]
# colnames(all_data_2)[1:15] <- c("ID", "length", "judgement", "effect_size", "effect_size_direction", "true_mean1", "true_mean2", "obs_mean_1", "obs_mean2", "obs_mean_dif", "df", "tvalue", "pwaardes", "obs_power", "d")
# 
# data_complete <- rbind(all_data[1:15],all_data_2)
# write.table(data_complete, "alldata.csv", sep="\t", row.names=FALSE, col.names=FALSE) 

#Read in data----
all_data <- read.table("alldata.csv") 
colnames(all_data)[1:15] <- c("ID", "length", "judgement", "effect_size", "effect_size_direction", "true_mean1", "true_mean2", "obs_mean_1", "obs_mean2", "obs_mean_dif", "df", "tvalue", "pvalues", "obs_power", "d")

#create variable for correct/incorrect----
#create variable for correct/incorrect
all_data$correct <- ifelse(all_data$judgement == 0 & (all_data$effect_size== 0) | all_data$judgement == 1 & (all_data$effect_size > 0),
                           1,
                           0)

#create variable for sig/nonsig
all_data$significant <- ifelse(all_data$pvalues <= 0.05,
                               1,
                               0)

#Plots----

#subset data ((note use of levels to deal with factor))only trials with more than 5 responses
all_data_sub <- all_data[all_data$length > 5, ] 

#plot observed mean difference across conditions
ggplot(all_data_sub,aes(x=obs_mean_dif))+geom_histogram() + 
  facet_grid(~effect_size_direction*judgement) +
  theme_bw()

ggplot(all_data_sub,aes(x=d))+geom_histogram() + 
  facet_grid(~effect_size_direction*judgement) +
  theme_bw()

ggplot(all_data_sub,aes(x=-d))+geom_histogram() + 
  facet_grid(~effect_size_direction*significant) +
  theme_bw()

#plot d as a function of effect size (0.8, 0.5,0.2,0,-0.2 etc) and judgments and significance
ggplot(all_data_sub,aes(x=-d))+geom_histogram() + 
  facet_grid(effect_size_direction~significant*judgement) +
  theme_bw()

#Plot the p-values (you see p-values are lower when they say yes)
ggplot(all_data_sub,aes(x=pvalues))+geom_histogram() + 
  facet_grid(effect_size_direction~significant*judgement) +
  theme_bw()


#mean power
data <- group_by(all_data_sub, effect_size_direction, judgement)

summarize(data, power = mean(obs_power, na.rm = T))
summarize(data, mean_dif = mean(obs_mean_dif, na.rm = T))
summarize(data, d = mean(-d, na.rm = T)) #note -d because d in dataset is calculated in opposite diferection!


#Analyze p-values
data <- group_by(all_data_sub, effect_size_direction, judgement)

summarize(data, pvaluessig = sum(pvalues <= 0.05), pvalues_nonsig = sum(pvalues > 0.05), power = sum(pvalues <= 0.05)/(sum(pvalues > 0.05) + sum(pvalues <= 0.05))*100, mean_dif = mean(obs_mean_dif, na.rm = T))

