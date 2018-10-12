library(dplyr)
library(ggplot2)
#organize and clean data

#Set working directory
setwd("C:/Users/Daniel/surfdrive/R/detect_effect/daniel/responses")

#Read excel files in folder
files <- list.files(pattern = "\\.csv$")

#read in all seperate data files into a single list
#gives warning that can be ignored (last line not empty)
datalist = lapply(files, function(x) read.table(x, header = F))

#determine max length to fill dataframe
max_length <- max(unlist(lapply(datalist,length)))

#fill dataframe
df_filled <- lapply(datalist,function(x) {
  ans <- rep(NA,length=max_length);
  ans[1:length(x)]<- x;
  return(ans)
  })
#combine lists into a dataframe
all_data <- as.data.frame(do.call(rbind, df_filled))

#Unlist and turn into dataframe
all_data <- as.data.frame(apply(all_data,2,as.numeric))
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

library(ggplot2)
#plot observed mean difference across conditions
ggplot(all_data_sub,aes(x=obs_mean_dif))+geom_histogram() + 
  facet_grid(~effect_size_direction*judgement) +
  theme_bw()

ggplot(all_data_sub,aes(x=d))+geom_histogram() + 
  facet_grid(~effect_size_direction*judgement) +
  theme_bw()

ggplot(all_data_sub,aes(x=-d, color = effect_size_direction))+
  geom_density() +
  facet_grid(judgement~.) +
  theme_bw()

ggplot(all_data_sub,aes(x=obs_mean_dif, color = effect_size_direction))+
  geom_density() +
  facet_grid(judgement~.) +
  theme_bw()


#Calculate X% of data
find_cover_region <- function(x, alpha=0.80) {
  n <- length(x)
  x <- sort(x)
  k <- as.integer(round((1-alpha) * n))
  i <- which.min(x[seq.int(n-k, n)] - x[seq_len(k+1L)])
  c(x[i], x[n-k+i-1L])
}
tapply(all_data_sub$d, all_data_sub$judgement, find_cover_region)
tapply(all_data_sub$obs_mean_dif, all_data_sub$judgement, find_cover_region)


ggplot(all_data_sub,aes(x=pvalues))+geom_histogram() + 
  facet_grid(effect_size_direction~significant*judgement) +
  theme_bw()

ggplot(all_data_sub,aes(x=pvalues))+geom_histogram() + 
  facet_grid(effect_size_direction~judgement) +
  theme_bw()


library(dplyr)
#mean power
data <- group_by(all_data_sub, effect_size_direction, judgement)

summarize(data, power = mean(obs_power, na.rm = T))
summarize(data, mean_dif = mean(obs_mean_dif, na.rm = T))
summarize(data, d = mean(-d, na.rm = T)) #note -d because d in dataset is calculated in opposite diferection!

#Analyze p-values
data <- group_by(all_data_sub, effect_size_direction, judgement)
summarize(data, pvaluessig = sum(pvalues <= 0.05), pvalues_nonsig = sum(pvalues > 0.05), power = sum(pvalues <= 0.05)/(sum(pvalues > 0.05) + sum(pvalues <= 0.05))*100, mean_dif = mean(obs_mean_dif, na.rm = T))
