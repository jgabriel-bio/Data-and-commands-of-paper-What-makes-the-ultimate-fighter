####Commands to estimate the better proxy for RHP of amberwing dragonfly###              

####Importing data####
rhp=read.csv2('Proxy_RHP_ptenera_Symmetrical_Contests.csv',h=T,stringsAsFactors = T,dec=',')

####Testing correlation between variables####
# Packages
library(ggplot2)
library(gridExtra)

# Function to create a grid of scatterplots for a specific column against others
create_scatterplot_figure <- function(data, col_index, other_indices) {
  plot_list <- list()
  for (i in other_indices) {
    if (i != col_index) {
      p <- ggplot(data, aes_string(x = names(data)[col_index], y = names(data)[i])) +
        geom_point() +
        theme_minimal() +
        labs(
          title = paste(names(data)[col_index], "vs", names(data)[i]),
          x = names(data)[col_index],
          y = names(data)[i]
        ) +
        theme(
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 6)
        )
      plot_list[[length(plot_list) + 1]] <- p
    }
  }
  return(do.call(grid.arrange, c(plot_list, ncol = 2)))
}

# Generate and save figures for each column against all others
for (col_index in 7:14) {
  other_indices <- 7:14
  plot_grid <- create_scatterplot_figure(rhp, col_index, other_indices)
  ggsave(paste0("scatterplot_figure_", col_index, ".png"), plot_grid, width = 12, height = 8)
}

#The scatterplot indicate that some variables have a non-linear correlation, would not allow us to use this variables together in the same model. This would lead to a higher probability of a error type 1. So we run models with each variable separeatly

####Running models#####

#Fight pair id was inserted as a random variable in all models, but was discarted lately due the lack of effect

#####Model - Wing length####

#Searching for outliers
plot(id_focal_outcome~wing_length, bty='l',ylab='Probability of winning',xlab="Scaled wing length (g)",bty="l",rhp)

#Command to mantain only complete line in the analysis, to turn them comparable
rhp_wing_length = rhp[complete.cases(rhp),] 

#Scaling Wing length
rhp_wing_length$scaled_wing_length <- scale(rhp_wing_length$wing_length) [,1]

#Full model
model_wing_length=glm(id_focal_outcome~scaled_wing_length,family="binomial",data=rhp_wing_length)

#Model results
summary(model_wing_length)

# Calculate mean and sd for wing length 
mean_winners <- mean(rhp_wing_length$wing_length[rhp_wing_length$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_wing_length$wing_length[rhp_wing_length$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd for wing length 
mean_losers <- mean(rhp_wing_length$wing_length[rhp_wing_length$id_focal_outcome == "0"], na.rm = TRUE) ; print(mean_losers)

sd_losers <- sd(rhp_wing_length$wing_length[rhp_wing_length$id_focal_outcome == "0"], na.rm = TRUE); print(sd_losers)

#Null model
model__wing_length_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_wing_length)

#Comparing models
anova(model_wing_length,model__wing_length_h0,test='Chi')
#P=0.42

#####Figure Wing length####
model_log_wing_length=glm(id_focal_outcome~scaled_wing_length,family=binomial,data=rhp_wing_length)

plot(id_focal_outcome~scaled_wing_length, bty='l',ylab='Probability of winning',xlab="Scaled wing length (g)",bty="l",rhp_wing_length) #no outlier

curve(predict(model_log_wing_length,data.frame(scaled_wing_length=x), type="resp"), add=T)

#####Model - Fresh mass####

#Searching for outliers
plot(id_focal_outcome~fresh_weight, bty='l',ylab='Probability of winning',xlab="Fresh weight (g)",bty="l",rhp) #no outlier

rhp = rhp[complete.cases(rhp),] 

#Scaling Fresh weight
rhp$scaled_fresh_weight <- scale(rhp$fresh_weight)[,1]

#Full model
model_fresh_weight=glm(id_focal_outcome~scaled_fresh_weight,family="binomial",data=rhp)

#Model results
summary(model_fresh_weight)

# Calculate mean and sd for fresh weight
mean_winners <- mean(rhp$fresh_weight[rhp$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp$fresh_weight[rhp$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd for fresh weight
mean_losers <- mean(rhp$fresh_weight[rhp$id_focal_outcome == "0"], na.rm = TRUE)  ; print(mean_losers)

sd_losers <- sd(rhp$fresh_weight[rhp$id_focal_outcome == "0"], na.rm = TRUE); print(sd_losers)

#Null model
model_fresh_weight_h0=glm(id_focal_outcome~1,family="binomial",data=rhp)

#Comparing models
anova(model_fresh_weight,model_fresh_weight_h0,test='Chi')
#P=0.06

#####Figure Fresh mass####
model_log_fresh_weight=glm(id_focal_outcome~scaled_fresh_weight,family=binomial,data=rhp)

plot(id_focal_outcome~scaled_fresh_weight, bty='l',ylab='Probability of winning',xlab="Scaled fresh weight (g)",bty="l",rhp)

curve(predict(model_log_fresh_weight,data.frame(scaled_fresh_weight=x), type="resp"), add=T)

#####Model - Thorax dry mass####

#Searching for outliers
plot(id_focal_outcome~thorax_dry_weight, bty='l',ylab='Probability of winning',xlab="Thorax dry weight (g)",bty="l",rhp)

#Taking off the outlier
library(dplyr)
rhp_thorax_dry_weight <- rhp %>% filter(!row_number() %in% c(25,54))

#Leaving only complete lines
rhp_thorax_dry_weight = rhp_thorax_dry_weight[complete.cases(rhp_thorax_dry_weight),] 

#Scaling Thorax dry weight
rhp_thorax_dry_weight$scaled_thorax_dry_weight<- scale(rhp_thorax_dry_weight$thorax_dry_weight)[,1]

#Full model
model_thorax_dry_weight=glm(id_focal_outcome~scaled_thorax_dry_weight,family="binomial",data=rhp_thorax_dry_weight)

#Model results
summary(model_thorax_dry_weight)

# Calculate mean and sd for Thorax dry weight
mean_winners <- mean(rhp_thorax_dry_weight$thorax_dry_weight[rhp_thorax_dry_weight$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_thorax_dry_weight$thorax_dry_weight[rhp_thorax_dry_weight$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd Thorax dry weight
mean_losers <- mean(rhp_thorax_dry_weight$thorax_dry_weight[rhp_thorax_dry_weight$id_focal_outcome == "0"], na.rm = TRUE); print(mean_losers)

sd_losers <- sd(rhp_thorax_dry_weight$thorax_dry_weight[rhp_thorax_dry_weight$id_focal_outcome == "0"], na.rm = TRUE); print(sd_losers)

#Null model
model_thorax_dry_weight_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_thorax_dry_weight)

#Comparing models
anova(model_thorax_dry_weight,model_thorax_dry_weight_h0,test='Chi')
#P=0.03

#####Figure Thorax dry mass####
model_log_thorax_dry_weight=glm(id_focal_outcome~scaled_thorax_dry_weight,family=binomial,data=rhp_thorax_dry_weight)

plot(id_focal_outcome~scaled_thorax_dry_weight, bty='l',ylab='Probability of winning',xlab="Scaled thorax dry weight (g)",bty="l",rhp_thorax_dry_weight) 

curve(predict(model_log_thorax_dry_weight,data.frame(scaled_thorax_dry_weight=x), type="resp"), add=T)

#####Model - Abdomen dry mass####

#Searching for outliers
plot(id_focal_outcome~abdomen_dry_weight, bty='l',ylab='Probability of winning',xlab="Abdomen dry weight (g)",bty="l",rhp)

#Leaving only complete lines
rhp_abdomen_dry_weight = rhp[complete.cases(rhp),] 

#Leaving only complete lines
rhp_abdomen_dry_weight = rhp_abdomen_dry_weight[complete.cases(rhp_abdomen_dry_weight),] 

#Scaling Thorax dry weight
rhp_abdomen_dry_weight$scaled_abdomen_dry_weight<- scale(rhp_abdomen_dry_weight$abdomen_dry_weight)[,1]

#Full model
model_abdomen_dry_weight=glm(id_focal_outcome~scaled_abdomen_dry_weight,family="binomial",data=rhp_abdomen_dry_weight)

#Model results
summary(model_abdomen_dry_weight)

# Calculate mean and sd for Abdomen dry weight
mean_winners <- mean(rhp_abdomen_dry_weight$abdomen_dry_weight[rhp_abdomen_dry_weight$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_abdomen_dry_weight$abdomen_dry_weight[rhp_abdomen_dry_weight$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd Abdomen dry weight
mean_losers <- mean(rhp_abdomen_dry_weight$abdomen_dry_weight[rhp_abdomen_dry_weight$id_focal_outcome == "0"], na.rm = TRUE); print(mean_losers)

sd_losers <- sd(rhp_abdomen_dry_weight$abdomen_dry_weight[rhp_abdomen_dry_weight$id_focal_outcome == "0"], na.rm = TRUE) ; print(sd_losers)


#Null model
model_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_abdomen_dry_weight)

#Comparing models
anova(model_abdomen_dry_weight,model_h0,test='Chi')
#P=0.01

#####Figure Abdomen dry mass####
model_log_abdomen_dry_weight=glm(id_focal_outcome~scaled_abdomen_dry_weight,family=binomial,data=rhp_abdomen_dry_weight)

plot(id_focal_outcome~scaled_abdomen_dry_weight, bty='l',ylab='Probability of winning',xlab="Scaled abdomen dry weight (g)",bty="l",rhp_abdomen_dry_weight) 

curve(predict(model_log_abdomen_dry_weight,data.frame(scaled_abdomen_dry_weight=x), type="resp"), add=T)

#####Model - Total dry mass####

#Searching for outliers
plot(id_focal_outcome~total_dry_weight, bty='l',ylab='Probability of winning',xlab="Total dry weight (g)",bty="l",rhp_total_dry_weight)

#Taking off the outlier
library(dplyr)
rhp_total_dry_weight <- rhp %>% filter(!row_number() %in% c(25,54))

#Leaving only complete lines
rhp_total_dry_weight = rhp[complete.cases(rhp),] 

#Scaling Total dry weight
rhp_total_dry_weight$scaled_total_dry_weight<- scale(rhp_total_dry_weight$total_dry_weight)[,1]

#Full model
model_total_dry_weight=glm(id_focal_outcome~scaled_total_dry_weight,family="binomial",data=rhp_total_dry_weight) 

#Model results
summary(model_total_dry_weight)

# Calculate mean and sd for Total dry weight
mean_winners <- mean(rhp_total_dry_weight$total_dry_weight[rhp_total_dry_weight$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_total_dry_weight$total_dry_weight[rhp_total_dry_weight$id_focal_outcome == "1"], na.rm = TRUE); print(sd_winners)

# Calculate mean and sd Total dry weight
mean_losers <- mean(rhp_total_dry_weight$total_dry_weight[rhp_total_dry_weight$id_focal_outcome == "0"], na.rm = TRUE) ; print(mean_losers)

sd_losers <- sd(rhp_total_dry_weight$total_dry_weight[rhp_total_dry_weight$id_focal_outcome == "0"], na.rm = TRUE) ; print(sd_losers)

#Null model
model_total_dry_weight_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_total_dry_weight)

#Comparing models
anova(model_total_dry_weight,model_total_dry_weight_h0,test='Chi')


#####Figure Total dry mass####
model_log_total_dry_weight=glm(id_focal_outcome~scaled_total_dry_weight,family=binomial,data=rhp_total_dry_weight)

plot(id_focal_outcome~scaled_total_dry_weight, bty='l',ylab='Probability of winning',xlab="Scaled total dry weight (g)",bty="l",rhp_total_dry_weight)

curve(predict(model_log_total_dry_weight,data.frame(scaled_total_dry_weight=x), type="resp"), add=T)


#####Model - Thorax fat mass####

#Searching for outliers
plot(id_focal_outcome~thorax_fat_weight, bty='l',ylab='Probability of winning',xlab="Thorax fat weight (g)",bty="l",rhp)

#Taking off the outlier
library(dplyr)
rhp_thorax_fat_weight <- rhp  %>% slice(-c(39))

View(rhp_thorax_fat_weight)

#Leaving only complete lines
rhp_thorax_fat_weight = rhp_thorax_fat_weight[complete.cases(rhp_thorax_fat_weight),] 

#Scaling Thorax fat weight
rhp_thorax_fat_weight$scaled_thorax_fat_weight<- scale(rhp_thorax_fat_weight$thorax_fat_weight)[,1]

#Full model
model_thorax_fat_weight=glm(id_focal_outcome~scaled_thorax_fat_weight,family="binomial",data=rhp_thorax_fat_weight)

#Model results
summary(model_thorax_fat_weight)

# Calculate mean and sd for Thorax fat weight
mean_winners <- mean(rhp_thorax_fat_weight$thorax_fat_weight[rhp_thorax_fat_weight$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_thorax_fat_weight$thorax_fat_weight[rhp_thorax_fat_weight$id_focal_outcome == "1"], na.rm = TRUE); print(sd_winners)

# Calculate mean and sd Thorax fat weight
mean_losers <- mean(rhp_thorax_fat_weight$thorax_fat_weight[rhp_thorax_fat_weight$id_focal_outcome == "0"], na.rm = TRUE) ; print(mean_losers)

sd_losers <- sd(rhp_thorax_fat_weight$thorax_fat_weight[rhp_thorax_fat_weight$id_focal_outcome == "0"], na.rm = TRUE) ; print(sd_losers)

#Null model
model_thorax_fat_weight_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_thorax_fat_weight)

#Comparing models
anova(model_thorax_fat_weight,model_thorax_fat_weight_h0,test='Chi')
#P=0.01

#####Figure Thorax fat mass####
model_log_thorax_fat_weight=glm(id_focal_outcome~scaled_thorax_fat_weight,family=binomial,data=rhp_thorax_fat_weight)

plot(id_focal_outcome~scaled_thorax_fat_weight, bty='l',ylab='Probability of winning',xlab="Scaled thorax fat weight (g)",bty="l",rhp_thorax_fat_weight) #no outlier

curve(predict(model_log_thorax_fat_weight,data.frame(scaled_thorax_fat_weight=x), type="resp"), add=T)


#####Model - Abdomen fat mass####

#Searching for outliers
plot(id_focal_outcome~abdomen_fat_weight, bty='l',ylab='Probability of winning',xlab="Abdomen fat weight (g)",bty="l",rhp)

#Leaving only complete lines
rhp_abdomen_fat_weight = rhp[complete.cases(rhp),] 

#Scaling Total dry weight
rhp_abdomen_fat_weight$scaled_abdomen_fat_weight<- scale(rhp_abdomen_fat_weight$abdomen_fat_weight)[,1]

#Full model
model_abdomen_fat_weight=glm(id_focal_outcome~scaled_abdomen_fat_weight,family="binomial",data=rhp_abdomen_fat_weight)

#Model results
summary(model_abdomen_fat_weight)

# Calculate mean and sd for Abdomen fat weight
mean_winners <- mean(rhp_abdomen_fat_weight$abdomen_fat_weight[rhp_abdomen_fat_weight$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_abdomen_fat_weight$abdomen_fat_weight[rhp_abdomen_fat_weight$id_focal_outcome == "1"], na.rm = TRUE); print(sd_winners)

# Calculate mean and sd Abdomen fat weight
mean_losers <- mean(rhp_abdomen_fat_weight$abdomen_fat_weight[rhp_abdomen_fat_weight$id_focal_outcome == "0"], na.rm = TRUE) ; print(mean_losers)

sd_losers <- sd(rhp_abdomen_fat_weight$abdomen_fat_weight[rhp_abdomen_fat_weight$id_focal_outcome == "0"], na.rm = TRUE); print(sd_losers)

#Null model
model_abdomen_fat_weight_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_abdomen_fat_weight)

#Comparing models
anova(model_abdomen_fat_weight,model_abdomen_fat_weight_h0,test='Chi')
#P=0.003

#####Figure Abdomen fat mass####
model_log_abdomen_fat_weight=glm(id_focal_outcome~scaled_abdomen_fat_weight,family=binomial,data=rhp_abdomen_fat_weight)

plot(id_focal_outcome~scaled_abdomen_fat_weight, bty='l',ylab='Probability of winning',xlab="Scaled abdomen fat weight (g)",bty="l",rhp_abdomen_fat_weight) 

curve(predict(model_log_abdomen_fat_weight,data.frame(scaled_abdomen_fat_weight=x), type="resp"), add=T)

#####Model - Total fat mass####

#Searching for outliers
plot(id_focal_outcome~total_fat_weight, bty='l',ylab='Probability of winning',xlab="Total fat weight (g)",bty="l",rhp)

rhp_total_fat_weight = rhp[complete.cases(rhp),] 

View(rhp_total_fat_weight)

#Leaving only complete lines
rhp_total_fat_weight= rhp_total_fat_weight[complete.cases(rhp_total_fat_weight),] 

#Scaling Total dry weight
rhp_total_fat_weight$scaled_total_fat_weight<- scale(rhp_total_fat_weight$total_fat_weight)[,1]

#Full model
model_total_fat_weight=glm(id_focal_outcome~scaled_total_fat_weight,family="binomial",data=rhp_total_fat_weight)

#Model results
summary(model_total_fat_weight)

# Calculate mean and sd for Total fat weight
mean_winners <- mean(rhp_total_fat_weight$total_fat_weight[rhp_total_fat_weight$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_total_fat_weight$total_fat_weight[rhp_total_fat_weight$id_focal_outcome == "1"], na.rm = TRUE); print(sd_winners)

# Calculate mean and sd Total fat weight
mean_losers <- mean(rhp_total_fat_weight$total_fat_weight[rhp_total_fat_weight$id_focal_outcome == "0"], na.rm = TRUE)  ; print(mean_losers)

sd_losers <- sd(rhp_total_fat_weight$total_fat_weight[rhp_total_fat_weight$id_focal_outcome == "0"], na.rm = TRUE) ; print(sd_losers)


#Null model
model_total_fat_weight_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_total_fat_weight)

#Comparing models
anova(model_total_fat_weight,model_total_fat_weight_h0,test='Chi')
#P=0.01

#####Figure Total fat mass####
model_log_total_fat_weight=glm(id_focal_outcome~scaled_total_fat_weight,family=binomial,data=rhp_total_fat_weight)

plot(id_focal_outcome~scaled_total_fat_weight, bty='l',ylab='Probability of winning',xlab="Scaled total fat weight (g)",bty="l",rhp_total_fat_weight) 

curve(predict(model_log_total_fat_weight,data.frame(scaled_total_fat_weight=x), type="resp"), add=T)

#####Model - Residuals fat thorax####

#Creating the residuals value of thorax fat#

#Creating data frame with only complete lines
rhp = rhp[complete.cases(rhp),] 

#Linear regression between fat weight and fresh weight
model_r1 <- lm(thorax_fat_weight ~ thorax_dry_weight, data = rhp)

# Extract the residuals
residuals_thorax <- resid(model_r1)

# Add the residuals as a new column in the data frame
rhp$residuals_thorax <- residuals_thorax

#Searching for outliers
plot(id_focal_outcome~residuals_thorax, bty='l',ylab='Probability of winning',xlab="Residuals of thorax fat weight in relation to thorax fresh weigh (g)",bty="l",rhp)

#Taking off the outlier
rhp_residuals_thorax <- rhp %>% filter(!row_number() %in% c(25))

#Leaving only complete lines
rhp_residuals_thorax = rhp_residuals_thorax[complete.cases(rhp_residuals_thorax),] 

#Scaling Residuals thorax weight
rhp_residuals_thorax$scaled_residuals_thorax<- scale(rhp_residuals_thorax$residuals_thorax)[,1]

#Full model
model_residuals_thorax=glm(id_focal_outcome~scaled_residuals_thorax,family="binomial",data=rhp_residuals_thorax)

#Model results
summary(model_residuals_thorax)

# Calculate mean and sd for Residuals fat thorax 
mean_winners <- mean(rhp_residuals_thorax$residuals_thorax[rhp_residuals_thorax$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_residuals_thorax$residuals_thorax[rhp_residuals_thorax$id_focal_outcome == "1"], na.rm = TRUE); print(sd_winners)

# Calculate mean and sd Residuals fat thorax
mean_losers <- mean(rhp_residuals_thorax$residuals_thorax[rhp_residuals_thorax$id_focal_outcome == "0"], na.rm = TRUE) ; print(mean_losers)

sd_losers <- sd(rhp_residuals_thorax$residuals_thorax[rhp_residuals_thorax$id_focal_outcome == "0"], na.rm = TRUE) ; print(sd_losers)

#Null model
model_thorax_residuals_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_residuals_thorax)

#Comparing models
anova(model_residuals_thorax,model_thorax_residuals_h0,test='Chi')


#####Figure Thorax residuals of fat weight####

model_log_residuals_thorax=glm(id_focal_outcome~scaled_thorax_residuals,family=binomial,data=rhp)

plot(id_focal_outcome~scaled_thorax_residuals, bty='l',ylab='Probability of winning',xlab="Scaled residuals thorax fat weight (g)",bty="l",rhp) 

curve(predict(model_log_residuals_thorax,data.frame(scaled_thorax_residuals=x), type="resp"), add=T)

#####Model - Residuals fat abdomen####

#Creating the residuals value of abdomen fat

#Creating data frame with only complete lines
rhp = rhp[complete.cases(rhp),] 

#Linear regression between fat weight and fresh weight
model_r2 <- lm(abdomen_fat_weight ~ abdomen_dry_weight, data = rhp)

# Extract the residuals
residuals_abdomen <- resid(model_r2)

# Add the residuals as a new column in the data frame
rhp$residuals_abdomen <- residuals_abdomen

#Searching for outliers
plot(id_focal_outcome~residuals_abdomen, bty='l',ylab='Probability of winning',xlab="Residuals of abdomen fat weight in relation to abdomen fresh weigh (g)",bty="l",rhp)

#Leaving only complete lines
rhp_residuals_abd = rhp[complete.cases(rhp),] 

#Scaling Residuals thorax weight
rhp_residuals_abd$scaled_residuals_abdomen<- scale(rhp_residuals_abd$residuals_abdomen)[,1]

#Full model
model_residuals_abdomen=glm(id_focal_outcome~scaled_residuals_abdomen,family="binomial",data=rhp_residuals_abd)

#Model results
summary(model_residuals_abdomen)

# Calculate mean and sd for Residuals fat abdomen
mean_winners <- mean(rhp_residuals_abd$residuals_abdomen[rhp_residuals_abd$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_residuals_abd$residuals_abdomen[rhp_residuals_abd$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd Residuals fat abdomen
mean_losers <- mean(rhp_residuals_abd$residuals_abdomen[rhp_residuals_abd$id_focal_outcome == "0"], na.rm = TRUE); print(mean_losers)

sd_losers <- sd(rhp_residuals_abd$residuals_abdomen[rhp_residuals_abd$id_focal_outcome == "0"], na.rm = TRUE); print(sd_losers)

#Null model
model_abdomen_residuals_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_residuals_abd)

#Comparing models
anova(model_residuals_abdomen,model_abdomen_residuals_h0,test='Chi')
#P=0.52

#####Figure Abdomen residuals of fat weight####

model_log_residuals_abdomen=glm(id_focal_outcome~scaled_residuals_abdomen,family=binomial,data=rhp_residuals_abd)

plot(id_focal_outcome~scaled_residuals_abdomen, bty='l',ylab='Probability of winning',xlab="Scaled residuals abdomen fat weight (g)",bty="l",rhp_residuals_abd) 

curve(predict(model_log_residuals_abdomen,data.frame(scaled_residuals_abdomen=x), type="resp"), add=T)


######Model - Reflection - Hue####

#Searching for outliers
plot(id_focal_outcome~hue_reflec, bty='l',ylab='Probability of winning',xlab="Hue of Reflectance",bty="l",rhp)

##Outliers
library(dplyr)
rhp_hue_reflec <- rhp %>% filter(!row_number() %in% c(4,10))

#Leaving only complete lines
rhp_hue_reflec= rhp_hue_reflec[complete.cases(rhp_hue_reflec),] 

#Scaling ds
rhp_hue_reflec$scaled_hue_reflec<- scale(rhp_hue_reflec$hue_reflec)[,1]

#Full model
model_total_hue_reflec=glm(id_focal_outcome~scaled_hue_reflec,family="binomial",data=rhp_hue_reflec)

#Model results
summary(model_total_hue_reflec)

# Calculate mean and sd for ds of reflectance
mean_winners <- mean(rhp_hue_reflec$hue_reflec[rhp_hue_reflec$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_hue_reflec$hue_reflec[rhp_hue_reflec$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd Total fat weight
mean_losers <- mean(rhp_hue_reflec$hue_reflec[rhp_hue_reflec$id_focal_outcome == "0"], na.rm = TRUE) ; print(mean_losers)

sd_losers <- sd(rhp_hue_reflec$hue_reflec[rhp_hue_reflec$id_focal_outcome == "0"], na.rm = TRUE) ; print(sd_losers)


#Null model
model_total_hue_reflec_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_hue_reflec)

#Comparing models
anova(model_total_hue_reflec,model_total_hue_reflec_h0,test='Chi')
#P=0.15

######Model - Reflection - Chroma####

#Searching for outliers
plot(id_focal_outcome~chroma_reflec, bty='l',ylab='Probability of winning',xlab="Chroma of Reflectance",bty="l",rhp)

##Outliers
library(dplyr)
rhp_chroma_reflec <- rhp %>% filter(!row_number() %in% c(60))

#Leaving only complete lines
rhp_chroma_reflec= rhp[complete.cases(rhp),] 

#Scaling Total dry weight
rhp_chroma_reflec$scaled_chroma_reflec<- scale(rhp_chroma_reflec$chroma_reflec)[,1]

#Full model
model_total_chroma_reflec=glm(id_focal_outcome~scaled_chroma_reflec,family="binomial",data=rhp_chroma_reflec)

#Model results
summary(model_total_chroma_reflec)

# Calculate mean and sd for Total fat weight
mean_winners <- mean(rhp_chroma_reflec$chroma_reflec[rhp_chroma_reflec$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_chroma_reflec$chroma_reflec[rhp_chroma_reflec$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd Total fat weight
mean_losers <- mean(rhp_chroma_reflec$chroma_reflec[rhp_chroma_reflec$id_focal_outcome == "0"], na.rm = TRUE)  ; print(mean_losers)

sd_losers <- sd(rhp_chroma_reflec$chroma_reflec[rhp_chroma_reflec$id_focal_outcome == "0"], na.rm = TRUE); print(sd_losers)


#Null model
model_total_chroma_reflec_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_chroma_reflec)

#Comparing models
anova(model_total_chroma_reflec,model_total_chroma_reflec_h0,test='Chi')
#P=0.85


######Model - Reflection - Brightness####

#Searching for outliers
plot(id_focal_outcome~brightness_reflec, bty='l',ylab='Probability of winning',xlab="Brightness of Reflectance",bty="l",rhp)

##Outliers
library(dplyr)
rhp_brightness_reflec <- rhp %>% filter(!row_number() %in% c(10))

#Leaving only complete lines
rhp_brightness_reflec= rhp_brightness_reflec[complete.cases(rhp_brightness_reflec),] 

#Scaling Total dry weight
rhp_brightness_reflec$scaled_brightness_reflec<- scale(rhp_brightness_reflec$brightness_reflec)[,1]

#Full model
model_total_brightness_reflec=glm(id_focal_outcome~scaled_brightness_reflec,family="binomial",data=rhp_brightness_reflec)

#Model results
summary(model_total_brightness_reflec)

# Calculate mean and sd for Total fat weight
mean_winners <- mean(rhp_brightness_reflec$brightness_reflec[rhp$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_brightness_reflec$brightness_reflec[rhp$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd Total fat weight
mean_losers <- mean(rhp_brightness_reflec$brightness_reflec[rhp$id_focal_outcome == "0"], na.rm = TRUE)  ; print(mean_losers)

sd_losers <- sd(rhp_brightness_reflec$brightness_reflec[rhp$id_focal_outcome == "0"], na.rm = TRUE); print(sd_losers)


#Null model
model_total_brightness_reflec_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_brightness_reflec)

#Comparing models
anova(model_total_brightness_reflec,model_total_brightness_reflec_h0,test='Chi')
#P=0.50

######Model - Transmittance - Hue####

#Searching for outliers
plot(id_focal_outcome~hue_transm, bty='l',ylab='Probability of winning',xlab="Hue of Transmittance",bty="l",rhp)

#Leaving only complete lines
rhp_hue_transm= rhp[complete.cases(rhp),] 

#Scaling Total dry weight
rhp_hue_transm$scaled_hue_transm<- scale(rhp_hue_transm$hue_transm)[,1]

#Full model
model_total_hue_transm=glm(id_focal_outcome~scaled_hue_transm,family="binomial",data=rhp_hue_transm)

#Model results
summary(model_total_hue_transm)

# Calculate mean and sd for Total fat weight
mean_winners <- mean(rhp_hue_transm$hue_transm[rhp_hue_transm$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_hue_transm$hue_transm[rhp_hue_transm$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd Total fat weight
mean_losers <- mean(rhp_hue_transm$hue_transm[rhp_hue_transm$id_focal_outcome == "0"], na.rm = TRUE)  ; print(mean_losers)

sd_losers <- sd(rhp_hue_transm$hue_transm[rhp_hue_transm$id_focal_outcome == "0"], na.rm = TRUE); print(sd_losers)


#Null model
model_total_hue_transm_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_hue_transm)

#Comparing models
anova(model_total_hue_transm,model_total_hue_transm_h0,test='Chi')
#P=0.47


######Model - Transmittance - Chroma####

#Searching for outliers
plot(id_focal_outcome~chroma_transm, bty='l',ylab='Probability of winning',xlab="Chroma of Transmittance",bty="l",rhp)

#Leaving only complete lines
rhp_chroma_transm= rhp[complete.cases(rhp),] 

#Scaling Total dry weight
rhp_chroma_transm$scaled_chroma_transm<- scale(rhp_chroma_transm$hue_transm)[,1]

#Full model
model_total_chroma_transm=glm(id_focal_outcome~scaled_chroma_transm,family="binomial",data=rhp_chroma_transm)

#Model results
summary(model_total_chroma_transm)

# Calculate mean and sd for Total fat weight
mean_winners <- mean(rhp_chroma_transm$chroma_transm[rhp_chroma_transm$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_chroma_transm$chroma_transm[rhp_chroma_transm$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd Total fat weight
mean_losers <- mean(rhp_chroma_transm$chroma_transm[rhp_chroma_transm$id_focal_outcome == "0"], na.rm = TRUE) ; print(mean_losers)

sd_losers <- sd(rhp_chroma_transm$chroma_transm[rhp_chroma_transm$id_focal_outcome == "0"], na.rm = TRUE) ; print(sd_losers)


#Null model
model_total_chroma_transm_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_chroma_transm)

#Comparing models
anova(model_total_chroma_transm,model_total_chroma_transm_h0,test='Chi')
#P=0.47

######Model - Transmittance - Brightness####


#Searching for outliers
plot(id_focal_outcome~brightness_transm, bty='l',ylab='Probability of winning',xlab="Brightness of Transmittance",bty="l",rhp)

#Leaving only complete lines
rhp_brightness_transm= rhp[complete.cases(rhp),] 

#Scaling Total dry weight
rhp_brightness_transm$scaled_brightness_transm<- scale(rhp_brightness_transm$brightness_transm)[,1]

#Full model
model_total_brightness_transm=glm(id_focal_outcome~scaled_brightness_transm,family="binomial",data=rhp_brightness_transm)

#Model results
summary(model_total_brightness_transm)

# Calculate mean and sd for Total fat weight
mean_winners <- mean(rhp_brightness_transm$brightness_transm[rhp_brightness_transm$id_focal_outcome == "1"], na.rm = TRUE) ; print(mean_winners)

sd_winners <- sd(rhp_brightness_transm$brightness_transm[rhp_brightness_transm$id_focal_outcome == "1"], na.rm = TRUE) ; print(sd_winners)

# Calculate mean and sd Total fat weight
mean_losers <- mean(rhp_brightness_transm$brightness_transm[rhp_brightness_transm$id_focal_outcome == "0"], na.rm = TRUE) ; print(mean_losers)

sd_losers <- sd(rhp_brightness_transm$brightness_transm[rhp_brightness_transm$id_focal_outcome == "0"], na.rm = TRUE) ; print(sd_losers)


#Null model
model_total_brightness_transm_h0=glm(id_focal_outcome~1,family="binomial",data=rhp_brightness_transm)

#Comparing models
anova(model_total_brightness_transm,model_total_brightness_transm_h0,test='Chi')
#P=0.35