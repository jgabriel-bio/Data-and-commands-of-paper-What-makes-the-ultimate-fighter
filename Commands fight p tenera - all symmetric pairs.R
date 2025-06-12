####Calculating the resource quality index####
index0=read.csv2('table_territory_index.csv',h=T,stringsAsFactors = T,dec=',')

#Package used 
library(dplyr)

# Summarizing the data of each day
index1 <- index0 %>%
  group_by(date, id_territory2) %>%
  summarize(
    female_visits = sum(female_visits, na.rm = TRUE),
    copulation = sum(copulation, na.rm = TRUE)
  )


# Summarizing the value of each territory calculating the index
index2 <- index1 %>%
  group_by(id_territory2) %>%
  summarise(
    quality_index = (sum(female_visits) + sum(copulation)*2) / n()
  )

View(index2)
?write.xlsx
#Exporting the table
library(openxlsx)
write.xlsx(index2, file = 'final_index.xlsx')

# Print the resulting data frame
print(index2)


####Which factors determine males decision to withdraw in each contest phase (total fat mass as a proxy to RHP)#######

#We test the hypotheses that 1) the males decision to withdraw is based on a partial contribution of their own fighting capacity, the  opponents fighting capacity and the resource value. And 2) that the males decisions to withdraw are exclusively based on their own fighting capacity, on the opponents fighting capacity or on the resource value. 

#######Importing data####
fight=read.csv2('p.tenera_contests_all_symmetric_pairs.csv',h=T,stringsAsFactors = T)

summary(fight)

#####Scaling the predictor variable####
fight$scaled_winner_total_fat_weight <- scale(fight$winner_total_fat_weight)[,1]
fight$scaled_loser_total_fat_weight <- scale(fight$loser_total_fat_weight) [,1]

#Taking of NAs data 
fight <- fight[!is.na(fight$territory_value_index),]
fight <- fight[!is.na(fight$scaled_winner_total_fat_weight), ]
fight <- fight[!is.na(fight$scaled_loser_total_fat_weight), ]

#testing multicollinearity between rhp loser, winner and territory quality 
cor(fight[,c(20,22,23)]) #Low correlations

#Package used
library(nlme)

library(car)


####FULL MODEL####
#Model with GLS (Generalised linear squares)
model_1=gls(overall_contest_duration~scaled_winner_total_fat_weight+scaled_loser_total_fat_weight+territory_value_index, weights=varPower(form=~scaled_winner_total_fat_weight), data=fight)

plot(model_1) #Residual distribution is ok 

coef(model_1)

Anova(model_1, type=3) 
# There is no relationship between overall contest duration and winner RHP (X2 = 0.10; P = 0.74)
# There is a relationship between overall contest duration and loser RHP (X2 = 7.93; P = 0.004)
# There is no relationship between overall contest duration and territory value  (X2 = 0.39; P = 0.53)

#Estimate value of each predictor
summary(model_1)


####Creating partial plot figures ####

#Loading packages
library(visreg)
library(dplyr)
library(ggplot2)
library(hrbrthemes)

#####Winner figure (Fig 2a) - Partial residual plot####
visreg(model_1, "scaled_winner_total_fat_weight", bty="l", partial = T)
coef(model_1)["scaled_winner_total_fat_weight"]


# Extracting predictions from visreg
vis_data <- visreg(model_1, "scaled_winner_total_fat_weight", partial = T, plot = FALSE)

# Convert to a data frame
vis_df <- vis_data$fit %>%
  as.data.frame() %>%
  rename(x = scaled_winner_total_fat_weight, y = visregFit, ymin = visregLwr, ymax = visregUpr)


# Plot in ggplot
tiff('Figure 2a_manuscrip version.tiff',  w=1800 , h=1200, res=300)
par(mar=c(5,4,2,2))
ggplot(vis_df, aes(x = x, y = y)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, fill = "gray") + 
  geom_point(data = fight, aes(x = scaled_winner_total_fat_weight, y = vis_data$res$visregRes), 
             color = "orange", alpha = 0.6) +
  theme_ipsum() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size =1.5), # Adds axis lines 
    axis.title.x = element_text(hjust = 0.5, size = 11, family = "Calibri",
                                margin = margin(t = 14)),
    # Center and enlarge X-axis label
    axis.title.y = element_text(hjust = 0.5, size = 11, family = "Calibri", margin = margin(r = 14))  # Center and enlarge Y-axis label and add margin
  )  +
  labs(x = "Scaled Winner Total Fat Weight", y = "Contest duration (s)") +
  annotate("text", x = -Inf, y = Inf, label = "a)", hjust = -0.6, vjust = 1.5, size = 4, fontface = "bold") 
dev.off()

#####Loser figure (Fig 2c) - Partial residual plot####

visreg(model_1, "scaled_loser_total_fat_weight", bty="l", partial = T)

# Extracting predictions from visreg
vis_data <- visreg(model_1, "scaled_loser_total_fat_weight", partial = T, plot = FALSE)

# Convert to a data frame
vis_df <- vis_data$fit %>%
  as.data.frame() %>%
  rename(x = scaled_loser_total_fat_weight, y = visregFit, ymin = visregLwr, ymax = visregUpr)


# Plot in ggplot
tiff('Figure 2b_manuscrip version.tiff',  w=1800 , h=1200, res=300)
par(mar=c(5,4,2,2))
ggplot(vis_df, aes(x = x, y = y)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, fill = "gray") + 
  geom_point(data = fight, aes(x = scaled_loser_total_fat_weight, y = vis_data$res$visregRes), 
             color = "orange", alpha = 0.6) +
  theme_ipsum() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size =1.5), # Adds axis lines 
    axis.title.x = element_text(hjust = 0.5, size = 11, family = "Calibri",
                                margin = margin(t = 14)),
    # Center and enlarge X-axis label
    axis.title.y = element_text(hjust = 0.5, size = 11, family = "Calibri", margin = margin(r = 14))  # Center and enlarge Y-axis label and add margin
  )  +
  labs(x = "Scaled Loser Total Fat Weight", y = "Contest duration (s)") +
  annotate("text", x = -Inf, y = Inf, label = "b)", hjust = -0.6, vjust = 1.5, size = 4, fontface = "bold") 
dev.off()


#####Territory index figure (Fig 2c) - Partial residual plot####

visreg(model_1, "territory_value_index", bty="l", partial = T)

# Extracting predictions from visreg
vis_data <- visreg(model_1, "territory_value_index", partial = T, plot = FALSE)

# Convert to a data frame
vis_df <- vis_data$fit %>%
  as.data.frame() %>%
  rename(x = territory_value_index, y = visregFit, ymin = visregLwr, ymax = visregUpr)


# Plot in ggplot
tiff('Figure 2c_manuscrip version.tiff',  w=1800 , h=1200, res=300)
par(mar=c(5,4,2,2))
ggplot(vis_df, aes(x = x, y = y)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, fill = "gray") + 
  geom_point(data = fight, aes(x = territory_value_index, y = vis_data$res$visregRes), 
             color = "orange", alpha = 0.6) +
  theme_ipsum() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size =1.5), # Adds axis lines 
    axis.title.x = element_text(hjust = 0.5, size = 11, family = "Calibri",
                                margin = margin(t = 14)),
    # Center and enlarge X-axis label
    axis.title.y = element_text(hjust = 0.5, size = 11, family = "Calibri", margin = margin(r = 14))  # Center and enlarge Y-axis label and add margin
  )  +
  labs(x = "Territory quality index", y = "Contest duration (s)") +
  annotate("text", x = -Inf, y = Inf, label = "c)", hjust = -0.6, vjust = 1.5, size = 4, fontface = "bold") 
dev.off()


####Calculating the part R2####

#packages used

library(rr2)
#####Winner partial R2########

model_2=gls(overall_contest_duration~scaled_loser_total_fat_weight+territory_value_index_daily, data=fight)

R2_lik(model_1,model_2) #0.48
0.95-0.60
#0.35

#####Loser partial R2########

model_3=gls(overall_contest_duration~scaled_winner_total_fat_weight+territory_value_index, weights=varPower(form=~scaled_winner_total_fat_weight), data=fight)

R2_lik(model_1,model_3) #0.59
0.95-0.59
#0.36

#####Territory index partial R2########

model_4=gls(overall_contest_duration~scaled_winner_total_fat_weight, weights=varPower(form=~scaled_winner_total_fat_weight), data=fight)

R2_lik(model_1,model_4) #0.79
0.95-0.79
#0.16

R2_lik(model_1)



###Supplementary Material Raw data figures####
######Figure S2a- Winner ############

library(ggplot2)
library(hrbrthemes)

jpeg('Figure S2a.jpg',  w=1800 , h=1200,res=300)
par(mar=c(5,4,2,2))
ggplot(fight, aes(x=scaled_winner_total_fat_weight, y=overall_contest_duration)) + 
  geom_point(color="orange", size = 3) +
  theme_ipsum() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black",  size =1.5), # Adds axis lines 
    axis.title.x = element_text(hjust = 0.5, size = 11, family = "Calibri",
                                margin = margin(t = 14)),
    # Center and enlarge X-axis label
    axis.title.y = element_text(hjust = 0.5, size = 11, family = "Calibri", margin = margin(r = 14))  # Center and enlarge Y-axis label and add margin
  ) +
  labs(y = "Contest duration (s)", x = "Scaled winner total fat mass") + 
  scale_y_continuous(breaks = seq(50, max(fight$overall_contest_duration), by = 500)) + # Adjust the 'by' value as needed
  annotate("text", x = -Inf, y = Inf, label = "a)", hjust = -0.6, vjust = 1.5, size = 4, fontface = "bold")
dev.off()

######Figure S2b- Loser ############
jpeg('Figure_S2b.jpg', width = 1800, height = 1200, res = 300)
par(mar=c(5,4,2,2))
ggplot(fight, aes(x=scaled_loser_total_fat_weight, y=overall_contest_duration)) + 
  geom_point(color="orange", size = 3) +
  theme_ipsum() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size =1.5), # Adds axis lines 
    axis.title.x = element_text(hjust = 0.5, size = 11, family = "Calibri",
                                margin = margin(t = 14)),
    # Center and enlarge X-axis label
    axis.title.y = element_text(hjust = 0.5, size = 11, family = "Calibri", margin = margin(r = 14))  # Center and enlarge Y-axis label and add margin
  ) +
  labs(y = "Contest duration (s)", x = "Scaled loser total fat mass") + 
  scale_y_continuous(breaks = seq(50, max(fight$overall_contest_duration), by = 500)) + # Adjust the 'by' value as needed
  annotate("text", x = -Inf, y = Inf, label = "b)", hjust = -0.6, vjust = 1.5, size = 4, fontface = "bold") 
dev.off()

######Figure S2c- Territory############

jpeg('Figure S2c.jpg',  w=1800 , h=1200,res=300)
par(mar=c(5,4,2,2))
ggplot(fight, aes(x=territory_value_index, y=overall_contest_duration)) + 
  geom_point(color="orange", size = 3) +
  theme_ipsum() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size =1.5), # Adds axis lines 
    axis.title.x = element_text(hjust = 0.5, size = 11, family = "Calibri",
                                margin = margin(t = 14)),
    # Center and enlarge X-axis label
    axis.title.y = element_text(hjust = 0.5, size = 11, family = "Calibri", margin = margin(r = 14))  # Center and enlarge Y-axis label and add margin
  ) +
  labs(y = "Contest duration (s)", x = "Territory quality index") + 
  scale_y_continuous(breaks = seq(50, max(fight$overall_contest_duration), by = 500)) + # Adjust the 'by' value as needed
  annotate("text", x = -Inf, y = Inf, label = "c)", hjust = -0.6, vjust = 1.5, size = 4, fontface = "bold") 
dev.off()










