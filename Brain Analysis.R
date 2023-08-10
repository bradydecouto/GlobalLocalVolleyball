library("ggplot2"); library("lme4"); 
library("dplyr"); library("apaTables");
library("lmerTest"); library("tidyr");
library("Rcpp"); library("sjstats"); library("pwr");
library("MuMIn"); library("rstatix"); library("emmeans")
library("MBESS")

setwd("C:/Users/bdecouto/SynologyDrive/Dissertation/Experiment 2 Volleyball/Analysis/Analysis")
rm(list = ls())
list.files()

Alpha<-read.csv("./Alpha_Hem.csv", header = TRUE, sep=",",  
                na.strings=c("NA","NaN"," ",""))
Beta<-read.csv("./Beta_Hem.csv", header = TRUE, sep=",",  
                na.strings=c("NA","NaN"," ",""))
Rel<-read.csv("./Rel_Hem.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""))

#remove unusable data. P11 had bad EEG, and P19 did not follow the instructions on the global task
#already done in a previous code file
test <- Beta %>%
  group_by(Hemisphere, Skill, Task, Condition) %>%
  summarise(B_A = mean(P_Avg),
            Err = sd(P_Avg)/sqrt(n()))
Alpha <- filter(Alpha, Task == "V")
Beta <- filter(Beta, Task == "V")
Rel <- filter(Rel, Task == "V")



hist(Beta$P_Avg,probability=T)
shapiro.test(Beta$P_Avg)
#Visualize
ggplot(filter(test, Task == "N"))+
  geom_bar(aes(Condition, B_A, fill = Hemisphere), stat = "identity", position = "dodge")+
  geom_errorbar(aes(Condition, B_A, ymin=B_A-Err, ymax=B_A+Err), position = position_dodge(.5), width=.2)+
  facet_wrap(~Skill)+
  scale_y_continuous(name = "Beta - Alpha Activity")+
  coord_cartesian(ylim = c(-1, .2))+
  #coord_cartesian(ylim = c(0, .5))+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=12, colour="black"), 
        axis.title=element_text(size=13,face="bold"),
        strip.text = element_text(size = 15))

#Alpha  test

xtabs(~Skill, Alpha)
mod1 <- (lmer(P_Avg ~ Skill * Condition  * Hemisphere +
                     (1|subID) + (1|Condition:subID) + (1|Hemisphere:subID), REML = FALSE, 
                   data = filter(Alpha, !(Condition == "C"))))
summary(mod1)
anova_stats(mod1)

r.squaredGLMM(mod1)
get.ci.partial.eta.squared(1.478, 1, 31, .90)

c <- emmeans(mod1, pairwise~Skill|Hemisphere , adjust="none")
summary(c)

c <- emmeans(mod1, pairwise~Hemisphere|Skill , adjust="none")
summary(c)


p <- c(0.9598, 0.0301, .0001, 0.3398)
p.adjust(p, method = "BH")

.24120    /(0.107   *sqrt(31))

sqrt((((15+16)/(15*16))+(.337^2)/(2*(15+16-2)))*((15+16)/(15+16-2)))
.337-(0.37439 * 1.96)
.337+(0.37439 * 1.96)



    #Explore Skill Hemisphere interaction
    mod1 <- (lmer(P_Avg ~ Hemisphere * Condition +
                         (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
                       data = filter(Alpha, Skill == "E")))
    summary(mod1)
    
    confint(mod1)
    
    ci.smd(ncp=4.874, n.1=15, n.2=16, conf.level=0.90)
    
    mod1 <- (lmer(P_Avg ~ Hemisphere * Condition +
                    (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
                  data = filter(Alpha, Skill == "N")))
    summary(mod1)
    

    



#Beta Test


mod1 <- (lmer(P_Avg ~ Skill * Condition  * Hemisphere +
                     (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
                   data = filter(Beta, !(Condition == "C"))))
summary(mod1)

r.squaredGLMM(mod1)
get.ci.partial.eta.squared(2.494, 2, 61, .90)

emmeans(mod1, pairwise~Skill|Hemisphere , adjust="none")
emmeans(mod1, pairwise~Hemisphere|Skill , adjust="none")

p <- c(0.8790, 0.0268, .0001, 0.0200)
p.adjust(p, method = "BH")

.219   /(.089*sqrt(62))








Beta$Condition <- as.factor(Beta$Condition)
levels(Beta$Condition)
    #Explore Skill Hemisphere interaction
    mod1 <- (lmer(P_Avg ~ Skill * Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Beta, Hemisphere == "Right")))
    summary(mod1)
    
    mod1 <- (lmer(P_Avg ~ Hemisphere * Condition +
                    (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
                  data = filter(Beta, Skill == "N")))
    summary(mod1)
    confint(mod1)
    
    p <- c(.9643, 0.0001, .0158, .0391)
    p.adjust(p, method = "BH")
    
    
    
    0.39643/(0.07669 *sqrt(30))
    
    sqrt((((15+15)/(15*15))+(0.6269756^2)/(2*(15+15-2)))*((15+15)/(15+15-2)))
    0.6269756-(0.3710184 * 1.96)
    0.6269756+(0.3710184 * 1.96)
    
    

#Rel Test


mod1 <- (lmer(P_Avg ~ Skill * Condition  * Hemisphere +
                     (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
              data = filter(Rel, !(Condition == "C"))))
anova_stats(mod1)
summary(mod1)

emmeans(mod1, pairwise~Hemisphere , adjust="none")
emmeans(mod1, pairwise~Condition , adjust="none")

p <- c(.005, .0058, .0839, .2679)
p.adjust(p, method = "BH")

0.0898  /(0.0322  *sqrt(31))



r.squaredGLMM(mod1)
get.ci.partial.eta.squared(38.53, 1, 31, .90)

    mod1 <- (lmer(P_Avg ~ Hemisphere +
                    (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
                  data = Rel))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lmer(P_Avg ~ Condition  +
                    (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
                  data = filter(Rel, !(Condition == "L"))))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lmer(P_Avg ~ Condition  +
                    (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
                  data = filter(Rel, !(Condition == "G"))))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lmer(P_Avg ~ Condition  +
                    (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
                  data = filter(Rel, !(Condition == "C"))))
    summary(mod1)
    confint(mod1)



p <- c(0.0041 , .118    , 0.308    )
p.adjust(p, method = "BH")



    

    
    




Plot <- Alpha %>%
  group_by(subID, Skill, Hemisphere) %>%
  summarise(P_Avg = mean(P_Avg))
Plot$Skill <- as.factor(Plot$Skill)
levels(Plot$Skill) <- c('Skilled', 'Less-Skilled')

ggplot((Plot))+
  geom_boxplot(aes(Hemisphere, P_Avg, fill = Hemisphere), outlier.shape = NA)+
  geom_point(aes(Hemisphere, P_Avg), alpha = .5)+  
  geom_line(aes(Hemisphere, P_Avg, group = subID), alpha = .3, color = "black")+
  ylab(expression(Parietal~"Alpha"~Power~( µV^{"2"})))+ 
  facet_wrap(~Skill)+
  coord_cartesian(ylim = c(-1.3, .5))+
  scale_y_continuous(breaks = c(-1.2, -.8, -.4, -0, .4))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=20, colour="black"),
        strip.text = element_text(size = 18))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 22, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 22, face = "bold"))+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("Left" = "grey95",
                               "Right" = "grey60"))
ggsave("Alpha.pdf", width = 8, height = 8)



Plot <- Beta %>%
  group_by(subID, Skill, Hemisphere) %>%
  summarise(P_Avg = mean(P_Avg))
Plot$Skill <- as.factor(Plot$Skill)
levels(Plot$Skill) <- c('Skilled', 'Less-Skilled')

ggplot((Plot))+
  geom_boxplot(aes(Hemisphere, P_Avg, fill = Hemisphere), outlier.shape = NA)+
  geom_point(aes(Hemisphere, P_Avg), alpha = .5)+  
  geom_line(aes(Hemisphere, P_Avg, group = subID), alpha = .3, color = "black")+
  ylab(expression(Parietal~"Beta"~Power~(µV^{"2"})))+ 
  facet_wrap(~Skill)+
  coord_cartesian(ylim = c(-1.3, .6))+
  scale_y_continuous(breaks = c(-1.2, -.8, -.4, -0, .4))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=20, colour="black"),
        strip.text = element_text(size = 18))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 22, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 22, face = "bold"))+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("Left" = "grey95",
                               "Right" = "grey60"))


ggsave("Beta.pdf", width = 8, height = 8)






Condition_Plot <- Rel %>%
  group_by(Task, subID, Hemisphere) %>%
  summarise(B_A = mean(P_Avg),
            Err = sd(P_Avg)/sqrt(n()))

ggplot(filter(Condition_Plot, Task == "V"))+
  geom_boxplot(aes(Hemisphere, B_A, fill = Hemisphere), outlier.shape = NA)+
  geom_point(aes(Hemisphere, B_A), alpha = .5)+  
  geom_line(aes(Hemisphere, B_A, group = subID), alpha = .3, color = "black")+
  ylab(expression(Parietal~"Beta"~-~"Alpha"~Power~(?V^{"2"})))+ 
  #coord_cartesian(ylim = c(-1, .2))+
  #facet_wrap(~Hemisphere)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=20, colour="black"),
        strip.text = element_text(size = 18))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 22, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 22, face = "bold"))+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("Left" = "grey95",
                               "Right" = "grey60"))

