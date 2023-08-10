library("ggplot2"); library("lme4"); 
library("dplyr"); 
library("lmerTest"); library("tidyr");
library("Rcpp"); library("sjstats"); library("pwr");
library("MuMIn")

setwd("C:/Users/bdecouto/SynologyDrive/Dissertation/Experiment 2 Volleyball/Analysis/Performance Analysis")
list.files()
rm(list = ls())

Performance <- read.csv("./Performance_Analysis.csv", header = TRUE, sep=",",  
                na.strings=c("NA","NaN"," ",""))

#P19 performed below 50% in Navon task. doing it wrong?



  
#Visualize Results
levels(Performance$Occ)
Performance$Skill <- as.factor(Performance$Skill)
Performance$Occ <- as.factor(Performance$Occ)

levels(Performance$Skill)[levels(Performance$Skill)=="Expert"] <- "Skilled"
levels(Performance$Skill)[levels(Performance$Skill)=="Novice"] <- "Less-Skilled"
levels(Performance$Occ)[levels(Performance$Occ)=="Early Occlusion"] <- "280ms"
levels(Performance$Occ)[levels(Performance$Occ)=="Late Occlusion"] <- "140ms"


Perf_Graph <- Performance %>%
  group_by(Condition, Occ, Skill) %>%
  summarise(Total_Err = (sd(Total)/sqrt(n())),
            Total = (sum(Total)/n()))
Perf_Graph$Skill <- factor(Perf_Graph$Skill, levels=c("Less-Skilled", "Skilled"))

write.csv(Perf_Graph, "Perf_Graph.csv")

dodge <- position_dodge(width = 0.5)


ggplot(filter(Perf_Graph, !(Condition == "Control")), aes(x = Condition, y = Total, fill = Skill))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin=Total-Total_Err, ymax=Total+Total_Err), position = position_dodge(.9), width=.2)+
  facet_wrap(~Occ, nrow = 1)+
  coord_cartesian(ylim = c(0, .67))+
  scale_x_discrete(name = "Milliseconds before ball contact")+
  scale_y_continuous(name = "Performance (% Correct)", breaks = c(0, .33, .66, .99),
                     labels = scales::percent_format(accuracy = 1))+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 12))+
  theme(legend.spacing.x = unit(.5,'cm'))+
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 5)))+
  theme(axis.text=element_text(size=15, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 15))+
  #scale_fill_grey(start = 0, end = .9)+
  scale_fill_manual(name="", values = c("Skilled" = "grey60",
                                         "Less-Skilled" = "grey20"))

ggsave("Occ.pdf", width = 8, height = 6)



ggplot(filter(Perf_Graph, !(Condition == "Control")), aes(x = Condition, y = Total))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin=Total-Total_Err, ymax=Total+Total_Err), position = position_dodge(.9), width=.2)+
  facet_wrap(~Occ, nrow = 1)+
  coord_cartesian(ylim = c(0, .67))+
  scale_x_discrete(name = "Milliseconds before ball contact")+
  scale_y_continuous(name = "Performance (% Correct)", breaks = c(0, .33, .66, .99),
                     labels = scales::percent_format(accuracy = 1))+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 12))+
  theme(legend.spacing.x = unit(.5,'cm'))+
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 5)))+
  theme(axis.text=element_text(size=15, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 15))
  #scale_fill_grey(start = 0, end = .9)


ggsave("Occ.pdf", width = 8, height = 6)









Performance$Occ <- as.factor(Performance$Occ)
GL <- filter(Performance, !(Condition == "Control"))
GC <- filter(Performance, !(Condition == "Local Prime"))
LC <- filter(Performance, !(Condition == "Global Prime"))
xtabs(~Condition, Performance)






#Analysis
chart <- Performance %>%
  group_by(subID) %>%
  summarise(Perf = mean(Total))
hist(log10(Performance$Total),probability=T)
shapiro.test((Performance$Total))


hist(chart$Perf,probability=T)
shapiro.test(chart$Perf)


Performance$Condition <- relevel(factor(Performance$Condition), ref = "Local Prime")

mod1 <- (lmer(log10(Total) ~ Condition * Skill  * Occ +
                (1|subID) + (1|Condition:subID) + (1|Occ:subID), REML = FALSE, 
              data = (filter(Performance, !(Condition == "Control")))))
res <- resid(model)

qqnorm(res)

summary((mod1))
anova_stats(mod1)

emmeans(mod1, pairwise~Condition , adjust="none")
emmeans(mod1, pairwise~Skill , adjust="none")
emmeans(mod1, pairwise~Occ , adjust="none")

p <- c(.0467, .0003, .00001)
p.adjust(p, method = "BH")


p <- c(.256, .0962, .0399, .0001, .0015, .0050)

p <- c(.1672, .4604, .5082, .5181, .6140, .2552, .3319, .0271, .2096, .0005, .0039, .4914)

p <- c(.00001, .00001,.00001, .0949, .0001, .00001)
p.adjust(p, method = "BH")


.134   /(0.0163  *sqrt(64))


anova_stats(mod1)
r.squaredGLMM(mod1)

Performance <- mutate(Performance, Total = log10(Total))
#investigate the effects of condition
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "280ms" & 
                                  !(Condition == "Control") & 
                                  Skill == "Less-Skilled")))
    summary(mod1)
    confint(mod1)
    
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "280ms" & 
                                  !(Condition == "Global Prime") & 
                                  Skill == "Less-Skilled")))
    summary(mod1)
    confint(mod1)
    
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "280ms" & 
                                  !(Condition == "Local Prime") & 
                                  Skill == "Less-Skilled")))
    summary(mod1)
    
    confint(mod1)
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "280ms" & 
                                  !(Condition == "Control") & 
                                  Skill == "Skilled")))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "280ms" & 
                                  !(Condition == "Global Prime") & 
                                  Skill == "Skilled")))
    summary(mod1)
    confint(mod1)
    
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "280ms" & 
                                  !(Condition == "Local Prime") & 
                                  Skill == "Skilled")))
    summary(mod1)
    confint(mod1)
    
    
#140 ms condition effects
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "140ms" & 
                                  !(Condition == "Control") & 
                                  Skill == "Less-Skilled")))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "140ms" & 
                                  !(Condition == "Global Prime") & 
                                  Skill == "Less-Skilled")))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "140ms" & 
                                  !(Condition == "Local Prime") & 
                                  Skill == "Less-Skilled")))
    summary(mod1)
    confint(mod1)
    
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "140ms" & 
                                  !(Condition == "Control") & 
                                  Skill == "Skilled")))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "140ms" & 
                                  !(Condition == "Global Prime") & 
                                  Skill == "Skilled")))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lmer(Total ~ Condition +
                    (1|subID), REML = FALSE, 
                  data = filter(Performance, Occ == "140ms" & 
                                  !(Condition == "Local Prime") & 
                                  Skill == "Skilled")))
    summary(mod1)
    confint(mod1)
    
    
    
    #Investigate Skill effect
    mod1 <- (lm(Total ~ Skill, data = filter(Performance, Occ == "280ms" & 
                                               (Condition == "Control"))))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lm(Total ~ Skill, data = filter(Performance, Occ == "280ms" & 
                                               (Condition == "Global Prime"))))
    summary(mod1)
    confint(mod1)
    
    
    mod1 <- (lm(Total ~ Skill, data = filter(Performance, Occ == "280ms" & 
                                  (Condition == "Local Prime"))))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lm(Total ~ Skill, data = filter(Performance, Occ == "140ms" & 
                                               (Condition == "Control"))))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lm(Total ~ Skill, data = filter(Performance, Occ == "140ms" & 
                                               (Condition == "Global Prime"))))
    summary(mod1)
    confint(mod1)
    
    
    
    mod1 <- (lm(Total ~ Skill, data = filter(Performance, Occ == "140ms" & 
                                               (Condition == "Local Prime"))))
    summary(mod1)
    confint(mod1)
    
    
    
    
    #Investigate occlusion effects
    
    mod1 <- (lmer(Total ~ Occ + (1|subID), REML = FALSE, data = filter(Performance, Skill == "Less-Skilled" & 
                                               (Condition == "Control"))))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lmer(Total ~ Occ+ (1|subID), REML = FALSE, data = filter(Performance, Skill == "Less-Skilled" & 
                                             (Condition == "Global Prime"))))
    summary(mod1)
    confint(mod1)
    
    
    mod1 <- (lmer(Total ~ Occ+ (1|subID), REML = FALSE, data = filter(Performance, Skill == "Less-Skilled" & 
                                             (Condition == "Local Prime"))))
    summary(mod1)
    confint(mod1)
    
    mod1 <- (lmer(Total ~ Occ+ (1|subID), REML = FALSE, data = filter(Performance, Skill == "Skilled" & 
                                             (Condition == "Control"))))
    summary(mod1)
    confint(mod1)
    
    
    mod1 <- (lmer(Total ~ Occ+ (1|subID), REML = FALSE, data = filter(Performance, Skill == "Skilled" & 
                                             (Condition == "Global Prime"))))
    summary(mod1)
    confint(mod1)
    
    
    
    mod1 <- (lmer(Total ~ Occ+ (1|subID), REML = FALSE, data = filter(Performance, Skill == "Skilled" & 
                                             (Condition == "Local Prime"))))
    summary(mod1)
    confint(mod1)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    p <- c(.323, .604, .627,.491, .414, .0921, .299, .0399, .0041,
           .138, .00985, .19, .226, .123, .0101, .000003, .00697,
           .00792, .0788, .00182, .00009, .000000001, .0000004, .000001)
    
    
    p.adjust(p, method = "BH")  
    
    
    
    
    
    
    
    
    
    
p <- c(.179, .00985, .114, .094,.442, .462, .004, .045, .263, .557, .218,
      .448, .000001, .266, .005, .147, .005, .008, .000000004,
      .047, .0000002,.002, .00001, .00007 )
p.adjust(p, method = "BH")  
    
  
      


      
#Test the effect of order ----null effect!
mod1 <- anova(lmer(Total ~ Skill * Condition  * Occ + Order.y +
                     (1|subID) + (1|Condition:subID) + (1|Occ:subID), REML = FALSE, 
                   data = (GL)))
(mod1)





Perf_Graph <- Performance %>%
  group_by(Condition) %>%
  summarise(Total_Err = (sd(Total)/sqrt(n())),
            Total = (sum(Total)/n()))
Perf_Graph$Skill <- factor(Perf_Graph$Condition, levels=c("Local", "Global"))


dodge <- position_dodge(width = 0.5)


p1 <-ggplot(filter(Perf_Graph, !(Condition == "Control")), aes(x = Condition, y = Total, fill = Condition))+
  geom_bar(stat = "identity", position = "dodge", width = .8)+
  geom_errorbar(aes(ymin=Total-Total_Err, ymax=Total+Total_Err), position = position_dodge(.9), width=.1)+
  coord_cartesian(ylim = c(0, .67))+
  scale_x_discrete(name = "Priming Condition")+
  scale_y_continuous(name = "Performance (% Correct)", breaks = c(0, .33, .66, .99),
                     labels = scales::percent_format(accuracy = 1))+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 12))+
  theme(legend.spacing.x = unit(.5,'cm'))+
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 5)))+
  theme(axis.text=element_text(size=15, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 15),
        legend.key = element_blank())+
  #scale_fill_grey(start = 0, end = .9)+
  scale_fill_manual(name="", values = c("Global Prime" = "grey80",
                                        "Local Prime" = "grey80"))


Perf_Graph <- Performance %>%
  filter(!(Condition == "Control")) %>%
  group_by(Skill) %>%
  summarise(Total_Err = (sd(Total)/sqrt(n())),
            Total = (sum(Total)/n()))
Perf_Graph$Skill <- factor(Perf_Graph$Skill, levels=c("Less-Skilled", "Skilled"))


dodge <- position_dodge(width = 0.5)


p2 <- ggplot(filter(Perf_Graph), aes(x = Skill, y = Total, fill = Skill))+
  geom_bar(stat = "identity", position = "dodge", width = .8)+
  geom_errorbar(aes(ymin=Total-Total_Err, ymax=Total+Total_Err), position = position_dodge(.9), width=.1)+
  coord_cartesian(ylim = c(0, .67))+
  scale_x_discrete(name = "Skill Group")+
  scale_y_continuous(name = "Performance (% Correct)", breaks = c(0, .33, .66, .99),
                     labels = scales::percent_format(accuracy = 1))+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 12))+
  theme(legend.spacing.x = unit(.5,'cm'))+
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)))+
  theme(axis.text=element_text(size=15, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 15))+
  #scale_fill_grey(start = 0, end = .9)+
  scale_fill_manual(name="", values = c("Less-Skilled" = "grey60",
                                        "Skilled" = "grey60"))



Perf_Graph <- Performance %>%
  filter(!(Condition == "Control")) %>%
  group_by(Occ) %>%
  summarise(Total_Err = (sd(Total)/sqrt(n())),
            Total = (sum(Total)/n()))
Perf_Graph$Skill <- factor(Perf_Graph$Skill, levels=c("Less-Skilled", "Skilled"))


dodge <- position_dodge(width = 0.5)


p3 <- ggplot(filter(Perf_Graph), aes(x = Occ, y = Total, fill = Occ))+
  geom_bar(stat = "identity", position = "dodge", width = .8)+
  geom_errorbar(aes(ymin=Total-Total_Err, ymax=Total+Total_Err), position = position_dodge(.9), width=.1)+
  coord_cartesian(ylim = c(0, .67))+
  scale_x_discrete(name = "Occlusion Interval")+
  scale_y_continuous(name = "Performance (% Correct)", breaks = c(0, .33, .66, .99),
                     labels = scales::percent_format(accuracy = 1))+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 12))+
  theme(legend.spacing.x = unit(.5,'cm'))+
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)))+
  theme(axis.text=element_text(size=15, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size = 15))+
  #scale_fill_grey(start = 0, end = .9)+
  scale_fill_manual(name="", values = c("280ms" = "grey40",
                                        "140ms" = "grey40"))
p3


library("cowplot")

plot_grid(p1, p2, p3, nrow = 1, align = "h")
ggsave("Perf.pdf", width = 12, height = 5)

