library(ISLR)
library(ggplot2)
library(dplyr)
library(FSA)
library(ARTool)
library(emmeans)

DF <- Wage

DF <- DF %>% 
  mutate(period = ifelse(year <= 2006, "2003-2006", "2007-2009"))  

DF$period<- as.factor(DF$period)

ggplot(DF, aes(x=education, y=wage, colour = period))+
  geom_boxplot()+
  theme_bw()

DF<- DF %>% 
  filter(education %in% c("3. Some College", "4. College Grad",   "5. Advanced Degree"))

DF$education<- as.character(DF$education)
DF$year<- as.factor(DF$year)
DF$education <- as.factor(DF$education)

ggplot(DF, aes(x=education, y=wage, colour = period))+
  geom_boxplot()+
  facet_wrap(~jobclass)+
  theme_bw()

df<- DF %>% 
  filter(year%in% c(2003, 2004, 2005,2006), education %in% c("3. Some College", "4. College Grad",  "5. Advanced Degree"))

summary_2003__2006<- df %>% 
  group_by(education, jobclass) %>% 
  summarise(N= n(),
            MiN = min(wage),
            Q_1 =quantile(wage, probs = 0.25),
            Mean = mean(wage),
            Median = median(wage),
            Q_3 =quantile(wage, probs = 0.75),
            sd = sd(wage),
            Max = max(wage),
            .groups = "drop")   


Industrial<- df %>% 
  filter(jobclass == "1. Industrial")

kruskal.test(wage~education, data= Industrial)

dunnTest(wage~education, data= Industrial)


Information<- df %>% 
  filter(jobclass == "2. Information")

kruskal.test(wage~education, data= Information)

dunnTest(wage~education, data= Information, method = "bonferroni")

ggplot(df, aes(x=education, y=wage, colour = jobclass))+
  geom_boxplot()+
  theme_bw()

Advanced_Degree<- df %>% 
  filter(education == "5. Advanced Degree")

wilcox.test(wage~jobclass, data =Advanced_Degree)

ggplot(Advanced_Degree, aes(x=jobclass, y=wage))+
  geom_boxplot()+
  theme_bw()

model<- art(wage~education+jobclass+  education:jobclass, data = df)
anova_result<- anova(model)

print(anova_result)

plot_data <- df %>%
  group_by(education, jobclass) %>%
  summarise(mean_wage = mean(wage, na.rm = TRUE), .groups = 'drop')

ggplot(plot_data, aes(x = education, y = mean_wage, color = jobclass, group = jobclass)) +
  geom_line(linewidth = 1) +        
  geom_point(size = 3) +              
  labs(title = "Interaction Plot: Education × Jobclass",
       x = "Education Level",
       y = "Mean Wage",
       color = "Jobclass") +
  theme_minimal() +                   
  theme(legend.position = "right")



df1 <- DF %>% 
  filter(year%in% c(2007, 2008, 2009), education %in% c("3. Some College", "4. College Grad",  "5. Advanced Degree"))

summary_2007_2008_2009<- df1 %>% 
  group_by(education, jobclass) %>% 
  summarise(N= n(),
            MiN = min(wage),
            Q_1 =quantile(wage, probs = 0.25),
            Mean = mean(wage),
            Median = median(wage),
            Q_3 =quantile(wage, probs = 0.75),
            sd = sd(wage),
            Max = max(wage),
            .groups = "drop")   


ggplot(df1, aes(x=education, y=wage, colour = jobclass))+
  geom_boxplot()+
  theme_bw()

Industrial<- df1 %>% 
  filter(jobclass == "1. Industrial")

kruskal.test(wage~education, data= Industrial)

dunnTest(wage~education, data= Industrial,  method = "bonferroni" )


Information<- df1 %>% 
  filter(jobclass == "2. Information")

kruskal.test(wage~education, data= Information)

dunnTest(wage~education, data= Information, method = "bonferroni")


Advanced_Degree<- df1 %>% 
  filter(education == "5. Advanced Degree")

wilcox.test(wage~jobclass, data =Advanced_Degree)

ggplot(Advanced_Degree, aes(x=jobclass, y=wage, colour = jobclass))+
  geom_boxplot()+
  theme_bw()


model<- art(wage~education+jobclass+ education:jobclass, data = df1)
anova_result<- anova(model)

print(anova_result)

plot_data <- df1 %>%
  group_by(education, jobclass) %>%
  summarise(mean_wage = mean(wage, na.rm = TRUE), .groups = 'drop')

ggplot(plot_data, aes(x = education, y = mean_wage, color = jobclass, group = jobclass)) +
  geom_line(linewidth = 1) +        
  geom_point(size = 3) +              
  labs(title = "Interaction Plot: Education × Jobclass",
       x = "Education Level",
       y = "Mean Wage",
       color = "Jobclass") +
  theme_minimal() +                   
  theme(legend.position = "right")