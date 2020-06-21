data<-read.csv("COVID19_1.csv",header=TRUE)

for(unique_value in unique(data$Education.Attainment))
{
  data[paste("Education.Attainment",unique_value,sep="::")]<-ifelse(data$Education.Attainment==unique_value,1,0)
}



for(unique_value in unique(data$Occupation))
{
  data[paste("Occupation",unique_value,sep="::")]<-ifelse(data$Occupation==unique_value,1,0)
}


for(unique_value in unique(data$Religion))
{
  data[paste("Religion",unique_value,sep="::")]<-ifelse(data$Religion==unique_value,1,0)
}





data<-data[,-c(3,4,5,6)]
data[1,]

datacor<-cor(data[,-5])
corrplot(datacor)



data<-data[,-c(9,10,16,17)]

Awareness<-as.factor(data$Awareness)

contrasts(Awareness)

###############################LOGISTIC###########################################



model <- glm(Awareness~.-1,data=data,family=binomial(link='logit'))
summary(model)

1-pchisq(441.05,521)

anova(model, test="Chisq")
pR2(model)



broom::tidy(model,exponentiate=TRUE,conf.level=.95)#odds
exp(model$coefficients)#odds






model <- glm(Awareness~.,data=data,family=binomial(link='logit'))
vif_values<-car::vif(model)
