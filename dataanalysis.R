setwd('D:/OneDrive/Modules/Y4S1 (Current)/SP3203 Aquatic Ecology Research/My Manuscript')
data <- read.csv('data.csv', header = T)

#Check data is loaded correctly
data
View(data)
summary(data)

#check species is categorical data
is.factor(data$species)

#exploratory plots
library(ggplot2)
myPlot <- ggplot(data, aes(width, mp_ingested_adjusted, colour = species)) +
  geom_smooth(method="lm", size = 1.2, se = FALSE) +
  geom_point(size=3) +
  theme_bw() + 
  xlab("Length (mm)") +
  ylab("Microbead ingested (mg)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(face = "italic")) + 
  scale_y_continuous(limits=c(0, 60))+
  scale_color_discrete(name ="Species:",  labels=expression(italic('M. meretrix'),italic('M. lusoria'))) +
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic"))) +
  theme_classic() 
myPlot

ggsave(myPlot, filename = "regressionPlot.png", dpi = 300, type = "cairo",
       width = 6, height =  = 4, units = "in")


#linear regression model
model1 <- lm(mp_ingested_adjusted ~ height * width * weight * species, data = data)
summary(model1)
par(mfrow = c(2,2))
plot(model1)
#Heteroscedasticity detected in residuals against fitted plot. 

#Breusch-Pagan test for heteroscedasticity
library(lmtest)
bptest(model1)
#no heteroscedasticity present

#identifying outliers
cooksd <- cooks.distance(model1)
sample_size <- nrow(data)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

#All identified points are outliers due to the relatively higher amount of microplastic ingested
#may be biologically relevant, so we cannot remove them.

#checking for multicollinearity
library(car)
vif(model1)
#collinearity observed, most likely from height and width.
#The mussels would be considered very large if length was used, even if they have low mass.
#width, is then a reasonable choice because it scales with bivalve size well across the three species.
model2 <- lm(mp_ingested_adjusted ~ width * weight * species, data = data)
vif(model2)
#collinearity issue persist
#Weight is a proxy to measure the mass, hence size of the bivalves. But wet weight is confounded by water content.
#Thus, it is not an idea measure of bivalve size.
model3 <- lm(mp_ingested_adjusted ~ width * species, data = data)
vif(model3)
#although score for all variables are above 3, they are already the minimum number of variables I can include in the model
#therefore, not dropping any

#listing down all possible models for AIC
model4 <- lm(mp_ingested_adjusted ~ width * species, data = data)
model5 <- lm(mp_ingested_adjusted ~ width + species, data = data)
model6 <- lm(mp_ingested_adjusted ~ width, data = data)
model7 <- lm(mp_ingested_adjusted ~ species, data = data)
model8 <- lm(mp_ingested_adjusted ~ width:species, data = data)

#since sample size is large (n = 48), use AIC instead of AICc
library(MuMIn)
model_sel <- model.sel(model4, model5, model6, model7, model8, rank = 'AIC')
model_sel

#model7 has the lowest AIC but has almost identical AIC values as model4 (0.1 difference)
#but model4 models width and its interaction with species, thus is a comprehensive model
#so model4 is my chosen model.

#analyzing the model
View(data)
model4 <- lm(mp_ingested_adjusted ~ width*species, data = data)
library(car)
Anova(model4)

#visualizing the model
install.packages('ggeffects')
install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")
require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(ggeffects)
library(ggplot2)

ggPredict(model4,colorAsFactor = TRUE,interactive=TRUE, se = TRUE)

dat <- ggpredict(model4, terms = c("width", "species"))
plot(dat, rawdata = TRUE) + labs(x = 'Shell length (mm)', y = 'Microbead ingested (mg)', title=NULL) +
  scale_color_discrete(name ="Species:", labels=expression(italic('M. meretrix'),italic('M. lusoria'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(face = "italic")) 

ggsave(dat, filename = "multipleregressionPlot.png", dpi = 300, type = "cairo",
       width = 6, height = 4, units = "in")

#diagnostic plots
par(mfrow=c(2,2))
plot(model4)

#Attempting to resolve non-normality issues with model
library(nlme)
#listing variance structures
vf1 <- varFixed(~width)
vf2 <- varIdent(form= ~ 1 | species)
vf3 <- varPower(form =~ width)
vf4 <- varPower(form =~ width |species)
vf5 <- varExp(form =~ width)
vf6 <- varConstPower(form =~width)
vf7 <- varConstPower(form=~ width | species)
vf8 <- varComb(varIdent(form =~ 1 | species))

#listing models
gls_model1 <- gls(mp_ingested_adjusted ~ width:species,
                  data = data,weights = vf1)
gls_model2 <- gls(mp_ingested_adjusted ~ width:species,
                  data = data,weights = vf2)
gls_model3 <- gls(mp_ingested_adjusted ~ width:species,
                  data = data,weights = vf3)
gls_model4 <- gls(mp_ingested_adjusted ~ width:species,
                  data = data,weights = vf4)
gls_model5 <- gls(mp_ingested_adjusted ~ width:species,
                  data = data,weights = vf5)
gls_model6 <- gls(mp_ingested_adjusted ~ width:species,
                  data = data,weights = vf6)
gls_model7 <- gls(mp_ingested_adjusted ~ width:species,
                  data = data,weights = vf7)
gls_model8 <- gls(mp_ingested_adjusted ~ width:species,
                  data = data,weights = vf8)

qqnorm(gls_model1)
qqnorm(gls_model2)
qqnorm(gls_model3)
qqnorm(gls_model4)
qqnorm(gls_model5)
qqnorm(gls_model6)
qqnorm(gls_model7)
qqnorm(gls_model8)
#all non-normal