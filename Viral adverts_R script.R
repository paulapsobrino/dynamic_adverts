##23rd September 2017
##Paula P Sobrino: Script for study on dynamic adverts & figurative operations

##Let' set up the working directory
setwd("~/Desktop/dynamic adverts_Nvivo")
dynamic=read.csv("Viral adverts_dataset.csv")

##Let's explore the data
summary(dynamic)
head (dynamic)
tail(dynamic)

##Everything looks good
#load packages
library(MASS) 
library(MuMIn)
library (lme4)

#check for collinearity with variance inflation factor 
library(car)
vif(mymodel_figops) # look for values much larger than 4

#RQ 1. TOTAL FIGURATIVENESS

# Figurative operations and Views per day
mymodel_figops = glmer(Views_per_day~FigOps_total + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_figops)
r.squaredGLMM(mymodel_figops)

#check positive/negative correlation
x_figops = data.frame(dynamic$ FigOps_total, dynamic$Views)
cor_figops = cor(x_figops,y=NULL, use = "everything", method = "pearson")
cor_figops
cor.test(dynamic$ FigOps_total, dynamic$Views)

#_____________________________________________________________

#RQ 2. TYPES OF FIGURATIVE OPERATIONS – Likelihood test ratio
# Figurative operations and Views per day

#Metonymy

mymodel_mety = glmer(Views_per_day~Metonymy + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_mety)

mymodel_mety = glmer(Views~Metonymy + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_mety)

#Correlational Metaphor

mymodel_metcor = glmer(Views_per_day~ MetaphorCorrelational + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_metcor)

mymodel_metcor = glmer(Views~MetaphorCorrelational + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_metcor)

#Resemblance Metaphor

mymodel_metres = glmer(Views_per_day~ MetaphorResemblance + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_metres)

mymodel_metres = glmer(Views~ MetaphorResemblance + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_metres)

#Metaphor and Metonymy

mymodel_metmety = glmer(Views_per_day~ Metonymy + MetaphorCorrelational + MetaphorResemblance+(1|Advert), data = dynamic, family = "poisson")
summary(mymodel_metmety)
mymodel_metmety.null = glmer(Views_per_day~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_metmety.null, mymodel_metmety, test = "Chisq")

mymodel_metmety = glmer(Views~ Metonymy + MetaphorCorrelational + MetaphorResemblance+(1|Advert), data = dynamic, family = "poisson")
summary(mymodel_metmety)
mymodel_metmety.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_metmety.null, mymodel_metmety, test = "Chisq")

#Dramatic irony

mymodel_idram = glmer(Views_per_day ~ IronyDramatic + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_idram)

mymodel_idram = glmer(Views ~ IronyDramatic + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_idram)
mymodel_idram.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_idram.null, mymodel_idram, test = "Chisq") 
r.squaredGLMM(mymodel_idram)

#check positive/negative correlation
x_idram = data.frame(dynamic$ IronyDramatic, dynamic$Views)
cor_idram = cor(x_idram,y=NULL, use = "everything", method = "pearson")
cor_idram
cor.test(dynamic$ IronyDramatic, dynamic$Views)

#Situational irony

mymodel_isit = glmer(Views_per_day ~ IronySituational + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_isit)

mymodel_isit = glmer(Views ~ IronySituational + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_isit)
mymodel_isit.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_isit.null, mymodel_isit, test = "Chisq") 
#Verbal irony

mymodel_iverb = glmer(Views_per_day ~ IronyVerbal + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_iverb)

mymodel_iverb = glmer(Views ~ IronyVerbal + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_iverb)

#All irony

mymodel_i = glmer(Views_per_day ~ IronyDramatic + IronySituational + IronyVerbal + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_i)

mymodel_i.null = glmer(Views_per_day~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_i.null, mymodel_i, test = "Chisq") 

mymodel_i = glmer(Views ~ IronyDramatic + IronySituational + IronyVerbal + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_i)

mymodel_i.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_i.null, mymodel_i, test = "Chisq") 

r.squaredGLMM(mymodel_i)

#Unmarked contrast

mymodel_uc = glmer(Views_per_day ~ UnmarkedContrast + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_uc)

#all contrast

mymodel_ct = glmer(Views_per_day ~ IronyDramatic + IronySituational + IronyVerbal + UnmarkedContrast + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_ct)
mymodel_ct.null = glmer(Views_per_day~1+(1|Advert), data = dynamic, family = "poisson")
anova(mymodel_ct.null, mymodel_ct, test = "Chisq")

mymodel_ct = glmer(Views ~ IronyDramatic + IronySituational + IronyVerbal + UnmarkedContrast + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_ct)
mymodel_ct.null = glmer(Views~1+(1|Advert), data = dynamic, family = "poisson")
anova(mymodel_ct.null, mymodel_ct, test = "Chisq")

r.squaredGLMM(mymodel_ct)

#Hyperbole

mymodel_hyper = glmer(Views_per_day ~ Hyperbole + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_hyper)

mymodel_hyper = glmer(Views ~ Hyperbole + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_hyper)

#Understatement

mymodel_under = glmer(Views_per_day ~ Understatement + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_under)

mymodel_under = glmer(Views ~ Understatement + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_under)

#all intensifiers

mymodel_inter = glmer(Views_per_day ~ Hyperbole + Understatement + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_inter)
mymodel_inter.null = glmer(Views_per_day~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_inter.null, mymodel_inter, test = "Chisq")

mymodel_inter = glmer(Views ~ Hyperbole + Understatement + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_inter)
mymodel_inter.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_inter.null, mymodel_inter, test = "Chisq")

#lets check combinations

# irony and metaphor&metonymy
mymodel_iromet = glmer(Views_per_day ~ Metonymy + MetaphorCorrelational + MetaphorResemblance + IronyDramatic + IronySituational + IronyVerbal + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_iromet)
mymodel_iromet.null = glmer(Views_per_day~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_iromet.null, mymodel_iromet, test = "Chisq")

mymodel_iromet = glmer(Views ~ Metonymy + MetaphorCorrelational + MetaphorResemblance + IronyDramatic + IronySituational + IronyVerbal + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_iromet)
mymodel_iromet.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_iromet.null, mymodel_iromet, test = "Chisq")

# all contrast and metaphor&metonymy
mymodel_conmet = glmer(Views_per_day ~ Metonymy + MetaphorCorrelational + MetaphorResemblance + IronyDramatic + IronySituational + IronyVerbal + UnmarkedContrast + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_conmet)
mymodel_conmet.null = glmer(Views_per_day~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_conmet.null, mymodel_conmet, test = "Chisq")

mymodel_conmet = glmer(Views ~ Metonymy + MetaphorCorrelational + MetaphorResemblance + IronyDramatic + IronySituational + IronyVerbal + UnmarkedContrast + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_conmet)
mymodel_conmet.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_conmet.null, mymodel_conmet, test = "Chisq")

# all contrast and all intensifiers
mymodel_conint = glmer(Views_per_day ~ IronyDramatic + IronySituational + IronyVerbal + UnmarkedContrast + Hyperbole + Understatement + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_conmet)
mymodel_conint.null = glmer(Views_per_day~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_conint.null, mymodel_conint, test = "Chisq")

mymodel_conint = glmer(Views ~ IronyDramatic + IronySituational + IronyVerbal + UnmarkedContrast + Hyperbole + Understatement + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_conint)
mymodel_conint.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_conint.null, mymodel_conint, test = "Chisq")

r.squaredGLMM(mymodel_conint)

# metaphor, metonymy and intensifiers
mymodel_metyint = glmer(Views_per_day ~ Metonymy + MetaphorCorrelational + MetaphorResemblance + Hyperbole + Understatement + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_metyint)
mymodel_metyint.null = glmer(Views_per_day~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_metyint.null, mymodel_metyint, test = "Chisq")

mymodel_metyint = glmer(Views ~ Metonymy + MetaphorCorrelational + MetaphorResemblance + Hyperbole + Understatement + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_metyint)
mymodel_metyint.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_metyint.null, mymodel_metyint, test = "Chisq")


##_________________________________________________________
##RQ3. MODES 

#Images 
mymodel_images = glmer(Views_per_day ~ Images + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_images)

mymodel_images = glmer(Views ~ Images + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_images)

#Words

mymodel_words = glmer(Views_per_day ~ Words + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_words)

mymodel_words = glmer(Views ~ Words + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_words)

#Music

mymodel_music = glmer(Views_per_day ~ Music + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_music)

mymodel_music = glmer(Views ~ Music + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_music)


#all three monomodal formats
mymodel_monomodal = glmer(Views ~ Words + Images + Music + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_monomodal)
mymodel_monomodal.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_monomodal.null, mymodel_monomodal, test = "Chisq")

#Images and words

mymodel_imawords = glmer(Views_per_day ~ Images_words + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_imawords)
mymodel_imawords.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_imawords.null, mymodel_imawords, test = "Chisq")
r.squaredGLMM(mymodel_imawords)

#Images and music
mymodel_imamus = glmer(Views_per_day ~ Images_music + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_imamus)

mymodel_imamus = glmer(Views ~ Images_music + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_imamus)

#all three hybrid
mymodel_hybrid = glmer(Views ~ Words + Images_words + Images_music + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_hybrid)
mymodel_hybrid.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_hybrid.null, mymodel_hybrid, test = "Chisq")

##_________________________________________________________
##RQ4. POSITIONING 

#segment 1

mymodel_segment1 = glmer(Views_per_day ~ Segment1_FigUnits + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_segment1)

mymodel_segment1 = glmer(Views ~ Segment1_FigUnits + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_segment1)

r.squaredGLMM(mymodel_segment1)

#check positive/negative correlation
x_Segment1_FigUnits = data.frame(dynamic$ Segment1_FigUnits, dynamic$Views)
cor_Segment1_FigUnits = cor(x_Segment1_FigUnits,y=NULL, use = "everything", method = "pearson")
cor_Segment1_FigUnits

#segment 2
mymodel_segment2 = glmer(Views_per_day ~ Segment2_FigUnits + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_segment2)

mymodel_segment2 = glmer(Views ~ Segment2_FigUnits + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_segment2)

#segment 3

mymodel_segment3 = glmer(Views_per_day ~ Segment3_FigUnits + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_segment3)

mymodel_segment3 = glmer(Views ~ Segment3_FigUnits + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_segment3)

#segment 4
mymodel_segment4 = glmer(Views_per_day ~ Segment4_FigUnits + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_segment4)

mymodel_segment4 = glmer(Views ~ Segment4_FigUnits + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_segment4)

#segment 1 and 4
mymodel_segment14 = glmer(Views_per_day ~ Segment1_FigUnits + Segment4_FigUnits + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_segment14)
mymodel_segment14.null = glmer(Views_per_day~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_segment14.null, mymodel_segment14, test = "Chisq")

mymodel_segment14 = glmer(Views ~ Segment1_FigUnits + Segment4_FigUnits + (1|Advert), data = dynamic, family = "poisson")
summary(mymodel_segment14)
mymodel_segment14.null = glmer(Views~1 + (1|Advert), data = dynamic, family = "poisson")
anova(mymodel_segment14.null, mymodel_segment14, test = "Chisq")



