

source("https://raw.githubusercontent.com/GaryNapier/Packages_functions/master/PACKAGES.R")

# Set the theme
theme_set(theme_grey())

setwd("U:/Gilleen_data")

He_measures <- read.csv("Healthy_measures.csv")
Pat_measures <- read.csv("Pat_measures.csv")

N_he <- nrow(He_measures)
N_pat <- nrow(Pat_measures)


names(He_measures)
names(Pat_measures)

# Order by ID #
He_measures   <- He_measures[order(He_measures$ID),]
Pat_measures <- Pat_measures[order(Pat_measures$ID),]

#-----------------------------------------------------------------------------------------------------

# PDG variables are listed as PDG then the condition, ABC or D. 
# Ten trust ratings which have a 't' e.g. PDGBT10. 

# Separate PDG move and trust ratings
He_PDG_move <- dplyr::select(He_measures, PDGA1:PDGA10, -c(SPSpostA, SPSpreB, SPSpostB, SPSpreC, 
              SPSpostC, SPSpreD), -c(PDGAT1:PDGAT10), PDGB1:PDGB10, -c(PDGBT1:PDGBT10), PDGC1:PDGC10, 
              -c(PDGCT1:PDGCT10), PDGD1:PDGD10, -c(PDGDT1:PDGDT10))

Pat_PDG_move <- dplyr::select(Pat_measures, PDGA1:PDGA10, -c(SPSpostA, SPSpreB, SPSpostB, SPSpreC, 
              SPSpostC, SPSpreD), -c(PDGAT1:PDGAT10), PDGB1:PDGB10, -c(PDGBT1:PDGBT10), PDGC1:PDGC10, 
              -c(PDGCT1:PDGCT10), PDGD1:PDGD10, -c(PDGDT1:PDGDT10))

He_PDG_trust <- dplyr::select(He_measures, -c(SPSpostA, SPSpreB, SPSpostB, SPSpreC, 
              SPSpostC, SPSpreD), PDGAT1:PDGAT10, PDGBT1:PDGBT10, PDGCT1:PDGCT10, PDGDT1:PDGDT10)

Pat_PDG_trust <- dplyr::select(Pat_measures, -c(SPSpostA, SPSpreB, SPSpostB, SPSpreC, 
                SPSpostC, SPSpreD), PDGAT1:PDGAT10, PDGBT1:PDGBT10, PDGCT1:PDGCT10, PDGDT1:PDGDT10)
#-----------------------------------------------------------------------------------------------------

# SPQ-B = schizotypy score

He_schtpy <- He_measures$SPQBT
Pat_schtpy <- Pat_measures$SPQBT

#-----------------------------------------------------------------------------------------------------

# SPSpreA and SPSpostA  subjective ratings of paranoia - so we can see the impact of interacting with 
# malev/benev opponents independently. 

He_paranoia <- dplyr::select(He_measures, SPSpreA, SPSpostA, SPSpreB, SPSpostB, SPSpreC, SPSpostC,
                             SPSpreD, SPSpostD)

Pat_paranoia <- dplyr::select(Pat_measures, SPSpreA, SPSpostA, SPSpreB, SPSpostB, SPSpreC, SPSpostC,
                       SPSpreD, SPSpostD)

#-----------------------------------------------------------------------------------------------------
# LNS = working memory score. 
#-----------------------------------------------------------------------------------------------------

He_wm <- He_measures$LNS
Pat_wm <- Pat_measures$LNS

#-----------------------------------------------------------------------------------------------------

# There are anxiety and depression measures and theory of mind (hinting task) 



#-----------------------------------------------------------------------------------------------------

# Trails set-shifting measure.

#-----------------------------------------------------------------------------------------------------

# PARANOIA SCALE

# Stat tests




# Prep data for plotting
He_paranoia <- melt(He_paranoia)
Pat_paranoia <- melt(Pat_paranoia)

He_paranoia <- summarySE(He_paranoia, measurevar = "value", groupvars = "variable") 
Pat_paranoia <- summarySE(Pat_paranoia, measurevar = "value", groupvars = "variable") 

Paranoia <- rbind(He_paranoia, Pat_paranoia)

Paranoia$Group <- factor(c(rep("Healthy", 8), rep("Patient", 8)))

Paranoia$variable <- factor(Paranoia$variable, 
                           levels = c("SPSpreA", "SPSpostA", "SPSpreB", "SPSpostB", "SPSpreC",
                                      "SPSpostC", "SPSpreD", "SPSpostD"))


ggplot(Paranoia, aes(x=variable, y=value, fill=Group, ymin=value-se,ymax=value+se))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(width=.3, position = "dodge")+
  ylab("Mean paranoia")+
  xlab("Condition")+
  ggtitle("Mean paranoia scores pre- and post-PDG tasks")
         
         
  







