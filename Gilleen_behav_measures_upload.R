

source("https://raw.githubusercontent.com/GaryNapier/Packages_functions/master/PACKAGES.R")

He_measures <- read.csv("Healthy_measures.csv")
Pat_measures <- read.csv("Pat_measures.csv")


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

Pat_paranoia <- select(Pat_measures, SPSpreA, SPSpostA, SPSpreB, SPSpostB, SPSpreC, SPSpostC,
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

# Make some plots

# Add paranoia to PDG and beads averaged plots

Plot_PDG <- function(){
  ggplot(Healthy_PDG_sum, aes(x=Sequence, y=Data, group=1)) +
    geom_errorbar(aes(ymin=Data-se, ymax=Data+se), width=.2)+
    geom_line(colour = "red", size = 1, alpha=.7)+
    geom_errorbar(data=Patient_PDG_sum, aes(ymin=Data-se, ymax=Data+se), width=.2)+
    geom_line(data=Patient_PDG_sum, aes(x=Sequence, y=Data), colour = "blue", size = 1, alpha=.7)+
    geom_point(data=data.frame(Seq_PDG),
               aes(x=1:length(Seq_PDG), y=Seq_PDG, colour=factor(Seq_PDG)),
               size=3, show.legend = FALSE)+
    geom_bar(stat="identity", data = data.frame(colMeans(He_paranoia)), width = 0.2)+
    scale_x_discrete(labels = Seq_PDG)+
    expand_limits(x = c(1, length(Seq_PDG)+3))+
    scale_y_continuous(breaks = seq(0,1,0.1), limits = c(-0.1, 1.1))+
    ylab("Trustworthiness rating")+
    geom_vline(xintercept = c(10.5, 20.5, 30.5), linetype = "longdash")+
    geom_hline(yintercept = Values, alpha=Alpha_line)+
    annotate("text", x = c(5, 15, 25, 35), y=0.1, label = c("A", "B", "C", "D"), fontface = "bold")+
    annotate("text", x = length(Seq_PDG)/2, y=c(-0.1, 1.1), label=c("Compete", "Cooperate"))+
    annotate("text", x=length(Seq_PDG)+1, y=Values, label=as.character(1:7), size=3 )+
    annotate("text", x = length(Seq_PDG)+2, y=0.5, label="Original scale", angle=270)+
    ggtitle("Mean PDG data, healthy (red) & patient (blue).\n0 = totally untrustworthy, 1 = totally trustworthy")+
    theme_grey()
}

Plot_PDG()
















