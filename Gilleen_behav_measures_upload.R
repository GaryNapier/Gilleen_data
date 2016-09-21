

source("https://raw.githubusercontent.com/GaryNapier/Packages_functions/master/PACKAGES.R")

# Set the theme
theme_set(theme_grey())

Blue <- "#F1F0FA"

setwd("U:/Gilleen_data")

He_measures <- read.csv("Healthy_measures.csv")
Pat_measures <- read.csv("Pat_measures.csv")

N_he <- nrow(He_measures)
N_pat <- nrow(Pat_measures)

# names(He_measures)
# names(Pat_measures)

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

# There are anxiety and depression measures 

#-----------------------------------------------------------------------------------------------------

# Trails set-shifting measure.

#-----------------------------------------------------------------------------------------------------

# PARANOIA SCALE

# SPSpreA and SPSpostA  subjective ratings of paranoia - so we can see the impact of interacting with 
# malev/benev opponents independently. 

He_paranoia <- dplyr::select(He_measures, SPSpreA, SPSpostA, SPSpreB, SPSpostB, SPSpreC, SPSpostC,
                             SPSpreD, SPSpostD)

Pat_paranoia <- dplyr::select(Pat_measures, SPSpreA, SPSpostA, SPSpreB, SPSpostB, SPSpreC, SPSpostC,
                              SPSpreD, SPSpostD)

# Stat tests on paranoia scale

Para_tests <- sapply(seq(He_paranoia), function(i) t.test(He_paranoia[,i], Pat_paranoia[,i]))

# Get p-vals for plotting
P_vals <- signif(unlist(Para_tests["p.value",]), digits=3 )
Sig <- c(which(P_vals <= 0.05) )

# Tests table

Para_tests_table <- rbind(Para_tests[c("statistic", "parameter", "p.value"),],
sapply(seq(He_paranoia), function(i) cohensD(He_paranoia[,i], Pat_paranoia[,i], method = "unequal")) )

rownames(Para_tests_table) <- c("t", "df", "p", "d")

colnames(Para_tests_table) <- colnames(He_paranoia)

htmlTable(txtRound(Para_tests_table, digits = 2), 
          align = c("c", "|"), align.header=c("c", "|"), col.columns=c("none", Blue),
          caption = "t-tests on each paranoia measure, healthy vs patient")

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
  geom_bar(stat = "identity", position = "dodge", alpha = .85)+
  geom_errorbar(width=.3, position = "dodge")+
  ylab("Mean paranoia")+
  xlab("Condition")+
  annotate("text", x = Sig, y = 20, label="*", fontface="bold", size=8)+
  ggtitle("Mean paranoia scores pre- and post-PDG tasks (* = significant)")

#-----------------------------------------------------------------------------------------------------

# SCHIZOTYPY SCORES

# SPQ-B = schizotypy score - from James

He_stpy <- He_measures$SPQBT
Pat_stpy <- Pat_measures$SPQBT

T_stpy <- t.test(He_stpy, Pat_stpy)
Effect_stpy <- cohensD(He_stpy, Pat_stpy, method = "unequal")
Means_stpy <- T_stpy["estimate"]

Stpy_table <- melt(data.frame(T_stpy[c("parameter", "statistic", "p.value")]))

Stpy_table <- rbind(Means_stpy[[1]][1], Means_stpy[[1]][2], Stpy_table, Effect_stpy)

Stpy_table$variable <- NULL

rownames(Stpy_table) <- c("Mean healthy", "Mean patient", "df", "t", "p", "d")

colnames(Stpy_table) <- ""

Stpy_html <- htmlTable(txtRound(Stpy_table, digits = 2), align = c("l", "|"), 
          align.header=c("c", "|"), col.columns=c("none", Blue), header = "", 
          caption = "Schizotypy - SPQBT")

#-----------------------------------------------------------------------------------------------------

# WORKING MEMORY

# LNS = working memory score. 

He_wm <- He_measures$LNS
Pat_wm <- Pat_measures$LNS

T_wm <- t.test(He_wm, Pat_wm)
Effect_wm <- cohensD(He_wm, Pat_wm, method = "unequal")
Means_wm <- T_wm["estimate"]

wm_table <- melt(data.frame(T_wm[c("parameter", "statistic", "p.value")]))

wm_table <- rbind(Means_wm[[1]][1], Means_wm[[1]][2], wm_table, Effect_wm)

wm_table$variable <- NULL

rownames(wm_table) <- c("Mean healthy", "Mean patient", "df", "t", "p", "d")

colnames(wm_table) <- ""

wm_html <- htmlTable(txtRound(wm_table, digits = 2), align = c("l", "|"), 
          align.header=c("c", "|"), col.columns=c("none", Blue), header = "", 
          caption = "Working memory - LNS")

#-----------------------------------------------------------------------------------------------------

# NART 

He_nart <- He_measures$NART
Pat_nart <- Pat_measures$NART

T_nart <- t.test(He_nart, Pat_nart)
Effect_nart <- cohensD(He_nart, Pat_nart, method = "unequal")
Means_nart <- T_nart["estimate"]

nart_table <- melt(data.frame(T_nart[c("parameter", "statistic", "p.value")]))

nart_table <- rbind(Means_nart[[1]][1], Means_nart[[1]][2], nart_table, Effect_nart)

nart_table$variable <- NULL

rownames(nart_table) <- c("Mean healthy", "Mean patient", "df", "t", "p", "d")

colnames(nart_table) <- ""

nart_html <- htmlTable(txtRound(nart_table, digits = 2), align = c("l", "|"), 
          align.header=c("c", "|"), col.columns=c("none", Blue), header = "", 
          caption = "NART")

#-----------------------------------------------------------------------------------------------------

# EDUCATION

He_edu <- He_measures$Education
Pat_edu <- Pat_measures$Education

T_edu <- t.test(He_edu, Pat_edu)
Effect_edu <- cohensD(He_edu, Pat_edu, method = "unequal")
Means_edu <- T_edu["estimate"]

edu_table <- melt(data.frame(T_edu[c("parameter", "statistic", "p.value")]))

edu_table <- rbind(Means_edu[[1]][1], Means_edu[[1]][2], edu_table, Effect_edu)

edu_table$variable <- NULL

rownames(edu_table) <- c("Mean healthy", "Mean patient", "df", "t", "p", "d")

colnames(edu_table) <- ""

edu_html <- htmlTable(txtRound(edu_table, digits = 2), align = c("l", "|"), 
          align.header=c("c", "|"), col.columns=c("none", Blue), header = "", 
          caption = "Education")

#-----------------------------------------------------------------------------------------------------

# TOM - theory of mind (hinting task) 

He_tom <- He_measures$HintingTotal
Pat_tom <- Pat_measures$HintingTotal

T_tom <- t.test(He_tom, Pat_tom)
Effect_tom <- cohensD(He_tom, Pat_tom, method = "unequal")
Means_tom <- T_tom["estimate"]

tom_table <- melt(data.frame(T_tom[c("parameter", "statistic", "p.value")]))

tom_table <- rbind(Means_tom[[1]][1], Means_tom[[1]][2], tom_table, Effect_tom)

tom_table$variable <- NULL

rownames(tom_table) <- c("Mean healthy", "Mean patient", "df", "t", "p", "d")

colnames(tom_table) <- ""

tom_html <- htmlTable(txtRound(tom_table, digits = 2), align = c("l", "|"), 
          align.header=c("c", "|"), col.columns=c("none", Blue), header = "", 
          caption = "Theory of mind - hinting task")

#-----------------------------------------------------------------------------------------------------

# RUN CONCATENATED HTML TABLES - behav. measures

print(Add_html(list(Stpy_html, wm_html, nart_html, edu_html, tom_html)))









