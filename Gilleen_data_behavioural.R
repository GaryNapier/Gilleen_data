
# Sources packages and custom functions
source("https://raw.githubusercontent.com/GaryNapier/Packages_functions/master/PACKAGES.R")

# GILLEEN DATA BEHAVIOURAL

# Attached is the complete PDG and Beads data for the Healthy and Patients. 
# There are 3 tabs, the PDG action for each round (1=compete, 2=cooperate); 
# PDG trust (the rating of trust after each round (1=totally untrustworthy...
# 7 totally trustworthy, 4 unsure) and Beads (1= definitely Jar A ... 
# 7=definitely Jar B; 4 unsure). I hope this is clear.
#                                                                                                                                                                
# As a reminder, for the PDG, A=mostly cooperative, D=mostly competitive,
# B=first half mostly cooperative switching to second half mostly competitive,
# and C vice versa). 

# p.s. there appears to be duplication of ID numbers for the patients but don't
# worry they are different - student confusion, but for data organisation on other
# files I'm leaving it as it is for now.

# set WD
tryCatch({
  setwd("U:/Gilleen_data")
}, error=function(e) {
  setwd("~/Dropbox/Gilleen_Analysis/For Gary")
})

#---------------------------------------------------------------------------------------
# Misc setup
#---------------------------------------------------------------------------------------

Bins <- 10
Alpha_hist <- .6
Alpha_dens <- .3
Alpha_line <- .3

Tasks <- c("PDG", "Beads")
Groups <- c("Healthy", "Patient")
N_tasks <- 2
N_groups <- 2

Blue <- "#F1F0FA"

Action <- c("Compete", "Cooperate")

#---------------------------------------------------------------------------------------
# Read in data
#---------------------------------------------------------------------------------------

Healthy_PDG   <- read.csv("Gilleen_healthy_PDG_trust.csv", header = TRUE)
Healthy_beads <- read.csv("Gilleen_healthy_beads.csv", header = TRUE)
Patient_PDG   <- read.csv("Gilleen_patient_PDG_trust.csv", header = TRUE)
Patient_beads <- read.csv("Gilleen_patient_beads.csv", header = TRUE)

# Clean Healthy_beads
Healthy_beads <- tryCatch({
  dplyr::select(Healthy_beads, -(X:X.11))
}, error=function(e){Healthy_beads})

N_subj_healthy <- nrow(Healthy_PDG)
N_subj_pat <- nrow(Patient_PDG)

# Healthy_PDG
# Healthy_beads
# Patient_PDG
# Patient_beads

#---------------------------------------------------------------------------------------
# Clean
#---------------------------------------------------------------------------------------

# Order by ID #
Healthy_PDG   <- Healthy_PDG[order(Healthy_PDG$ID),]
Healthy_beads <- Healthy_beads[order(Healthy_beads$ID),]
Patient_PDG   <- Patient_PDG[order(Patient_PDG$ID),]
Patient_beads <- Patient_beads[order(Patient_beads$ID),]

# Take out ID col & save as sep variable if needed
Healthy_ID <- Healthy_PDG[,1]
Patient_ID <- Patient_PDG[,1]

Healthy_PDG   <- Healthy_PDG[,-1]
Healthy_beads <- Healthy_beads[,-1]
Patient_PDG   <- Patient_PDG[,-1]
Patient_beads <- Patient_beads[,-1]

# Convert data to 0-1 scale
Healthy_PDG    <- (Healthy_PDG-1)/max(Healthy_PDG-1)
Healthy_beads  <- (Healthy_beads-1)/max(Healthy_beads-1)
Patient_PDG    <- (Patient_PDG-1)/max(Patient_PDG-1)
Patient_beads  <- (Patient_beads-1)/max(Patient_beads-1)

# Unique values on the standardised scale 
Values <- 0:6/6

# Sequences
Seq_A <-  c(1, 1, 0, 1, 1, 1, 0, 1, 1, 1)
Seq_B <-  c(1, 1, 1, 0, 1, 0, 0, 0, 1, 0)
Seq_C <-  c(0, 0, 0, 1, 0, 1, 1, 1, 0, 1)
Seq_D <-  c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0)
Seq_PDG <- c(Seq_A, Seq_B, Seq_C, Seq_D)
Seq_beads <- c(Seq_D, Seq_C, Seq_B, Seq_A)

#---------------------------------------------------------------------------------------
# Stats
#---------------------------------------------------------------------------------------

# Get means
Healthy_PDG_mean    <- colMeans(Healthy_PDG)
Healthy_beads_mean  <- colMeans(Healthy_beads)
Patient_PDG_mean    <- colMeans(Patient_PDG)
Patient_beads_mean  <- colMeans(Patient_beads)

# Get medians 
Healthy_PDG_med    <- colMedians(Healthy_PDG)
Healthy_beads_med  <- colMedians(Healthy_beads)
Patient_PDG_med    <- colMedians(Patient_PDG)
Patient_beads_med  <- colMedians(Patient_beads)

Healthy_PDG_ster    <- std.error(Healthy_PDG)
Healthy_beads_ster <- std.error(Healthy_beads)
Patient_PDG_ster    <- std.error(Patient_PDG)
Patient_beads_ster  <- std.error(Patient_beads)

# Summarise data
# See http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# Convert to long first
Healthy_PDG_long <- gather(Healthy_PDG, Sequence, Data, A:D.9)
Healthy_beads_long <- gather(Healthy_beads, Sequence, Data, A:D.9)
Patient_PDG_long <- gather(Patient_PDG, Sequence, Data, A:D.9)
Patient_beads_long <- gather(Patient_beads, Sequence, Data, A:D.9)

Healthy_PDG_sum <- summarySE(Healthy_PDG_long, measurevar="Data", groupvars="Sequence")
Healthy_beads_sum <- summarySE(Healthy_beads_long, measurevar="Data", groupvars="Sequence")
Patient_PDG_sum <- summarySE(Patient_PDG_long, measurevar="Data", groupvars="Sequence")
Patient_beads_sum <- summarySE(Patient_beads_long, measurevar="Data", groupvars="Sequence")


#---------------------------------------------------------------------------------------
# Plots
#---------------------------------------------------------------------------------------

# Plot PDG
Plot_PDG <- function(){
  ggplot(Healthy_PDG_sum, aes(x=Sequence, y=Data, group=1)) +
    geom_errorbar(aes(ymin=Data-se, ymax=Data+se), width=.2)+
    geom_line(colour = "red", size = 1, alpha=.7)+
    geom_errorbar(data=Patient_PDG_sum, aes(ymin=Data-se, ymax=Data+se), width=.2)+
    geom_line(data=Patient_PDG_sum, aes(x=Sequence, y=Data), colour = "blue", size = 1, alpha=.7)+
    geom_point(data=data.frame(Seq_PDG),
               aes(x=1:length(Seq_PDG), y=Seq_PDG, colour=factor(Seq_PDG)),
               size=3, show.legend = FALSE) +
    geom_point()+
    geom_point()+
    scale_x_discrete(labels = Seq_PDG)+
    ylim(0, 1)+
    geom_vline(xintercept = c(10.5, 20.5, 30.5), linetype = "longdash")+
    geom_hline(yintercept = Values, alpha=Alpha_line)+
    annotate("text", x = c(5, 15, 25, 35), y=0.1, label = c("A", "B", "C", "D"), fontface = "bold")+
    ggtitle("Mean PDG data, healthy (red) & patient (blue)")+
    theme_grey()
}
# x11()
Plot_PDG()

#

# Plot beads
Plot_beads <- function() {
  ggplot(Healthy_beads_sum, aes(x=Sequence, y=Data, group=1)) +
    geom_errorbar(aes(ymin=Data-se, ymax=Data+se), width=.2)+
    geom_line(colour = "red", size = 1, alpha=.7)+
    geom_errorbar(data=Patient_beads_sum, aes(ymin=Data-se, ymax=Data+se), width=.2)+
    geom_line(data=Patient_beads_sum, aes(x=Sequence, y=Data), colour = "blue", size = 1, alpha=.7)+
    geom_point(data=data.frame(Seq_beads),
               aes(x=1:length(Seq_beads), y=Seq_beads, colour=factor(Seq_beads)),
               size=3, show.legend = FALSE) +
    geom_point()+
    geom_point()+
    scale_x_discrete(labels = Seq_beads)+
    ylim(0, 1)+
    geom_vline(xintercept = c(10.5, 20.5, 30.5), linetype = "longdash")+
    geom_hline(yintercept = Values, alpha=Alpha_line)+
    annotate("text", x = c(5, 15, 25, 35), y=0.1, label = c("D", "C", "B", "A"), fontface = "bold")+
    ggtitle("Mean beads data, healthy (red) & patient (blue)")+
    theme_grey()
}
# x11()
Plot_beads()

#

# Plot individuals
Plot_ind <- function(Data_set, Subject){
  Plot_data <- data.frame(t(Data_set[Subject,]))
  Data_set_name <- deparse(substitute(Data_set))
  
  ggplot()+
    geom_line(data=Plot_data, aes(x=1:length(Plot_data
                                             [,1]), y=Plot_data[,1]),
              colour = "red", size = 1, alpha=.7)+
    geom_point(data=Plot_data, aes(x=1:length(Plot_data[,1]), y=Plot_data[,1]))+
    
    {if (Data_set_name == "Healthy_PDG" || Data_set_name == "Patient_PDG"){
      geom_point(data=data.frame(Seq_PDG),
                 aes(x=1:length(Seq_PDG), y=Seq_PDG, colour=factor(Seq_PDG)),
                 size=2, show.legend = FALSE) 
    }else{
      geom_point(data=data.frame(Seq_beads),
                 aes(x=1:length(Seq_beads), y=Seq_beads, colour=factor(Seq_beads)),
                 size=2, show.legend = FALSE) 
    }}+
    
    {if (Data_set_name == "Healthy_PDG" || Data_set_name == "Patient_PDG"){
      scale_x_continuous("Beads", breaks=1:length(Plot_data[,1]), labels=Seq_PDG)
    }else{
      scale_x_continuous("Beads", breaks=1:length(Plot_data[,1]), labels=Seq_beads)
    }}+
    
    {if (Data_set_name == "Healthy_PDG" || Data_set_name == "Patient_PDG"){
      annotate("text", x = c(5, 15, 25, 35), y=0.1, label = c("A", "B", "C", "D"), fontface = "bold")
    }else{
      annotate("text", x = c(5, 15, 25, 35), y=0.1, label = c("D", "C", "B", "A"), fontface = "bold")
    }}+
    
    coord_cartesian(ylim=c(0,1))+
    scale_y_continuous(breaks = seq(0, 1, 0.1))+
    ylab("Responses (jar probability estimate)")+
    geom_vline(xintercept = c(10.5, 20.5, 30.5), linetype = "longdash")+
    geom_hline(yintercept = Values, alpha=Alpha_line)+
    ggtitle(sprintf(strcat(Data_set_name, " subject %g"),Subject))+
    theme_grey()
  
}
# x11()
Plot_ind(Healthy_PDG, 20)


# 
# x11()
# # Histograms of mean responses on each trial (healthy PDG/beads, patient PDG/beads)
# Hist_healthy_PDG <- ggplot()+
#   geom_histogram(data= data.frame(Healthy_PDG_mean), 
#                  aes(Healthy_PDG_mean),
#                  bins=Bins, colour="gray", fill="blue", alpha=Alpha_hist)+
#   coord_cartesian(xlim=c(0,1), ylim=c(0,10))+
#   scale_x_continuous(breaks = seq(0, 1, 0.1))+
#   scale_y_continuous(breaks = seq(0, 10, 1))+
#   ggtitle("Healthy_PDG_mean")+
#   labs(x="Responses (jar probability estimate)")
# Hist_patient_PDG <- ggplot()+
#   geom_histogram(data= data.frame(Patient_PDG_mean), 
#                  aes(Patient_PDG_mean),
#                  bins=Bins, colour="gray", fill="blue", alpha=Alpha_hist)+
#   coord_cartesian(xlim=c(0,1), ylim=c(0,10))+
#   scale_x_continuous(breaks = seq(0, 1, 0.1))+
#   scale_y_continuous(breaks = seq(0, 10, 1))+
#   ggtitle("Patient_PDG_mean")+
#   labs(x="Responses (jar probability estimate)")
# Hist_healthy_beads <- ggplot()+
#   geom_histogram(data= data.frame(Healthy_beads_mean), 
#                  aes(Healthy_beads_mean),
#                  bins=Bins, colour="gray", fill="blue", alpha=Alpha_hist)+
#   coord_cartesian(xlim=c(0,1), ylim=c(0,10))+
#   scale_x_continuous(breaks = seq(0, 1, 0.1))+
#   scale_y_continuous(breaks = seq(0, 10, 1))+
#   ggtitle("Healthy_beads_mean")+
#   labs(x="Responses (jar probability estimate)")
# Hist_patient_beads <- ggplot()+
#   geom_histogram(data= data.frame(Patient_beads_mean), 
#                  aes(Patient_beads_mean),
#                  bins=Bins, colour="gray", fill="blue", alpha=Alpha_hist)+
#   coord_cartesian(xlim=c(0,1), ylim=c(0,10))+
#   scale_x_continuous(breaks = seq(0, 1, 0.1))+
#   scale_y_continuous(breaks = seq(0, 10, 1))+
#   ggtitle("Patient_beads_mean")+
#   labs(x="Responses (jar probability estimate)")
# grid.arrange(Hist_healthy_PDG, Hist_patient_PDG, Hist_healthy_beads,
#              Hist_patient_beads, ncol=2, nrow=2)



# x11()
Hist_healthy_PDG <- ggplot(data= data.frame(Healthy_PDG_mean), aes(x=Healthy_PDG_mean))+
  geom_histogram(aes(y=..count..),
                 bins =Bins, colour="gray", fill="blue", alpha=Alpha_hist)+
  coord_cartesian(xlim=c(0,1), ylim=c(0,10))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 10, 1))+
  ggtitle("Healthy_PDG_mean")+
  labs(x="Responses (jar probability estimate)", y="Count")+
  geom_density(alpha=Alpha_dens, fill="red")
Hist_patient_PDG <- ggplot(data= data.frame(Patient_PDG_mean), aes(x=Patient_PDG_mean))+
  geom_histogram(aes(y=..count..),
                 bins =Bins, colour="gray", fill="blue", alpha=Alpha_hist)+
  coord_cartesian(xlim=c(0,1), ylim=c(0,10))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 10, 1))+
  ggtitle("Patient_PDG_mean")+
  labs(x="Responses (jar probability estimate)", y="Count")+
  geom_density(alpha=Alpha_dens, fill="red")
Hist_healthy_beads <- ggplot(data= data.frame(Healthy_beads_mean), aes(x=Healthy_beads_mean))+
  geom_histogram(aes(y=..count..),
                 bins =Bins, colour="gray", fill="blue", alpha=Alpha_hist)+
  coord_cartesian(xlim=c(0,1), ylim=c(0,10))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 10, 1))+
  ggtitle("Healthy_beads_mean")+
  labs(x="Responses (jar probability estimate)", y="Count")+
  geom_density(alpha=Alpha_dens, fill="red")
Hist_patient_beads <- ggplot(data= data.frame(Patient_beads_mean), aes(x=Patient_beads_mean))+
  geom_histogram(aes(y=..count..),
                 bins =Bins, colour="gray", fill="blue", alpha=Alpha_hist)+
  coord_cartesian(xlim=c(0,1), ylim=c(0,10))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 10, 1))+
  ggtitle("Patient_beads_mean")+
  labs(x="Responses (jar probability estimate)", y="Count")+
  geom_density(alpha=Alpha_dens, fill="red")
grid.arrange(Hist_healthy_PDG, Hist_patient_PDG, Hist_healthy_beads,
             Hist_patient_beads, ncol=2, nrow=2, 
             top=textGrob("Histograms of mean responses on each trial"))


#-----------------------------------------------------------------------------------------------
# For the behavioural measures, we want to know if there are group differences in 

# a) 1st half ‘draws to certainty (DTC)’ - no. of choices until subjects
# reach maximum certainty in 1st half either task.
#-----------------------------------------------------------------------------------------------

# Get_how_many_DTC <- function(){
FH_1 <- 1:5
FH_2 <- 11:15
FH_3 <- 21:25
FH_4 <- 31:35
Draws <- c("Draw_1", "Draw_2", "Draw_3", "Draw_4", "Draw_5")

# PDG 
Healthy_PDG_A <- Healthy_PDG[FH_1] # Blue jar (1)
Healthy_PDG_B <- Healthy_PDG[FH_2] # Blue 1st half (1), Red 2nd (0)
Healthy_PDG_C <- Healthy_PDG[FH_3] # Red 1st (0), Blue 2nd (1)
Healthy_PDG_D <- Healthy_PDG[FH_4] # Red jar

Patient_PDG_A <- Patient_PDG[FH_1]
Patient_PDG_B <- Patient_PDG[FH_2]
Patient_PDG_C <- Patient_PDG[FH_3]
Patient_PDG_D <- Patient_PDG[FH_4]

# How many subj reached DTC at which bead index? PDG
How_many_DTC_He_A <- sapply(Healthy_PDG_A, function(x) sum(x==1)  ) # Blue jar (1)
How_many_DTC_He_B <- sapply(Healthy_PDG_B, function(x) sum(x==1)  ) # Blue 1st half (1), Red 2nd (0)
How_many_DTC_He_C <- sapply(Healthy_PDG_C, function(x) sum(x==0)  ) # Red 1st (0), Blue 2nd (1)
How_many_DTC_He_D <- sapply(Healthy_PDG_D, function(x) sum(x==0)  ) # Red jar (0)

How_many_DTC_healthy_PDG <- data.frame(Draws=Draws,
                                       Proportion_subjects = colMeans(rbind(How_many_DTC_He_A, 
                                                                How_many_DTC_He_B, 
                                                                How_many_DTC_He_C, 
                                                                How_many_DTC_He_D) / N_subj_healthy ))

How_many_pat_A <- sapply(Patient_PDG_A, function(x) sum(x==1)  )
How_many_pat_B <- sapply(Patient_PDG_B, function(x) sum(x==1)  )
How_many_pat_C <- sapply(Patient_PDG_C, function(x) sum(x==0)  )
How_many_pat_D <- sapply(Patient_PDG_D, function(x) sum(x==0)  )

How_many_DTC_pat_PDG <- data.frame(Draws=Draws,
                                   Proportion_subjects = colMeans(rbind(How_many_pat_A, 
                                                            How_many_pat_B,
                                                            How_many_pat_C,
                                                            How_many_pat_D) / N_subj_pat ))

# BEADS
Healthy_beads_A <- Healthy_beads[FH_1]
Healthy_beads_B <- Healthy_beads[FH_2]
Healthy_beads_C <- Healthy_beads[FH_3]
Healthy_beads_D <- Healthy_beads[FH_4]

Patient_beads_A <- Patient_beads[FH_1]
Patient_beads_B <- Patient_beads[FH_2]
Patient_beads_C <- Patient_beads[FH_3]
Patient_beads_D <- Patient_beads[FH_4]

# How many subj reached DTC at which bead index? Beads
How_many_DTC_He_beads_A <- sapply(Healthy_beads_A, function(x) sum(x==0)  ) # Blue jar (1)
How_many_DTC_He_beads_B <- sapply(Healthy_beads_B, function(x) sum(x==0)  ) # Blue 1st half (1), Red 2nd (0)
How_many_DTC_He_beads_C <- sapply(Healthy_beads_C, function(x) sum(x==1)  ) # Red 1st (0), Blue 2nd (1)
How_many_DTC_He_beads_D <- sapply(Healthy_beads_D, function(x) sum(x==1)  ) # Red jar (0)

How_many_DTC_healthy_beads <- data.frame(Draws=Draws, 
                                         Proportion_subjects = colMeans(rbind(How_many_DTC_He_beads_A,
                                                                  How_many_DTC_He_beads_B, 
                                                                  How_many_DTC_He_beads_C,
                                                                  How_many_DTC_He_beads_D) / N_subj_healthy))

How_many_DTC_pat_beads_A <- sapply(Patient_beads_A, function(x) sum(x==1)  )
How_many_DTC_pat_beads_B <- sapply(Patient_beads_B, function(x) sum(x==1)  )
How_many_DTC_pat_beads_C <- sapply(Patient_beads_C, function(x) sum(x==0)  )
How_many_DTC_pat_beads_D <- sapply(Patient_beads_D, function(x) sum(x==0)  )

How_many_DTC_pat_beads <- data.frame(Draws=Draws, 
                                     Proportion_subjects = colMeans(rbind(How_many_DTC_pat_beads_A, 
                                                              How_many_DTC_pat_beads_B, 
                                                              How_many_DTC_pat_beads_C,
                                                              How_many_DTC_pat_beads_D) / N_subj_pat ))

# return(How_many_DTC_healthy_PDG)
# return(How_many_DTC_pat_PDG)
# return(How_many_DTC_healthy_beads)
# return(How_many_DTC_pat_beads)
# }

# Get_how_many_DTC()

# Plots of how many DTC
DTC_PDG <- ggplot(NULL, aes(Draws, Proportion_subjects))+
  theme_grey()+
  stat_summary_bin(aes(fill = "Healthy"),
                   data=How_many_DTC_healthy_PDG,
                   fun.y="sum", geom = "bar", alpha=.5)+
  stat_summary_bin(aes(fill= "Patient"), 
                   data=How_many_DTC_pat_PDG, 
                   fun.y="sum", geom = "bar", alpha=.5)+
  theme(legend.position="bottom")+
  ggtitle("PDG")+
  scale_y_continuous(limits=c(0, 0.15))
  

DTC_beads <- ggplot(NULL, aes(Draws, Proportion_subjects))+
  stat_summary_bin(aes(fill = "Healthy"),
                   data=How_many_DTC_healthy_beads,
                   fun.y="sum", geom = "bar", alpha=.5)+
  stat_summary_bin(aes(fill= "Patient"), 
                   data=How_many_DTC_pat_beads, 
                   fun.y="sum", geom = "bar", alpha=.5)+
  ggtitle("Beads")+
  scale_y_continuous(limits=c(0, 0.15))+
  theme_grey()+
  theme(legend.position="none")

Legend <- g_legend(DTC_PDG)
grid.arrange(DTC_PDG+theme(legend.position="none"), DTC_beads, Legend, ncol=2, nrow=2, 
             layout_matrix=rbind(c(1, 2), c(3, 3)), 
             top="Proportion of subjects making DTC, averaged across all four beads tasks",
             widths=c(2.7, 2.7), heights=c(2.5, 0.2))

#-----------------------------------------------------------------------------------------------
# Count how many draws to certainty
# Healthy PDG
DTC_A <- apply(Healthy_PDG_A[,c(1:5)], 1, function(x) min(which(x==1)))
DTC_A <- ifelse(DTC_A==Inf, 5, DTC_A)
DTC_B <- apply(Healthy_PDG_B[,c(1:5)], 1, function(x) min(which(x==1)))
DTC_B <- ifelse(DTC_B==Inf, 5, DTC_B)
DTC_C <- apply(Healthy_PDG_C[,c(1:5)], 1, function(x) min(which(x==0)))
DTC_C <- ifelse(DTC_C==Inf, 5, DTC_C)
DTC_D <- apply(Healthy_PDG_D[,c(1:5)], 1, function(x) min(which(x==0)))
DTC_D <- ifelse(DTC_D==Inf, 5, DTC_D)

DTC_healthy_PDG <- (sum(c(DTC_A, DTC_B, DTC_C, DTC_D)) / 4) / N_subj_healthy

# Patient PDG
DTC_A <- apply(Patient_PDG_A[,c(1:5)], 1, function(x) min(which(x==1)))
DTC_A <- ifelse(DTC_A==Inf, 5, DTC_A)
DTC_B <- apply(Patient_PDG_B[,c(1:5)], 1, function(x) min(which(x==1)))
DTC_B <- ifelse(DTC_B==Inf, 5, DTC_B)
DTC_C <- apply(Patient_PDG_C[,c(1:5)], 1, function(x) min(which(x==0)))
DTC_C <- ifelse(DTC_C==Inf, 5, DTC_C)
DTC_D <- apply(Patient_PDG_D[,c(1:5)], 1, function(x) min(which(x==0)))
DTC_D <- ifelse(DTC_D==Inf, 5, DTC_D)

DTC_pat_PDG <- (sum(c(DTC_A, DTC_B, DTC_C, DTC_D)) / 4) / N_subj_pat

# Healthy beads
DTC_A <- apply(Healthy_beads_A[,c(1:5)], 1, function(x) min(which(x==1)))
DTC_A <- ifelse(DTC_A==Inf, 5, DTC_A)
DTC_B <- apply(Healthy_beads_B[,c(1:5)], 1, function(x) min(which(x==1)))
DTC_B <- ifelse(DTC_B==Inf, 5, DTC_B)
DTC_C <- apply(Healthy_beads_C[,c(1:5)], 1, function(x) min(which(x==0)))
DTC_C <- ifelse(DTC_C==Inf, 5, DTC_C)
DTC_D <- apply(Healthy_beads_D[,c(1:5)], 1, function(x) min(which(x==0)))
DTC_D <- ifelse(DTC_D==Inf, 5, DTC_D)

DTC_healthy_beads <- (sum(c(DTC_A, DTC_B, DTC_C, DTC_D)) / 4) / N_subj_healthy

# Patient beads
DTC_A <- apply(Patient_beads_A[,c(1:5)], 1, function(x) min(which(x==1)))
DTC_A <- ifelse(DTC_A==Inf, 5, DTC_A)
DTC_B <- apply(Patient_beads_B[,c(1:5)], 1, function(x) min(which(x==1)))
DTC_B <- ifelse(DTC_B==Inf, 5, DTC_B)
DTC_C <- apply(Patient_beads_C[,c(1:5)], 1, function(x) min(which(x==0)))
DTC_C <- ifelse(DTC_C==Inf, 5, DTC_C)
DTC_D <- apply(Patient_beads_D[,c(1:5)], 1, function(x) min(which(x==0)))
DTC_D <- ifelse(DTC_D==Inf, 5, DTC_D)

DTC_pat_beads <- (sum(c(DTC_A, DTC_B, DTC_C, DTC_D)) / 4) / N_subj_pat

DTC <- signif(c(DTC_healthy_PDG, DTC_pat_PDG, DTC_healthy_beads, DTC_pat_beads), digits=3)

htmlTable(DTC, cgroup=Tasks, n.cgroup= rep(2, N_tasks), header=rep(Groups, 2), 
          align = c("c", "|"), align.header=c("c", "|"), col.columns=c("none", Blue), 
          caption = "Average draws to certainty over first half of seqs A:D")

#-----------------------------------------------------------------------------------------------
# b) 2nd half DTC - no. of choices until subjects reach maximum certainty after the change 
# of jar/trustworthiness in sequences B and C only
#-----------------------------------------------------------------------------------------------
# Get second half indeces of seqs B & C, PDG & beads
Index_PDG_B <- 16:20
Index_PDG_C <- 26:30

Index_beads_C <- 16:20
Index_beads_B <- 26:30

# PDG
Healthy_PDG_B <- Healthy_PDG[Index_PDG_B] # Blue 1st half (1), Red 2nd (0)
Healthy_PDG_C <- Healthy_PDG[Index_PDG_C] # Red 1st (0), Blue 2nd (1)

Patient_PDG_B <- Patient_PDG[Index_PDG_B]
Patient_PDG_C <- Patient_PDG[Index_PDG_C]

# Beads
Healthy_beads_B <- Healthy_beads[Index_beads_B]
Healthy_beads_C <- Healthy_beads[Index_beads_C]

Patient_beads_B <- Patient_beads[Index_beads_B]
Patient_beads_C <- Patient_beads[Index_beads_C]

# Count how many draws to certainty, 2nd half BC

# Healthy PDG BC
DTC_B <- apply(Healthy_PDG_B, 1, function(x) min(which(x==1)))
DTC_B <- ifelse(DTC_B==Inf, 5, DTC_B)
DTC_C <- apply(Healthy_PDG_C[,c(1:5)], 1, function(x) min(which(x==1)))
DTC_C <- ifelse(DTC_C==Inf, 5, DTC_C)

DTC_healthy_PDG_BC <- (sum(c(DTC_B, DTC_C)) / 2) / N_subj_healthy

# Patient PGD BC
DTC_B <- apply(Patient_PDG_B, 1, function(x) min(which(x==1)))
DTC_B <- ifelse(DTC_B==Inf, 5, DTC_B)
DTC_C <- apply(Patient_PDG_C, 1, function(x) min(which(x==1)))
DTC_C <- ifelse(DTC_C==Inf, 5, DTC_C)

DTC_patient_PDG_BC <- (sum(c(DTC_B, DTC_C)) / 2) / N_subj_pat

# Healthy beads BC
DTC_B <- apply(Healthy_beads_B, 1, function(x) min(which(x==1)))
DTC_B <- ifelse(DTC_B==Inf, 5, DTC_B)
DTC_C <- apply(Healthy_beads_C, 1, function(x) min(which(x==1)))
DTC_C <- ifelse(DTC_C==Inf, 5, DTC_C)

DTC_healthy_beads_BC <- (sum(c(DTC_B, DTC_C)) / 2) / N_subj_healthy

# Patient beads BC
DTC_B <- apply(Patient_beads_B, 1, function(x) min(which(x==1)))
DTC_B <- ifelse(DTC_B==Inf, 5, DTC_B)
DTC_C <- apply(Patient_beads_C, 1, function(x) min(which(x==1)))
DTC_C <- ifelse(DTC_C==Inf, 5, DTC_C)

DTC_patient_beads_BC <- (sum(c(DTC_B, DTC_C)) / 2) / N_subj_pat

DTC_BC <- signif(c(DTC_healthy_PDG_BC, 
                   DTC_patient_PDG_BC, 
                   DTC_healthy_beads_BC, 
                   DTC_patient_beads_BC), digits=3)

htmlTable(DTC_BC, cgroup=Tasks, n.cgroup= rep(2, N_tasks), header=rep(Groups, 2), 
          align = c("c", "|"), align.header=c("c", "|"), col.columns=c("none", Blue), 
          caption = "Average draws to certainty over second half of seqs B & C")



#-----------------------------------------------------------------------------------------------
# c) initial decision to cooperate/compete in the PDG (I don’t know if James has supplied 
# this data - NB I’m talking about the initial decision, not the initial trustworthiness 
# rating which comes after seeing what the opponent did)
# (1=compete, 2=cooperate)
#-----------------------------------------------------------------------------------------------

Healthy_PDG_act <- read.csv("Gilleen_healthy_PGD_action.csv", header=TRUE)
Pat_PDG_act <- read.csv("Gilleen_patient_PDG_action.csv", header=TRUE)

# Drop ID and save
Healthy_ID_PDG_action <- Healthy_PDG_act[,1]
Patient_ID_PDG_action <- Pat_PDG_act[,1]

Healthy_PDG_act <- Healthy_PDG_act[,-1]
Pat_PDG_act <- Pat_PDG_act[,-1]

# Get initial compete/cooperate actions
Indeces <- c(1, 11, 21, 31)
Healthy_actions <- Healthy_PDG_act[, Indeces]
Healthy_actions <- unname(unlist(Healthy_actions))

Pat_actions <- Pat_PDG_act[, Indeces]
Pat_actions <- unname(unlist(Pat_actions))

# Get proportions
Healthy_prop_compete <- sum(Healthy_actions==1) / N_subj_healthy / 4 # Compete
Healthy_prop_cooperate <- sum(Healthy_actions==2) / N_subj_healthy / 4 # Cooperate

Pat_prop_compete <- sum(Pat_actions==1) / N_subj_pat / 4
Pat_prop_cooperate <- sum(Pat_actions==2) / N_subj_pat / 4

# Standard errors
SE_prop <- function(p, n){
  sqrt((p*(1-p))/n)
}

SE_healthy_compete <-  SE_prop(Healthy_prop_compete, N_subj_healthy)
SE_healthy_cooperate <- SE_prop(Healthy_prop_cooperate, N_subj_healthy)
SE_pat_compete <- SE_prop(Pat_prop_compete, N_subj_pat)
SE_pat_cooperate <- SE_prop(Pat_prop_cooperate, N_subj_pat)

SE_action <- c(SE_healthy_compete, SE_healthy_cooperate, SE_pat_compete, SE_pat_cooperate)
Limits <- aes()

# Put proportions into dataframe & htmlTable
Action_table <- signif(c(Healthy_prop_compete, Healthy_prop_cooperate, 
                         Pat_prop_compete, Pat_prop_cooperate), digits=3)

Action_table <- rbind(c(sum(Healthy_actions==1), sum(Healthy_actions==2), 
                        sum(Pat_actions==1), sum(Pat_actions==2)), Action_table)

htmlTable(Action_table, cgroup=Groups, n.cgroup= rep(2, N_tasks), header=rep(Action, 2), 
          align = c("c", "|"), align.header=c("c", "|"), col.columns=c("none", Blue), 
          rnames=c("Total", "Proportion"),
          caption = "Proportion of subjects' initial decisions in PDG, 
          across all four seqs")

Groups <- c(rep("Healthy", 2), rep("Patient", 2))

Group_action <- c("Healthy_compete", "Healthy_cooperate", "Patient_compete", "Patient_cooperate")

Action_df <- data.frame(Groups, Group_action, Action_table)

ggplot(Action_df, aes(x=Group_action, y=Action_table, fill=Groups))+
  geom_bar(stat="identity") +
  scale_fill_discrete(name="Group", 
                      breaks=c(1, 2), 
                      labels=c("Healthy", "Patient"))+
  xlab("Group & action")+ylab("Proportion of subjects")+
  geom_errorbar(aes(ymin=Action_table-SE_action, ymax=Action_table+SE_action), width=0.25)+
  theme_gray()

# Stats
# Question: Do healthy differ from groups in the number of compete moves?

Group <- c(rep("Healthy",  N_subj_healthy*4), rep("Patient", N_subj_pat*4))

Test <- data.frame(Action=c(Healthy_actions, Pat_actions), Group)
           
Table <- xtabs(~ Group+Action, data=Test)
Prop_test <- prop.test(Table)

Test_table <- signif(c(Prop_test$statistic, Prop_test$parameter, Prop_test$p.value), digits=3)

htmlTable(Test_table, header=c("Chi-sq", "d.f.", "p"), 
          align = c("c", "|"), align.header=c("c", "|"), col.columns=c("none", Blue), 
          caption = txtMergeLines("Chi-sq test of proportions for initial action on PDG"))

#-----------------------------------------------------------------------------------------------
# d) are the group average trust ratings on each decision point the same in sequences
# B and C? i.e. do we find the effect James found in the dissertation (Figure 5), that 
# Scz subjects have lower trust ratings in the ‘bad opponent’ condition but not the 
# ‘good opponent' condition?
#-----------------------------------------------------------------------------------------------


Index_A <- 1:10  # Mostly 1 - cooperate?
Index_D <- 31:40 # Mostly 0 - compete?

Healthy_PDG_A <- Healthy_PDG[Index_A]
Healthy_PDG_D <- Healthy_PDG[Index_D]

Pat_PDG_A <- Patient_PDG[Index_A]
Pat_PDG_D <- Patient_PDG[Index_D]

Test_A <- sapply(seq(Healthy_PDG_A), function(i) t.test(Healthy_PDG_A[,i], Pat_PDG_A[,i]))
Test_D <- sapply(seq(Healthy_PDG_D), function(i) t.test(Healthy_PDG_D[,i], Pat_PDG_D[,i]))

# Get vector of p-vals
P_vals <- c(signif(unlist(Test_A["p.value",]), digits=3), signif(unlist(Test_D["p.value",]), digits=3) )

Sig <- c(which(P_vals <= 0.05) )


# Plot Seqs A and D
Plot_PDG_AD <- function(){
  ggplot(Healthy_PDG_sum[c(Index_A, Index_D), ], aes(x=Sequence, y=Data, group=1)) +
    geom_errorbar(aes(ymin=Data-se, ymax=Data+se), width=.2)+
    geom_line(colour = "red", size = 1, alpha=.7)+
    geom_errorbar(data=Patient_PDG_sum[c(Index_A, Index_D), ],
                  aes(ymin=Data-se, ymax=Data+se), width=.2)+
    geom_line(data=Patient_PDG_sum[c(Index_A, Index_D), ], 
              aes(x=Sequence, y=Data), colour = "blue", size = 1, alpha=.7)+
    geom_point(data=data.frame(Seq_PDG[c(Index_A, Index_D)]),
               aes(x=1:length(Seq_PDG[c(Index_A, Index_D)]),
                   y=Seq_PDG[c(Index_A, Index_D)], colour=factor(Seq_PDG[c(Index_A, Index_D)])),
               size=5, show.legend = FALSE) +
    geom_point()+
    geom_point()+
    scale_x_discrete(labels = Seq_PDG[c(Index_A, Index_D)])+
    ylim(0, 1)+
    geom_vline(xintercept = c(10.5), linetype = "longdash")+
    geom_hline(yintercept = Values, alpha=Alpha_line)+
    annotate("text", x = c(5, 15), y=0.1, label = c("A", "D"), fontface = "bold")+
    annotate("text", x = Sig, y = 0.8, label="*", fontface="bold", size=8)+
    ggtitle("Mean PDG data, healthy (red) & patient (blue). * = significant")+
    theme_grey()
}
# x11()
Plot_PDG_AD()

Test_A[c("statistic", "parameter", "p.value")]








# 
# While the mean and standard deviation are descriptive statistics, 
# the mean and standard error describes bounds for a random sampling process.
# This difference changes the meaning of what is being reported: a description of variation 
# in measurements vs a statement of uncertainty around the estimate of the mean.
# In other words standard error shows how close your sample mean is to the population mean.
# Standard deviation shows how much individuals within the same sample differ 
# from the sample mean. This also means that standard error should decrease if the 
# sample size increases, as the estimate of the population mean improves. 
# Standard deviation will not be affected by sample size.
# 



















