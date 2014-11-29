###########################################################################################################################
# R code for by-word ssanova comparisons using the gss package with comparison plots using ggplot2.
# written based on code from Jon Brennan and Josef Freuhwald, with input from Tal Linzen.
###########################################################################################################################

############################################################################################################################
# Notes:
#  Uses the libraries gss and ggplot2. These must be installed before the code will work.
#  Assumes that you have the file sample_JASA2006_SSANOVAdata.csv downloaded as well.
#
#  Currently set up for .png or similar output.
#  Some image file formats (in particular .eps) don't support the type of transparency parameters used in the plots.
#  To get useable .eps output, remove the "alpha = x" parameter on lines 58, 59, 70, 81
#  Output won't look quite as good without it, but should still be interpretable.
#############################################################################################################################


# This sample script is set up to show comparison between highest tongue position in "Baghdad" and "bagged apples"

# load relevant libraries
library(ggplot2)
library(gss)

# load data
dat <- read.csv("sample_JASA2006_SSANOVAdata.csv")


# Simple version (requires that you have the file ssaplot.R)
source("ssaplot.R")

# You should now have a function ssaplot available:
# takes the basic arguments group1, group2, data - group1 and group2 are the groups to be compared, data is a dataframe
# there are also a number of optional arguments:
# plot can be "main" or "interaction" - main effects plot or interaction plots
# color can be TRUE or FALSE - color or greyscale plot
# points can be TRUE or FALSE - plot individual points in addition to the splines?

# Currently requires that the "group" variable be called word and that your position variable be X and measured variable be Y.
# no particular ordering necessary

ssaplot("bagged", "baghdad",dat)

# a couple of variations:
#BCI interaction plot
ssaplot("bagged", "baghdad",dat, plot="interaction")

#B&W for publication
ssaplot("bagged", "baghdad",dat, color=FALSE)

#Plots individual repetitions on top of the spline
ssaplot("bagged", "baghdad",dat,points=TRUE)

# if you need to modify the plot further by hand, like to give it a title
myplot <- ssaplot("bagged", "baghdad",dat)
print(myplot+ggtitle("A comparison plot"))



#######################################################
# DIY version (ssaplot is a wrapper around this code) #
#######################################################

# setting up comparisons:

# Words/contexts to be compared:
group1 <-"bagged"
group2 <-"baghdad"

#making a data frame containing just the target words/groups
w1w2<-droplevels(subset(dat,word%in%c(group1,group2)))

# fit model
ssa.fit <-ssanova(Y~word+X+word:X,data=w1w2)

# get predictions for constructing confidence intervals
X=seq(min(w1w2$X),max(w1w2$X),by=0.01)
grid <- expand.grid(X=X ,word = c(group1,group2))
grid$ssa.fit <- predict(ssa.fit,newdata = grid,se = T)$fit
grid$ssa.SE <- predict(ssa.fit,newdata = grid,se = T)$se.fit


# plotting comparison:
# set up plot object:
comparison <- ggplot(grid,aes(x = X,colour = word,group = word))+ theme_bw()

# for greyscale plots, uncomment the next line
# comparison <- comparison + scale_fill_grey()
cbPalette <- c("#999999", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# for colors that will print well in greyscale, uncomment the next line
# comparison <- comparison+scale_fill_brewer(palette="PuBuGn")

# Main effects plot
comparison<-comparison  + geom_line(aes(y = ssa.fit),alpha = 1,colour = "grey20")
comparison<-comparison + geom_ribbon(aes(ymin = ssa.fit-(1.96*ssa.SE), ymax = ssa.fit+(1.96*ssa.SE),fill = word ),alpha = 0.75,colour = "NA")

#flip the Y axis
comparison<-comparison + scale_y_reverse()+ scale_fill_manual(values=cbPalette)
# labels
comparison<-comparison + ylab("y")
print(comparison)



# Add individual data points:
comparison <- comparison+geom_point(data= w1w2, aes(x=jitter(X),y=Y),alpha = I(1/3))
comparison


# Interaction effects plots:
X=seq(min(w1w2$X),max(w1w2$X),by=0.01)
grid <- expand.grid(X = X,word = c(group1,group2))
grid$Fit <- predict(ssa.fit,grid,se = T,inc = c("word","word:X"))$fit
grid$SE <- predict(ssa.fit,grid,se = T,inc = c("word","word:X"))$se.fit
inter <- ggplot(grid,aes(x=X))+theme_bw()
inter <- inter + geom_line(aes(y = Fit))
inter <- inter + geom_ribbon(aes(ymax = Fit+(1.96*SE),ymin = Fit -(1.96*SE)),alpha=0.5)
inter <- inter + facet_wrap(~word)
inter <- inter + geom_hline(y = 0,lty = 2)
inter <- inter + ylab("y")
inter
