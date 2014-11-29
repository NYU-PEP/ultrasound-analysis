ssaplot = function(groups,data,plot="main",groupvar="word",color=TRUE,points=FALSE, xlims=c(200,600), ylims=c(140,320)){
  # clunky thing to allow varying column names:
  #arguments = as.list(match.call()[-1])
  
  # important packages
  require(ggplot2)
  require(gss)
  cbPalette <- c("#009E73", "#0072B2", "#D55E00", "#CC79A7","#999999", "#56B4E9")
  greys <- c("#000000","#999999", "#7a7974")
  
  # subset data
  word = data[[groupvar]]
  X = data[["X"]]
  Y = data[["Y"]]
  sub_data = as.data.frame(cbind(X,Y))
#   if(points==TRUE){
#     sub_data$rep = as.factor(data$repetition)
#   }
  sub_data$word = data[[groupvar]]
  
  sub_data <-droplevels(subset(sub_data,word%in%groups))
  
  # fit model
  ssa.fit <-ssanova(Y~word+X+word:X,data=sub_data)
  
  # predict new values:
  
  # Generating a grid of values as prediction points:
  # set up X-values
  X=seq(min(sub_data$X),max(sub_data$X),by=0.01)
  # add factor values:
  grid <- expand.grid(X=X ,word = groups)
  
  # Branches here depending on what you're trying to look at:
  
  # Main effects plot:
  if(plot=="main"){
    # predict values:
    grid$ssa.fit <- predict(ssa.fit,newdata = grid,se = T)$fit
    grid$ssa.SE <- predict(ssa.fit,newdata = grid,se = T)$se.fit
    
    # set up plot:
    
    comparison <- ggplot(grid,aes(x = X,colour = word,group = word))+ theme_bw() + coord_cartesian(xlim=xlims,ylim=ylims)
    if(color==TRUE){
      comparison <- comparison + scale_fill_manual(values=cbPalette) + scale_color_manual(values=cbPalette)
    }
    if(color==FALSE){
      comparison <- comparison + scale_fill_manual(values = greys)
    }
    comparison<-comparison + geom_line(aes(y = ssa.fit),alpha = 1,colour = "grey20")
    comparison<-comparison + geom_ribbon(aes(ymin = ssa.fit-(1.96*ssa.SE), ymax = ssa.fit+(1.96*ssa.SE),fill = word ),alpha = 0.65,colour = "NA")
    
    if(points==TRUE){
      comparison <- comparison+geom_point(data= sub_data, aes(x=jitter(X),y=Y),alpha = 0.5)
    }
    #flip the Y axis
    comparison<-comparison + scale_y_reverse()
    # labels
    comparison<-comparison + ylab("y")
    comparison
  }
  
  else if(plot=="interaction"){
    grid$Fit <- predict(ssa.fit,grid,se = T,inc = c("word","word:X"))$fit
    grid$SE <- predict(ssa.fit,grid,se = T,inc = c("word","word:X"))$se.fit
    inter <- ggplot(grid,aes(x=X))+theme_bw()
    inter <- inter + geom_line(aes(y = Fit))
    inter <- inter + geom_ribbon(aes(ymax = Fit+(1.96*SE),ymin = Fit -(1.96*SE)),alpha=0.75)
    inter <- inter + facet_wrap(~word)
    inter <- inter + geom_hline(y = 0,lty = 2)
    inter <- inter + ylab("y")
    inter
  }
  else print("Don't understand the plot option")
  
}