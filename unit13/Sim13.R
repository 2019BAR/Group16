library(manipulate)
manipulate({
  payoff = matrix(c(TN,FN,FP,TP),2,2)
  cutoff = seq(0, 1, 0.01)
  result = sapply(cutoff, function(p) sum(
    table(factor(y==1, c(F,T)), factor(pred>p, c(F,T))) # confusion matrix
    * payoff ))
  i = which.max(result)
  par(cex=0.7)
  plot(cutoff, result, type='l', col='cyan', lwd=2, main=sprintf(
    "Optomal Expected Result: $%d @ %.2f",result[i],cutoff[i]))
  abline(v=seq(0,1,0.1),h=seq(-6000,0,100),col='lightgray',lty=3)
  points(cutoff[i], result[i], pch=20, col='red', cex=2)
},
TN = slider(-100,0,   0,step=5),
FN = slider(-100,0,-100,step=5),
FP = slider(-100,0, -10,step=5),
TP = slider(-100,0, -50,step=5)
) 