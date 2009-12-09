runnum<-function(fitter=minmod3log,starta=c(9,6,2)){
    fitter(glucdata[[x]]$times,glucdata[[x]]$gluc,glucdata[[x]]$ins,starta)
}
