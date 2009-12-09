minmod3<-function(times,conc,i,starta,id=1){
    require(odesolve)
    require(minpack.lm)
                                        #assumed contants
    V=1.7
    Sg=.014
    f=.5
    Gb=conc[1]
    Ib=i[1]


##################################################################################
#create data
    data=cbind(times,conc,i)
    data=as.data.frame(data)
    a1=starta[1];
    a2=starta[2];

################################################################################
#insulin function
    Insulin=approxfun(times,i,rule=2)
#################################################################################

################################################################################
#ra meal profile function. If you are at a time point,
#pick that else, find the interval and interpolate

    ra<-function(a1,a2,t){
	tps=c(0,40,120)
	a=c(0,a1,a2)
	if(t<=0){0}else{
            if(t>120){
                a2*exp(-.017*(t-120))}else{
                    if(t%in%tps){a[which(tps==t)]}else{
                        io=which(tps==rev(tps[tps<=t])[1])
                        i=io+1
                        a[io]+((a[i]-a[io])/(tps[i]-tps[io]))*(t-tps[io])
                    }
                }
	}
    }
###################################################################################
                                        #minimal model
    model2 <- function(t, Y, parameters) {
	with(as.list(parameters),{
            dy1=-(Sg+Y[2])*Y[1]+Sg*Gb+ra((a1)^2,(a2^2),t)/V
            dy2=(-(p2^2))*Y[2]+(p3^2)*(Insulin(t)-Ib)
            list(c(dy1, dy2))
	})
    }

#################################################################################
    fit2<-function(par){
	temp=conc-(lsoda(c(Gb,0), times, model2, parms=c(p2=par[["p2"]],p3=par[["p3"]],
                                                 a1=par[["a1"]],a2=par[["a2"]]))[,2])
    }

    trial=nls.lm(par=c(p2 = sqrt(.03093), p3 = sqrt(.02062),a1=starta[1],a2=starta[2]),fn=fit2)
    test=lsoda(c(Gb,0), 0:120, model2, trial[1]$par)
    par(mfrow=c(3,1))

    plot(0:120,test[,2],type="l",main="Glucose",xlab="min",ylab="glucose")
    points(times,conc)
    si=1.7*(trial$par[2]^2/trial$par[1]^2)
    text=paste("insulin Sens.= ",si,sep="")
    mtext(text, side = 3, line = 0)

    plot(0:120,Insulin(0:120),type="l",col="blue",main="Int. Insulin",xlab="min",ylab="Insulin")

    ras=sapply(0:120,function(x){ra(trial$par[3],trial$par[4],x)})
    plot(0:120,ras,col="green",type="l",main="Meal Profile (Ra)",xlab="min",ylab="Ra")
    trial
}

####################################################################################

