minmod<-function(times,conc,i,starta,id=1){
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
    a3=starta[3];
    a4=starta[4];
    a5=starta[5];

################################################################################
                                        #insulin function
    Insulin=approxfun(times,i,rule=2)
#################################################################################

                                        #get a6 by integrating and standardizing the area
    geta6<-function(a1,a2,a3,a4,a5){
	area1=.5*(times[2]-times[1])*a1;
	area2=.5*(a1+a2)*(times[3]-times[2]);
	area3=.5*(a2+a3)*(times[4]-times[3]);
	area4=.5*(a3+a4)*(times[5]-times[4]);
	area5=.5*(a4+a5)*(times[6]-times[5]);
	area90=area1+area2+area3+area4+area5;
	missing=500-area90;
	a6=(2*(missing/(times[7]-times[6])))-a5
	a6
    }
################################################################################
                                        #ra meal profile function. If you are at a time point,
                                        #pick that else, find the interval and interpolate

    ra<-function(a1,a2,a3,a4,a5,t){
	a6=geta6(a1,a2,a3,a4,a5)
	a=c(0,a1,a2,a3,a4,a5,a6)
	if(t<=0){0}else{
            if(t>120){
                a6*exp(-.017*(t-120))}else{
                    if(t%in%times){a[which(times==t)]}else{
                        io=which(times==rev(times[times<=t])[1])
                        i=io+1
                        a[io]+((a[i]-a[io])/(times[i]-times[io]))*(t-times[io])
                    }
                }
	}
    }
###################################################################################
                                        #minimal model
    model2 <- function(t, Y, parameters) {
	with(as.list(parameters),{
            dy1=-(Sg+Y[2])*Y[1]+Sg*Gb+ra(a1,a2,a3,a4,a5,t)/V
            dy2=(-p2)*Y[2]+p3*(Insulin(t)-Ib)
            list(c(dy1, dy2))
	})
    }

#################################################################################
    fit2<-function(par){
	temp=conc-(lsoda(c(Gb,0), times, model2, parms=c(p2=par[["p2"]],p3=par[["p3"]],
                                                 a1=par[["a1"]],a2=par[["a2"]],a3=par[["a3"]],
                                                 a4=par[["a4"]],a5=par[["a5"]]))[,2])
    }

    trial=nls.lm(par=c(p2 = .03093, p3 = .00001062,a1=starta[1],a2=starta[2],a3=starta[3],a4=starta[4],a5=starta[5]),fn=fit2)
    test=lsoda(c(Gb,0), 0:120, model2, trial[1]$par)
    plot(0:120,test[,2],type="l",main="Simulated Glucose",xlab="min",ylab="glucose")
    points(times,conc)
    si=1.7*(trial$par[2]/trial$par[1])
    text=paste("insulin Sens.= ",si,sep="")
    mtext(text, side = 3, line = 0)
    trial
}

####################################################################################

