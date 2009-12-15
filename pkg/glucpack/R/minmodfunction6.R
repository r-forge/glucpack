minmod<-function(times,conc,i,starta,id=1,weight){
    require(odesolve)
    require(minpack.lm)
    D=75                                    #assumed contants
    V=1.34
    Sg=.028
    f=.5
    Gb=conc[1]
    Ib=i[1]
    weight=weight/2.2;
    garea=(D*1000*f)/weight
	cat("Effective Dose: ")
	cat(garea);
	cat("\n")

##################################################################################
                                        #create data
    data=cbind(times,conc,i)
    data=as.data.frame(data)
    a1=starta[1];
    a2=starta[2];
    a3=starta[3];
    
	atimes=c(0,30,60,90,120)
################################################################################
                                        #insulin function
    Insulin=approxfun(times,i,rule=2)
#################################################################################

                                        #get a5 by integrating and standardizing the area
    geta4<-function(a1,a2,a3){

	area1=.5*(atimes[2]-atimes[1])*a1;
	area2=.5*(a1+a2)*(atimes[3]-atimes[2]);
	area3=.5*(a2+a3)*(atimes[4]-atimes[3]);
	area90=area1+area2+area3;
	missing=garea-area90;
	a4=(2*(missing/(atimes[5]-atimes[4])))-a3
	a4
    }
################################################################################
                                        #ra meal profile function. If you are at a time point,
                                        #pick that else, find the interval and interpolate

    ra<-function(a1,a2,a3,t){
	a4=geta4(a1,a2,a3)
	a=c(0,a1,a2,a3,a4)
	if(t<=0){0}else{
            if(t>120){
                a4*exp(-.017*(t-120))}else{
                    if(t%in%atimes){a[which(atimes==t)]}else{
                        io=which(atimes==rev(atimes[atimes<=t])[1])
                        i=io+1
                        a[io]+((a[i]-a[io])/(atimes[i]-atimes[io]))*(t-atimes[io])
                    }
                }
	}
    }
###################################################################################
                                        #minimal model
    model2 <- function(t, Y, parameters) {
	with(as.list(parameters),{
            dy1=-(Sg+Y[2])*Y[1]+Sg*Gb+ra(a1,a2,a3,t)/V
            dy2=(-p2)*Y[2]+p2*Si*(Insulin(t)-Ib)
            list(c(dy1, dy2))
	})
    }

#################################################################################
    fit2<-function(par){
	temp=conc-(lsoda(c(Gb,0), times, model2, parms=c(p2=par[["p2"]],Si=par[["Si"]],
                                                 a1=par[["a1"]],a2=par[["a2"]],a3=par[["a3"]]))[,2])
    }

    trial=nls.lm(par=c(p2 = .012, Si = .001062,a1=starta[1],a2=starta[2],a3=starta[3]),fn=fit2)
    test=lsoda(c(Gb,0), 0:120, model2, trial[1]$par)
    plot(0:120,test[,2],type="l",main="Estimated Glucose",xlab="min",ylab="glucose")
    points(times,conc)
	
    text=paste("insulin Sens.= ",V*10^4*trial[1]$par[2],sep="")
    mtext(text, side = 3, line = 0)
    trial    
}

####################################################################################

