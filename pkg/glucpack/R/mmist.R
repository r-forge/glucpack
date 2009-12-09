AUC<-function(x,times){
    ix=approxfun(times,x,rule=2)
    area1=integrate(ix,times[1],times[length(times)])
    area1
}


AUC2<-function(x,times){
    x=x-x[1];
    ix=approxfun(times,x,rule=2)
    area=integrate(ix,times[1],times[length(times)])
    area
}


AUC3<-function(x,times){
    x=x-x[1];
    ix=approxfun(times,x,rule=2)
    arr=ix(0:120)
    ix2=approxfun(0:120,abs(arr),rule=2)
    area=integrate(ix2,0,120)
    area
}


AUCI<-function(x,times){
    (as.numeric(AUC3(x,times)[1])+as.numeric(AUC2(x,times)[1]))/2
}

AUCRATIO<-function(x,y,times){
    g=as.numeric(AUC(x,times)[1])
    i=as.numeric(AUC(y,times)[1])
    g/i
}

AUCIRATIO<-function(x,y,times){
    g=as.numeric(AUCI(x,times)[1])
    i=as.numeric(AUCI(y,times)[1])
    g/i
}

INS120<-function(y,times){
    ind=which(times==120)
    y[ind]
}

MATSUDA<-function(x,y,times){
	meanx=mean(x);
	meany=mean(y);
	
    10000/(sqrt((y[1]*x[1])*(meany*meanx)))
}

ISI120<-function(x,y,bw,times){
	meanx=mean(x);
	meany=mean(y);

    (75000+(x[1]-x[length(x)]*.19*bw))/(120*meanx*log(meany))
}


OGIS<-function(height,weight,g0,g90,g120,i0,i90,p1=650,p2=325,p3=81.3*10^3,p4=132,p5=652*10^(-6),p6=173,gclamp=90,V=10^4){
	#weight kg, height m, glucose load set to 75 grams
    BSA=0.20247 * height^(.725) * weight^(.425);
    dose=75/BSA;
    clogtt=p4*(((((p1*dose-V*(g120-g90)/60)/g90)+(p3/g0)))/(i90-i0+p2))
    b=(p5*(g90-gclamp)+1)*clogtt
    cleu=.5*(b+sqrt(b^2+4*p5*p6*(g90-gclamp)*clogtt))
    cleu
}

HOMA<-function(g0,i0){
	homa=(g0*i0)/405;
	homa
}

ISIBEL<-function(x,y,times,c){
	meanx=mean(x);
	meany=mean(y);

    bel=2/(((meanx*meany)/c)+1)
    bel
}

QUICKI<-function(g0,i0){
	quicki=1/(log(g0)+log(i0))
	quicki
}
