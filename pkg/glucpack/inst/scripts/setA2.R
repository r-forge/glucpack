require(tcltk)
library(tkrplot)


#Setup vars
clusts=c()
tt <- tktoplevel()
a=c(5.3,4.7,4.5,4,4.5);
times=c(0,10,20,30,60,90,120)

curx=50;
profilematrix=matrix(ncol=20,nrow=20);
ymax=100
ymin=0
b=c(10,20,30,60,90,120)
m=0;
#Original Plot
hscale <- 1.5
vscale <- 1

plotFunction <- function()
{
    params <- par(bg="white")
    x <- b[1:5]
    y <- a[1:5]*10
    plot(x,y,main="StartA Profile",type="b",ylim=c(ymin,ymax),xlab="Time in Minutes", ylab="Ra*10")
    par(params)
	usrs<<- par("usr")
	plts<<-par("plt")
    text(60,100,labels=m)
}


#functions
refresher<-function(){eval(tkrreplot(img))}

LoadCluster<-function(){cchoice=tkcurselection(tl);
			clustselect=as.character(tkget(tl,cchoice))
			a<<-clusts[[clustselect]]
			refresher()
			}

SaveCluster<-function(){
			clusts[[name]]<<-a;
			tkinsert(tl,"end",ind)
			ind<<-ind+1;
			refresher()
			}

ClearCluster<-function(){
    for(i in 1:length(a)){
        a[i]<<- ((ymax+ymin)/20)}
    refresher()
}

DeleteCluster <- function(){
    			cchoice=tkcurselection(tl);
    			tkdelete(tl,cchoice)
}

Closestx <- function(x,y){
	winwidth<- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
	widmin<-as.numeric(winwidth*plts[1])
	widmax<-as.numeric(winwidth*plts[2])
	x=as.numeric(x)

	if(x<=widmax&&x>=widmin){
	     span=widmax-widmin;
	     spanxs=as.numeric(usrs[2])-as.numeric(usrs[1])
	     tempx=as.numeric(usrs[1])+(x-widmin)*spanxs/(span)
   	     z=round(tempx)
	     disttemp=(b-z)^2
	     z=which(disttemp==min(disttemp))
	     curx<<-z;
	     tkconfigure(img,cursor="hand1")
         }else{curx<<-0}

    }
Newy<-function(x,y){
    if(curx>0){
	winheight<-as.numeric(tclvalue(tkwinfo("reqheight",img)))
	heightmin<-as.numeric(winheight*plts[3])
	heightmax<-as.numeric(winheight*plts[4])
	y=as.numeric(y)
	y=winheight-y
	span=heightmax-heightmin
	spanys=as.numeric(usrs[4])-as.numeric(usrs[3])
	tempy=as.numeric(usrs[3])+(y-heightmin)*spanys/(span)
	z=round(tempy)
	if(z>=ymin&&z<=ymax){
            z=z/10;
            a[curx]<<-z;
            m<<-paste("a6= ",round(geta6(a[1],a[2],a[3],a[4],a[5]),1),sep="")
            eval(tkrreplot(img))

        }
	tkconfigure(img,cursor="hand2")
    }
}

importCluster <- function(){
    val<<-tclvalue(loadvar);
    a<<-eval(as.name(val));
    refresher()
}

randomCluster<-function(){
    sd=(ymax-ymin)
    sd2=5
    sd3=1
    a<<-rnorm(5,sd2,sd3)
    a<<-a%%(ymax+1)
    refresher()
}
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
plot.new()
par(mfrow=c(3,3))







dataGen<-function(){

    a[6]<<-geta6(a[1],a[2],a[3],a[4],a[5])
    if(a[6]>=0){
    trytest(a)}
    else{cat("Error: a6<0.  Please reduce A1-A5 to allow area for A6\n")}


}




configdialog<-function(){
    doneConfig <- function()

    {	a<<-"";
        mid=(as.numeric(tclvalue(newymin))+as.numeric(tclvalue(newymax)))/2
        for(i in 1:as.numeric(tclvalue(numx))){a[i]<<-mid}
        val<<-tclvalue(times);
        b<<-eval(as.name(val));
        ymin<<-as.numeric(tclvalue(newymin))
        ymax<<-as.numeric(tclvalue(newymax))
        refresher()
        tkdestroy(config)
        tkgrab.release(config)
        tkdestroy(config)
        tkfocus(tt)
    }

    config<-tktoplevel();
    tkwm.deiconify(config)
    tkgrab.set(config)
    tkfocus(config)

    tkgrid(tklabel(config,text="Number of Timepoints"),column=1,row=1)
    numx<-tclVar("");
    entry1 <- tkentry(config,width=10,textvariable=numx);
    tkgrid(entry1,column=2,row=1)

    tkgrid(tklabel(config,text="Variable Holding Timepoints"),column=1,row=2)
    times<-tclVar("");
    entry3 <- tkentry(config,width=10,textvariable=times);
    tkgrid(entry3,column=2,row=2)

    tkgrid(tklabel(config,text="Ymin"),column=1,row=3)
    newymin<-tclVar("");
    entry4a <- tkentry(config,width=10,textvariable=newymin);
    tkgrid(entry4a,column=2,row=3)

    tkgrid(tklabel(config,text="Ymax"),column=1,row=4)
    newymax<-tclVar("");
    entry4b <- tkentry(config,width=10,textvariable=newymax);
    tkgrid(entry4b,column=2,row=4)

    tkgrid(done.b <- tkbutton(config,text="Done", command=doneConfig),column=1,row=5, columnspan=2)


}


nameCluster<-function(){
    doneCname <- function()

    {
        clusts[[tclvalue(newcname)]]<<-a;
        tkinsert(tl,"end",tclvalue(newcname))
        tkdestroy(cname)
        tkgrab.release(cname)
        tkdestroy(cname)
        tkfocus(tt)

    }


    cname<-tktoplevel();
    tkwm.deiconify(cname)
    tkgrab.set(cname)
    tkfocus(cname)

    tkgrid(tklabel(cname,text="Cluster Name"),column=1,row=1)
    newcname<-tclVar("");
    entryc <- tkentry(cname,width=10,textvariable=newcname);
    tkgrid(entryc,column=2,row=1)
    tkgrid(done.b <- tkbutton(cname,text="OK", command=doneCname),column=1,row=2, columnspan=2)


}


                                        #set up grid

img <- tkrplot(tt,fun=plotFunction,hscale=hscale,vscale=vscale)
tkgrid(img,row=1,column=1,columnspan=20,rowspan=6)
tl<-tklistbox(tt,height=10,selectmode="single",background="white")
tkgrid(tklabel(tt,text="Saved Profiles"),column=21,row=1,columnspan=2)
tkgrid(tl,row=2,column=21, columnspan=2)

tkgrid(but.w <- tkbutton(tt,text="Clear", command=ClearCluster),column=5,row=7)
tkgrid(but.w <- tkbutton(tt,text="Save", command=nameCluster),column=9,row=7)
loadvar<- tclVar("")
tkgrid(but.w <- tkbutton(tt,text="Random", command=randomCluster),column=13,row=7)
ebox <-tkentry(tt,width="10",textvariable=loadvar)
tkgrid(ebox,column=17,row=7)
tkgrid(but.w <- tkbutton(tt,text="Load Variable", command=importCluster),column=18,row=7)

loadbutton<-tkbutton(tt,text="Load",command=LoadCluster)
tkgrid(loadbutton,column=21,row=3)
delbutton<-tkbutton(tt,text="Delete",command=DeleteCluster)
tkgrid(delbutton,column=22,row=3)
impbutton<-tkbutton(tt,text="Import",command=LoadCluster)
tkgrid(impbutton,column=21,row=4)
impbutton<-tkbutton(tt,text="Export",command=LoadCluster)
tkgrid(impbutton,column=22,row=4)
impbutton<-tkbutton(tt,text="Configure",command=configdialog)
tkgrid(impbutton,column=21,row=5)
impbutton<-tkbutton(tt,text="BruteForce",command=LoadCluster)
tkgrid(impbutton,column=22,row=5)
genbutton<-tkbutton(tt,text="Simulate",command=dataGen)
tkgrid(genbutton,column=21,row=6, columnspan=2)


#keybindings
tkbind(tt,"<ButtonPress>",Closestx);
tkbind(tt,"<ButtonRelease>",Newy);
tkconfigure(img,cursor="hand2")
#tkfocus(tt)

