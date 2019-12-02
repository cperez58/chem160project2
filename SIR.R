alpha_m<-0.0000006 #(person-1day-1)
alpha_f<-0.0000009
gamma_m<-0.05
gamma_f<-0.007 #(day-1)
Sm<-14000
Sf<-9000
Im<-1000
If<-1000
Sm.hist<-c()
Sf.hist<-c()
Im.hist<-c()
If.hist<-c()
for (day in 1:2000) {
    S.hist[day]<-S
    Sm.hist[day]<-Sm
    Sf.hist[day]<-Sf
    Im.hist[day]<-Im
    If.hist[day]<-If
    delta.Sm<-(gamma_m)*Im-(alpha_m)*Sm*If
    delta.Im<-(alpha_m)*Sm*If-(gamma_m)*Im
    delta.Sf<-(gamma_f)*If-(alpha_f)*Sf*Im
    delta.If<-(alpha_f)*Sf*Im-(gamma_f)*If
    Sm<-Sm+delta.Sm
    Im<-Im+delta.Im
    Sf<-Sf+delta.Sf
    If<-If+delta.If
    Sm<-max(Sm,0) 
    Sf<-max(Sf,0) 
    Im<-max(Im,0)
    If<-max(If,0)
}
plot(Sm.hist,type="l",ylim=c(0,2000))
lines(Im.hist,col=1)
lines(If.hist,col=2)
lines(Sf.hist,col=3)
