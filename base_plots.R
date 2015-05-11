with(airquality,plot(Wind,Ozone),
     title(main="Ozone ans wind in New York City"),type="n")
with(subset(airquality,Month==5),points(Wind,Ozone,col="blue"))
with(subset(airquality,Month!=5),points(Wind,Ozone,col="red"))
legend("topright",pch=1,col=c("blue","red"),legend=c("May","not May"))

with(airquality,plot(Wind,Ozone),
     title(main="Ozone ans wind in New York City"),pch=20)
model<-lm(Ozone~Wind,airquality)
abline(model,lwd=2)

par(mfrow=c(1,2))
with(airquality,{
        plot(Wind,Ozone,main="Ozone and wind")
        plot(Solar.R,Ozone,main="Ozone and Solar Radiation")
})
     