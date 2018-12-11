
run = seq(1,18)
Res = c(137.75,147.75,157.75,187.75,137.75,117.75,
147.75,147.75,147.75,177.75,157.75,107.75,
167.75,157.75,117.75,177.75,147.75,117.75)
spec = seq(1,6)
#postscript("u:/meth1/lectures/chemplot.ps",horizontal=F)
plot(run,Res,type="p",xlab="Operators",ylab="Chemical Level",
        main="Figure 4: Variation Due Only to R(S,O)",cex=.99,
        ylim=c(100,200),xaxt="n")
rect(0.75,136.6,3.25,160)
text(1,134,"R1",cex=.55)
text(2,134,"R2",cex=.55)
text(3,134,"R3",cex=.55)
text(2,131,"SPEC1",cex=.75)

rect(3.75,116.6,6.25,190)
text(4,114,"R4",cex=.55)
text(5,114,"R5",cex=.55)
text(6,114,"R6",cex=.55)
text(5,111,"SPEC2",cex=.75)

rect(6.75,141.6,9.25,155)
text(7,139,"R7",cex=.55)
text(8,139,"R8",cex=.55)
text(9,139,"R9",cex=.55)
text(8,136,"SPEC3",cex=.75)

rect(9.75,106.6,12.25,180)
text(10,104,"R10",cex=.55)
text(11,104,"R11",cex=.55)
text(12,104,"R12",cex=.55)
text(11,101,"SPEC4",cex=.75)

rect(12.75,116.6,15.25,170)
text(13,114,"R13",cex=.55)
text(14,114,"R14",cex=.55)
text(15,114,"R15",cex=.55)
text(14,111,"SPEC5",cex=.75)


rect(15.75,116.6,18.25,180)
text(16,114,"R16",cex=.55)
text(17,114,"R17",cex=.55)
text(18,114,"R18",cex=.55)
text(17,111,"SPEC6",cex=.75)

axis(side=1,at=c(3.5,9.5,15.5),
labels = c("Operator 1","Operator 2","Operator 3"))
segments(0,147.5,19,147.5)
#graphics.off()

