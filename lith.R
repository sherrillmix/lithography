plotLith<-function(file='lith.pdf',width=1,height=1,buffer=.05,holeDiameter=.1,nLine=70,mainLwd=30,constrictWidth=2,constrictLength=30,constrictRamp=constrictLength*3){
  #convert microns to inches
  mi2in<-3.93701e-5
  mainLwdIn<-mainLwd*mi2in
  constrictWidthIn<-constrictWidth*mi2in
  #cairo_pdf to avoid resolution issues
  cairo_pdf(file,width=width/mi2in/90,height=height/mi2in/90)
    #no margins
    par(mar=c(0,0,0,0))
    #empty plot
    plot(1,1,type='n',xlab='',ylab='',ylim=c(0,height),xlim=c(0,width),bty='n',xaxt='n',yaxt='n',xaxs='i',yaxs='i')
    #hole positions
    topHole<-c(width/2,height-buffer-holeDiameter/2)
    bottomHole<-c(width/2,buffer+holeDiameter/2)
    #top hole
    plotrix::draw.circle(topHole[1],topHole[2],radius=holeDiameter/2,col='black')
    #bottom hole
    plotrix::draw.circle(bottomHole[1],bottomHole[2],radius=holeDiameter/2,col='black')
    #line coordinates
    linePos<-seq(buffer,width-buffer,length.out=nLine)
    circlePos<-seq(topHole[1]-holeDiameter/2.1,topHole[1]+holeDiameter/2.1,length.out=nLine)
    lineTop<-buffer+(height-buffer*2)*.8
    lineBottom<-buffer+(height-buffer*2)*.2
    lineMid<-(lineTop+lineBottom)/2
    constrictBottom<-lineMid-(constrictLength+constrictRamp*2)*mi2in
    topRampBottom<-lineMid-constrictRamp*mi2in
    bottomRampTop<-lineMid-(constrictLength+constrictRamp)*mi2in
    #top to mid
    rect(linePos-mainLwdIn/2,lineMid,linePos+mainLwdIn/2,lineTop,col='black',border=NA)
    #bottom to mid
    rect(linePos-mainLwdIn/2,constrictBottom,linePos+mainLwdIn/2,lineBottom,col='black',border=NA)
    #just extend whole way to reduce connection problems
    rect(linePos-constrictWidthIn/2,lineMid,linePos+constrictWidthIn/2,constrictBottom,col='black',border=NA)
    #top ramp
    polyDf<-do.call(rbind,lapply(linePos,function(xx)data.frame('x'=c(xx-mainLwdIn/2,xx+mainLwdIn/2,xx+constrictWidthIn/2,xx-constrictWidthIn/2,NA),'y'=c(constrictBottom,constrictBottom,bottomRampTop,bottomRampTop,NA))))
    polygon(polyDf$x,polyDf$y,col='black',border=NA)
    #bottom ramp
    polyDf<-do.call(rbind,lapply(linePos,function(xx)data.frame('x'=c(xx-mainLwdIn/2,xx+mainLwdIn/2,xx+constrictWidthIn/2,xx-constrictWidthIn/2,NA),'y'=c(lineMid,lineMid,topRampBottom,topRampBottom,NA))))
    polygon(polyDf$x,polyDf$y,col='black',border=NA)
    #top connectors
    #polyDf<-do.call(rbind,lapply(linePos,function(xx){
        #slope<-c(xx-mainLwdIn/2-topHole[1]+mainLwdIn/2,lineTop-topHole[2]+holeDiameter/2.1)
        #perp<-rev(slope)*c(1,-1)
        #offset<--perp/sqrt(sum(perp^2))*mainLwdIn
        #data.frame('x'=c(xx-mainLwdIn/2,xx+mainLwdIn/2,topHole[1]+offset[1],topHole[1]+mainLwdIn/2,NA),'y'=c(lineTop,lineTop,topHole[2]+holeDiameter/2.1+offset[2],topHole[2]+holeDiameter/2.1,NA))
    #}))
    #polygon(polyDf$x,polyDf$y,col='black',border=NA)
    polygon(c(topHole[1],max(linePos)+mainLwdIn/2,min(linePos)-mainLwdIn/2),c(topHole[2]+holeDiameter/2,lineTop,lineTop),col='black',border=NA)
    #bottom connectors
    #polyDf<-do.call(rbind,lapply(linePos,function(xx){
        #slope<-c(bottomHole[1]+mainLwdIn/2-xx-mainLwdIn/2,bottomHole[2]-holeDiameter/2.1-lineBottom)
        #perp<-rev(slope)*c(1,-1)
        #offset<--perp/sqrt(sum(perp^2))*mainLwdIn
        #data.frame('x'=c(xx-mainLwdIn/2,xx+mainLwdIn/2,bottomHole[1]+offset[1],bottomHole[1]+mainLwdIn/2,NA),'y'=c(lineBottom,lineBottom,bottomHole[2]-holeDiameter/2.1+offset[2],bottomHole[2]-holeDiameter/2.1,NA))
    #}))
    #polyDf<-do.call(rbind,lapply(linePos,function(xx)data.frame('x'=c(xx-mainLwdIn/2,xx+mainLwdIn/2,bottomHole[1]+mainLwdIn/2,bottomHole[1]-mainLwdIn/2,NA),'y'=c(lineBottom,lineBottom,bottomHole[2]-holeDiameter/2.1,bottomHole[2]-holeDiameter/2.1,NA))))
    #polygon(polyDf$x,polyDf$y,col='black',border=NA)
    polygon(c(bottomHole[1],max(linePos)+mainLwdIn/2,min(linePos)-mainLwdIn/2),c(bottomHole[2]-holeDiameter/2,lineBottom,lineBottom),col='black',border=NA)
  dev.off()
}
#open in inkscape and save with base unit px
plotLith()
