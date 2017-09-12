library(showtext)
mi2in<-3.93701e-5
plotLith<-function(width=1.1,height=1.1,buffer=.1,holeDiameter=.1,nLine=200,mainLwd=30,constrictWidth=2,constrictLength=30,constrictRamp=constrictLength*3,title='',titleCex=100){
  #convert microns to inches
  mainLwdIn<-mainLwd*mi2in
  constrictWidthIn<-constrictWidth*mi2in
  #cairo_pdf to avoid resolution issues
  #cairo_pdf(file,width=width/mi2in/90,height=height/mi2in/90)
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
    polyDf<-do.call(rbind,mapply(function(xx,yy,lineWidth)data.frame('x'=c(xx-lineWidth/2,xx+lineWidth/2,xx+yy/2,xx-yy/2,NA),'y'=c(constrictBottom,constrictBottom,bottomRampTop,bottomRampTop,NA)),linePos,constrictWidthIn,mainLwdIn,SIMPLIFY=FALSE))
    polygon(polyDf$x,polyDf$y,col='black',border=NA)
    #bottom ramp
    polyDf<-do.call(rbind,mapply(function(xx,yy,lineWidth)data.frame('x'=c(xx-lineWidth/2,xx+lineWidth/2,xx+yy/2,xx-yy/2,NA),'y'=c(lineMid,lineMid,topRampBottom,topRampBottom,NA)),linePos,constrictWidthIn,mainLwdIn,SIMPLIFY=FALSE))
    polygon(polyDf$x,polyDf$y,col='black',border=NA)
    #top connectors
    #polyDf<-do.call(rbind,lapply(linePos,function(xx){
        #slope<-c(xx-mainLwdIn/2-topHole[1]+mainLwdIn/2,lineTop-topHole[2]+holeDiameter/2.1)
        #perp<-rev(slope)*c(1,-1)
        #offset<--perp/sqrt(sum(perp^2))*mainLwdIn
        #data.frame('x'=c(xx-mainLwdIn/2,xx+mainLwdIn/2,topHole[1]+offset[1],topHole[1]+mainLwdIn/2,NA),'y'=c(lineTop,lineTop,topHole[2]+holeDiameter/2.1+offset[2],topHole[2]+holeDiameter/2.1,NA))
    #}))
    #polygon(polyDf$x,polyDf$y,col='black',border=NA)
    polygon(c(topHole[1],max(linePos)+max(mainLwdIn,na.rm=TRUE)/2,min(linePos)-max(mainLwdIn,na.rm=TRUE)/2),c(topHole[2]+holeDiameter/2,lineTop,lineTop),col='black',border=NA)
    #bottom connectors
    #polyDf<-do.call(rbind,lapply(linePos,function(xx){
        #slope<-c(bottomHole[1]+mainLwdIn/2-xx-mainLwdIn/2,bottomHole[2]-holeDiameter/2.1-lineBottom)
        #perp<-rev(slope)*c(1,-1)
        #offset<--perp/sqrt(sum(perp^2))*mainLwdIn
        #data.frame('x'=c(xx-mainLwdIn/2,xx+mainLwdIn/2,bottomHole[1]+offset[1],bottomHole[1]+mainLwdIn/2,NA),'y'=c(lineBottom,lineBottom,bottomHole[2]-holeDiameter/2.1+offset[2],bottomHole[2]-holeDiameter/2.1,NA))
    #}))
    #polyDf<-do.call(rbind,lapply(linePos,function(xx)data.frame('x'=c(xx-mainLwdIn/2,xx+mainLwdIn/2,bottomHole[1]+mainLwdIn/2,bottomHole[1]-mainLwdIn/2,NA),'y'=c(lineBottom,lineBottom,bottomHole[2]-holeDiameter/2.1,bottomHole[2]-holeDiameter/2.1,NA))))
    #polygon(polyDf$x,polyDf$y,col='black',border=NA)
    polygon(c(bottomHole[1],max(linePos)+max(mainLwdIn,na.rm=TRUE)/2,min(linePos)-max(mainLwdIn,na.rm=TRUE)/2),c(bottomHole[2]-holeDiameter/2,lineBottom,lineBottom),col='black',border=NA)
    if(title!=''){
      showtext.begin()
      text(rep(c(topHole[1],bottomHole[1]),each=2)*c(.5,1.5),rep(c(topHole[2],bottomHole[2]),each=2),title,cex=titleCex,font=2)
      showtext.end()
    }
    return(invisible(list('lines'=linePos,'mid'=lineMid,'top'=topHole,'bottom'=bottomHole)))
  #dev.off()
}
#open in inkscape and save with base unit px
#cairo_pdf('lithMulti.pdf',width=3/mi2in/90,height=3/mi2in/90)

interleave<-function(xx,yy){
  n<-max(length(xx),length(yy))
  out<-2*n
  out[(1:n)*2-1]<-xx
  out[(1:n)*2]<-yy
  return(out)
}

#save as dxf 12 scale by 100x
cairo_pdf('lithMulti.pdf',width=3/mi2in/100,height=3/mi2in/100)
  layout(matrix(c(0,1,1,2,2,0,3,3,4,4,5,5,0,6,6,7,7,0),ncol=3))
  plotLith(constrictWidth=2,title="2um")
  plotLith(constrictWidth=3,title="3um")
  plotLith(constrictWidth=4,title="4um")
  plotLith(constrictWidth=5,title="5um")
  plotLith(constrictWidth=6,title="6um")
  plotLith(constrictWidth=9,title="9um")
  testWidths<-seq(1,30,.25)
  widths<-rep(interleave(testWidths,NA),rep(c(2,1),length(testWidths)))
  coords<-plotLith(constrictWidth=widths,nLine=length(widths),mainLwd=30*widths/widths,title='Mixed')
  text(coords[['lines']]+min(diff(coords[['lines']]))*.5,ifelse(rep(1:(length(widths)/length(testWidths)),length.out=length(testWidths))==5,coords[['mid']],NA),as.character(widths),cex=10,srt=90,font=2)
dev.off()


spiralCoords<-function(x1,y1,x2,y2,rotations=1,nv=100,width=.01){
  start<-atan2(y1-y2,x1-x2)
  startRadius<-sqrt((x1-x2)^2+(y1-y2)^2)
  angles<-seq(start,start+2*pi*rotations,length.out=nv)
  radius<-seq(startRadius,0,length.out=nv)
  xs<-x2+cos(angles)*radius
  ys<-y2+sin(angles)*radius
  xs2<-x2+cos(angles)*(width+radius)
  ys2<-y2+sin(angles)*(width+radius)
  #plot(x,y)
  #points(x1,y1,col='red',cex=2)
  #points(x2,y2,col='blue',cex=2)
  return(data.frame('x'=xs,'y'=ys,'angle'=angles))
}

doubleSpiral<-function(x1,y1,x2,y2,width,weights=c(.5,.5),...){
  midX<-(x1*weights+x2*rev(weights))
  midY<-(y1*weights+y2*rev(weights))
  inSpiral<-spiralCoords(x1,y1,midX[2],midY[2],width=width,...)
  outSpiral<-spiralCoords(x2,y2,midX[1],midY[1],width=-width,...)
  outSpiral<-outSpiral[nrow(outSpiral):1,]
  browser()
  rbind(inSpiral,outSpiral)
}
#spi<-doubleSpiral(1,1,2,2,rotations=1,nv=500,width=-.1)
#plot(spi[,1:2],type='l')
#lines(spi[,3:4],col='red')

#points(doubleSpiral(1.01,1,2.01,2,rotations=1.5),col='red')
#zz<-spiralCoords(1,1,2,2,rotations=2,width=.2,nv=4000)
#plot(zz[,1],zz[,2],type='l')
#lines(zz[,3],zz[,4])


spiral<-function(a,b,rotations=1,nv=500,width=.1){
  theta<-seq(0,2*pi*rotations,length.out=nv)
  r=a+b*theta 
  xs<-cos(theta)*r
  ys<-sin(theta)*r
  tangent<-(b*tan(theta)+(a+b*theta))/(b-(a+b*theta)*tan(theta))
  div<-sqrt(tangent^2+1)
  dx=b*cos(theta)-(a+b*theta)*sin(theta)
  dy=b*sin(theta)+(a+b*theta)*cos(theta)
  offset<-cbind(1/div*sign(dx),tangent/div*sign(dx))
  return(data.frame(x=xs,y=ys,x2s=xs+-offset[,2]*width,y2s=ys+offset[,1]*width,theta,tangent,dx,dy))
}

