library(showtext)
mi2in<-3.93701e-5
plotLith<-function(width=1,height=1,buffer=.15,holeDiameter=.1,nLine=200,mainLwd=30,constrictWidth=2,constrictLength=30,constrictRamp=constrictLength*3,title='',titleCex=100){
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
    #triangles
    maxLeft<-max(linePos)+max(mainLwdIn,na.rm=TRUE)/2
    maxRight<-min(linePos)-max(mainLwdIn,na.rm=TRUE)/2
    polygon(c(topHole[1],maxLeft,maxRight),c(topHole[2]+holeDiameter/2,lineTop,lineTop),col='black',border=NA)
    polygon(c(bottomHole[1],maxLeft,maxRight),c(bottomHole[2]-holeDiameter/2,lineBottom,lineBottom),col='black',border=NA)
    #rectangles above/below triangles
    rect(maxLeft,lineTop+max(mainLwdIn,na.rm=TRUE)/10,maxRight,lineTop-max(mainLwdIn,na.rm=TRUE)*3,col='black',border=NA)
    rect(maxLeft,lineBottom-max(mainLwdIn,na.rm=TRUE)/10,maxRight,lineBottom+max(mainLwdIn,na.rm=TRUE)*3,col='black',border=NA)
    if(title!=''){
      showtext_begin()
      text(rep(c(topHole[1],bottomHole[1]),each=2)*c(.5,1.5),rep(c(topHole[2],bottomHole[2]),each=2),title,cex=titleCex,font=2)
      showtext_end()
    }
    #box
    rect(c(0,0,0,width),c(0,0,height,0),c(0+buffer/10,width,width,width-buffer/10),c(height,0+buffer/10,height-buffer/10,height),col='black',border=NA)
    return(invisible(list('lines'=linePos,'mid'=lineMid,'top'=topHole,'bottom'=bottomHole)))
  #dev.off()
}

spiralLith<-function(width=.5,height=1,buffer=.05,holeDiameter=wideWidth*2,yBase=.5,angle=0,narrowWidth=40,narrowLength=5000,wideWidth=800,wideLength=5000,rampLength=200,nRot=2,title='',titleCex=20){ 
  wideWidthIn<-wideWidth*mi2in
  narrowWidthIn<-narrowWidth*mi2in
  wideLengthIn<-wideLength*mi2in
  narrowLengthIn<-narrowLength*mi2in
  rampLengthIn<-rampLength*mi2in
  holeDiameterIn<-holeDiameter*mi2in
  offset<-findSpacingDouble(narrowLength,nRot)*mi2in
  #empty plot
  par(mar=c(0,0,0,0))
  plot(1,1,type='n',xlab='',ylab='',ylim=c(0,height),xlim=c(0,width),bty='n',xaxt='n',yaxt='n',xaxs='i',yaxs='i')
  #hole positions
  bottomHole<-c(width/2,buffer+holeDiameterIn/2)
  topHole<-c(width/2,bottomHole[2]+wideLengthIn*2+offset+2*rampLengthIn+narrowWidthIn)
  mid<-bottomHole[2]+wideLengthIn+c(0,rampLengthIn*2+offset+narrowWidthIn)
  #top hole
  plotrix::draw.circle(topHole[1],topHole[2],radius=holeDiameterIn/2,col='black')
  #bottom hole
  plotrix::draw.circle(bottomHole[1],bottomHole[2],radius=holeDiameterIn/2,col='black')
  #thick rects
  rect(c(topHole[1]-wideWidthIn/2,bottomHole[1]-wideWidthIn/2),c(topHole[2],bottomHole[2]),c(topHole[1]+wideWidthIn/2,bottomHole[1]+wideWidthIn/2),c(mid[2]-rampLengthIn*.02,mid[1]+rampLengthIn*.02),col='black',border=NA)
  #top ramp
  polygon(c(topHole[1]-wideWidthIn/2,topHole[1]+wideWidthIn/2,topHole[1]-narrowWidthIn/2,topHole[1]+narrowWidthIn/2),c(mid[2],mid[2],mid[2]-rampLengthIn,mid[2]-rampLengthIn),col='black',border=NA)
  #top connector
  rect(topHole[1]+narrowWidthIn/2,mid[2],topHole[1]-narrowWidthIn/2,mid[2]-rampLengthIn-narrowWidthIn,col='black',border=NA)
  #bottomRamp
  polygon(c(bottomHole[1]-wideWidthIn/2,bottomHole[1]+wideWidthIn/2,bottomHole[1]-narrowWidthIn/2,bottomHole[1]+narrowWidthIn/2),c(mid[1],mid[1],mid[1]+rampLengthIn,mid[1]+rampLengthIn),col='black',border=NA)
  #bottom connector
  rect(bottomHole[1]+narrowWidthIn/2,mid[1],bottomHole[1]-narrowWidthIn/2,mid[1]+rampLengthIn+narrowWidthIn,col='black',border=NA)
  #spiral
  spiralCoords<-doubleSpiral(bottomHole[1],mid[1]+rampLengthIn,topHole[1],mid[2]-rampLengthIn-narrowWidthIn,width=narrowWidthIn,rotations=nRot)
  polygon(c(spiralCoords[,'x'],rev(spiralCoords[,'x2'])),c(spiralCoords[,'y'],rev(spiralCoords[,'y2'])),col='black',border=NA)
  if(title!=''){
    showtext_begin()
    text(topHole[1]+-.7*wideWidthIn,topHole[2]+-.95*wideLengthIn,title,cex=titleCex,font=2,srt=90,adj=c(0,0))
    text(bottomHole[1]+.7*wideWidthIn,bottomHole[2]+.95*wideLengthIn,title,cex=titleCex,font=2,srt=90,adj=c(1,1))
    showtext_end()
  }
}

constrictLith<-function(width=.5,height=1,buffer=.05,holeDiameter=wideWidth,yBase=.5,angle=0,narrowWidth=40,narrowLength=5000,wideWidth=800,wideLength=5000,rampLength=200,supportSpacing=1e9,supportWidth=200,title='',titleCex=20){ 
  wideWidthIn<-wideWidth*mi2in
  narrowWidthIn<-narrowWidth*mi2in
  wideLengthIn<-wideLength*mi2in
  narrowLengthIn<-narrowLength*mi2in
  rampLengthIn<-rampLength*mi2in
  holeDiameterIn<-holeDiameter*mi2in
  supportSpacingIn<-supportSpacing*mi2in
  #empty plot
  par(mar=c(0,0,0,0))
  plot(1,1,type='n',xlab='',ylab='',ylim=c(0,height),xlim=c(0,width),bty='n',xaxt='n',yaxt='n',xaxs='i',yaxs='i')
  #hole positions
  bottomHole<-c(width/2,buffer+holeDiameterIn/2)
  topHole<-c(width/2,bottomHole[2]+wideLengthIn*2+narrowLengthIn+2*rampLengthIn)
  mid<-bottomHole[2]+wideLengthIn+c(0,rampLengthIn*2+narrowLengthIn)
  #top hole
  plotrix::draw.ellipse(topHole[1],topHole[2],holeDiameterIn/2,holeDiameterIn/4,segment=c(0,180),col='black')
  #plotrix::draw.circle(topHole[1],topHole[2],radius=holeDiameterIn/2,col='black')
  #bottom hole
  #plotrix::draw.circle(bottomHole[1],bottomHole[2],radius=holeDiameterIn/2,col='black')
  plotrix::draw.ellipse(bottomHole[1],bottomHole[2],holeDiameterIn/2,holeDiameterIn/4,segment=c(180,360),col='black')
  #thick rects
  if(supportSpacing>wideWidth/2){
    rect(c(topHole[1]-wideWidthIn/2,bottomHole[1]-wideWidthIn/2),c(topHole[2],bottomHole[2]),c(topHole[1]+wideWidthIn/2,bottomHole[1]+wideWidthIn/2),c(mid[2]-rampLengthIn*.02,mid[1]+rampLengthIn*.02),col='black',border=NA)
  }else{
    supportRows<-seq(0,wideWidthIn-supportSpacingIn,supportSpacingIn)
    supportCols<-seq(0,wideLengthIn-supportSpacingIn,supportSpacingIn)
    supportLefts<--wideWidthIn/2+supportRows
    supportRights<--wideWidthIn/2+c(supportRows[-1]-supportWidth*mi2in,wideWidthIn)
    supportBottoms<-supportCols
    supportTops<-c(supportCols[-1]-supportWidth*mi2in,wideLengthIn)
    #full vertical rects
    rect(c(topHole[1]+supportLefts,bottomHole[1]+supportLefts),rep(c(topHole[2],bottomHole[2]),each=length(supportRows)),c(topHole[1]+supportRights,bottomHole[1]+supportRights),rep(c(mid[2]-rampLengthIn*.02,mid[1]+rampLengthIn*.02),each=length(supportRows)),col='black',border=NA)
    #full horizontal rects
    rect(rep(c(topHole[1]-wideWidthIn/2,bottomHole[1]-wideWidthIn/2),each=length(supportCols)),c(topHole[2]-supportBottoms,bottomHole[2]+supportBottoms),rep(c(topHole[1]+wideWidthIn/2,bottomHole[1]+wideWidthIn/2),each=length(supportCols)),c(topHole[2]-supportTops,bottomHole[2]+supportTops),col='black',border=NA)
  }
  #top ramp
  polygon(c(topHole[1]-wideWidthIn/2,topHole[1]+wideWidthIn/2,topHole[1]-narrowWidthIn/2,topHole[1]+narrowWidthIn/2),c(mid[2],mid[2],mid[2]-rampLengthIn,mid[2]-rampLengthIn),col='black',border=NA)
  #bottomRamp
  polygon(c(bottomHole[1]-wideWidthIn/2,bottomHole[1]+wideWidthIn/2,bottomHole[1]-narrowWidthIn/2,bottomHole[1]+narrowWidthIn/2),c(mid[1],mid[1],mid[1]+rampLengthIn,mid[1]+rampLengthIn),col='black',border=NA)
  #thin section
  rect(bottomHole[1]+narrowWidthIn/2,mid[1],bottomHole[1]-narrowWidthIn/2,mid[2],col='black',border=NA)
  if(title!=''){
    showtext_begin()
    text(topHole[1]-.5*wideWidthIn-200*mi2in,topHole[2]+-.95*wideLengthIn,title,cex=titleCex,font=2,srt=90,adj=c(0,0))
    text(bottomHole[1]+.5*wideWidthIn+200*mi2in,bottomHole[2]+.95*wideLengthIn,title,cex=titleCex,font=2,srt=90,adj=c(1,1))
    showtext_end()
  }
}

interleave<-function(xx,yy){
  n<-max(length(xx),length(yy))
  out<-2*n
  out[(1:n)*2-1]<-xx
  out[(1:n)*2]<-yy
  return(out)
}

if(FALSE){
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
    rbind(inSpiral,outSpiral)
  }
  #spi<-doubleSpiral(1,1,2,2,rotations=1,nv=500,width=-.1)
  #plot(spi[,1:2],type='l')
  #lines(spi[,3:4],col='red')

  #points(doubleSpiral(1.01,1,2.01,2,rotations=1.5),col='red')
  #zz<-spiralCoords(1,1,2,2,rotations=2,width=.2,nv=4000)
  #plot(zz[,1],zz[,2],type='l')
  #lines(zz[,3],zz[,4])

}

spiral<-function(a,b,rotations=1,nv=500,width=.1,start=0){
  theta<-seq(start,start+2*pi*rotations,length.out=nv)
  r=a+b*(theta-start)
  xs<-cos(theta)*r
  ys<-sin(theta)*r
  #tangent<-(b*tan(theta)+(a+b*theta))/(b-(a+b*theta)*tan(theta))
  #div<-sqrt(tangent^2+1)
  dx<-b*cos(theta)-(a+b*(theta-start))*sin(theta)
  dy<-b*sin(theta)+(a+b*(theta-start))*cos(theta)
  div<-sqrt(dx^2+dy^2)
  offset<-cbind(dx/div,dy/div)
  x2s<-xs+-offset[,2]*width
  y2s<-ys+offset[,1]*width
  #offset<-cbind(1/div*sign(dx),tangent/div*sign(dx))
  return(data.frame(x=xs,y=ys,x2=x2s,y2=y2s,theta,dx,dy))
}

doubleSpiral<-function(x1,y1,x2,y2,width,rotations=1,...){
  midX<-(x1+x2)/2
  midY<-(y1+y2)/2
  #rbind(inSpiral,outSpiral)
  startAngle<-atan2(y1-y2,x1-x2)
  a<-0
  b<-sqrt((x1-x2)^2+(y1-y2)^2)/rotations/2/pi/2
  nv<-rotations*300
  inSpiral<-spiral(0,b,width=width,rotations=rotations,start=startAngle,nv=nv,...)
  inSpiral[,c(1,3)]<-inSpiral[,c(1,3)]+midX
  inSpiral[,c(2,4)]<-inSpiral[,c(2,4)]+midY
  #inSpiral[1:(nv-25),]
  outSpiral<-spiral(0,-b,width=-width,rotations=rotations,start=startAngle,nv=nv,...)
  #inSpiral[1:(nv-25),]
  outSpiral[,c(1,3)]<-outSpiral[,c(1,3)]+midX
  outSpiral[,c(2,4)]<-outSpiral[,c(2,4)]+midY
  inSpiral<-inSpiral[nrow(outSpiral):1,]
  return(rbind(inSpiral,outSpiral))
}

spiralIntegral<-function(a,b,theta,theta0=0){
  sqrt((a+b*(theta-theta0))^2+b^2)*(a+b*(theta-theta0))/b/2+b/2*log(sqrt((a+b*(theta-theta0))^2+b^2)+a+b*(theta-theta0))
}
spiralLength<-function(a,b,theta2,theta1=0,theta0=0){
  spiralIntegral(a,b,theta2,theta0)-spiralIntegral(a,b,theta1,theta0)
}
findSpacingDouble<-function(length,rotations){
  b<-optim(list(b=1),function(b)abs(length-spiralLength(0,b,rotations*2*pi)/2),method='Brent',lower=0,upper=10000)$par
  spacing<-b*2*pi*rotations
  return(spacing)
}
