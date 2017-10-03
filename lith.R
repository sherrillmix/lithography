source('functions.R')

#open in inkscape and save with base unit px
#cairo_pdf('lithMulti.pdf',width=3/mi2in/90,height=3/mi2in/90)

#save as dxf 12 scale by 100x
dim<-1.1
cairo_pdf('lithMulti.pdf',width=3*dim/mi2in/100,height=3*dim/mi2in/100)
  layout(matrix(c(0,1,1,2,2,0,3,3,4,4,5,5,0,6,6,7,7,0),ncol=3))
  plotLith(constrictWidth=2,title="2um",width=dim,height=dim)
  plotLith(constrictWidth=3,title="3um",width=dim,height=dim)
  plotLith(constrictWidth=4,title="4um",width=dim,height=dim)
  plotLith(constrictWidth=5,title="5um",width=dim,height=dim)
  plotLith(constrictWidth=6,title="6um",width=dim,height=dim)
  plotLith(constrictWidth=9,title="9um",width=dim,height=dim)
  testWidths<-seq(1,30,.25)
  widths<-rep(interleave(testWidths,NA),rep(c(2,1),length(testWidths)))
  coords<-plotLith(constrictWidth=widths,nLine=length(widths),mainLwd=30*widths/widths,title='Mixed')
  showtext.begin()
  text(coords[['lines']]+min(diff(coords[['lines']],na.rm=TRUE))*.05,ifelse(rep(1:(length(widths)/length(testWidths)),length.out=length(testWidths))==(length(widths)/length(testWidths)),coords[['mid']],NA),c(NA,as.character(widths)[-length(widths)]),cex=7,srt=90,font=2,adj=c(1,.5))
  showtext.end()
dev.off()

pdf('spiral35_5000.pdf',width=.1/mi2in/100,height=.5/mi2in/100)
spiralLith(nRot=2,narrowWidth=35,wideWidth=500,narrowLength=5000,width=.1,height=.5,wideLength=3000,holeDiameter=2000)
dev.off()

pdf('spiral35_10000.pdf',width=.1/mi2in/100,height=.5/mi2in/100)
spiralLith(nRot=3,narrowWidth=35,wideWidth=500,narrowLength=10000,width=.1,height=.5,wideLength=3000,holeDiameter=2000)
dev.off()

pdf('spiral50_10000.pdf',width=.1/mi2in/100,height=.5/mi2in/100)
spiralLith(nRot=3,narrowWidth=50,wideWidth=2000,narrowLength=10000,width=.1,height=.5,wideLength=3000,holeDiameter=2000)
dev.off()

pdf('spiral140_5000.pdf',width=.2/mi2in/100,height=.9/mi2in/100)
spiralLith(nRot=1,narrowWidth=140,wideWidth=2000,narrowLength=5000,wideLength=5000,width=.2,height=.9)
dev.off()

pdf('spiral140_10000.pdf',width=.2/mi2in/100,height=.9/mi2in/100)
spiralLith(nRot=2,narrowWidth=140,wideWidth=2000,narrowLength=10000,wideLength=5000,width=.2,height=.9)
dev.off()

pdf('constrict5_5.pdf',width=1/mi2in/100,height=1/mi2in/100)
constrictLith(narrowWidth=5,wideWidth=3000,narrowLength=5,wideLength=3000,width=1,height=1)
dev.off()

pdf('constrict10_10.pdf',width=1/mi2in/100,height=1/mi2in/100)
constrictLith(narrowWidth=10,wideWidth=3000,narrowLength=10,wideLength=3000,width=1,height=1)
dev.off()

nRot<-1.4
offset<-findSpacingDouble(10000,nRot)
pdf('test.pdf')
  zz<-doubleSpiral(0,0,offset,0,width=30,rotations=nRot)
  plot(zz[,1:2],type='l')
  lines(zz[,3:4],col='red')
#points(zz[,3:4])
dev.off()




zz<-spiral(10,20/2/pi/2,2,width=.8);plot(zz[,1:2],type='l');lines(zz[,3:4]);points(0,0)
zz2<-spiral(0,-20/2/pi/2,2,width=-.8);lines(zz2[,1:2],col='red');lines(zz2[,3:4],col='red')
