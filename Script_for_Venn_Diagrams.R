#install.packages("VennDiagram")
library(VennDiagram)
library(ggplot2)
library(gridExtra)


classes <- read.table("../chaperone_classes.txt", sep="\t", head=T)
hq <- read.table("high_quality_2015.txt", sep="\t",head=T)
hq <- merge(hq,classes,by.x="Chaperone",by.y="Gene")
hq <- subset(hq, Client != "NA")

##venn diagram 2 circles
###Gim1 and Gim4
interest_A <- "Gim1"
interest_B <- "Gim4"

#set out list A and B.
A <- hq[which(hq$Protein == interest_A),]
B <- hq[which(hq$Protein == interest_B),]

list_A <- as.character(A$Client)
list_B <- as.character(B$Client)

#change label to wanted.
x <- list("Gim1" = list_A, "Gim4" = list_B)
overlap <- calculate.overlap(x)

plot1 <- draw.pairwise.venn(length(overlap$a1), length(overlap$a2), length(overlap$a3), fill=c("steelblue2","springgreen3"), category=c(interest_A,interest_B), cat.pos=c(-45,45), cex=1.5, cat.cex=1.5, cat.dist=0.05, ext.text=0, margin=0.05, ind=FALSE)

#########a bigger venn diagram! 3 circles.
interest_A <- "Ssa1"
interest_B <- "Ssa4"
interest_C <- "Sse2"

A <- hq[which(hq$Protein == interest_A),]
B <- hq[which(hq$Protein == interest_B),]
C <- hq[which(hq$Protein == interest_C),]

list_A <- as.character(A$Client)
list_B <- as.character(B$Client)
list_C <- as.character(C$Client)

#change label to wanted.
x <- list("Ssa1" = list_A, "Ssa4" = list_B, "Sse2" = list_C)


#determine intersections
A <- x[[1]]
B <- x[[2]]
C <- x[[3]]
nab <- intersect(A, B) #common to both list A and B
nbc <- intersect(B, C) #common to both list C and B
nac <- intersect(A, C) #common to both list A and C
nabc <- intersect(nab, C) #common to both list A and B and C

plot.new()
plot2 <- draw.triple.venn(length(A), length(B), length(C), length(nab),length(nbc),length(nac),length(nabc), fill=c("green3","gold2","red2"), category=c(interest_A,interest_B,interest_C), cat.pos=c(225,135,0), cex=1.5, cat.cex=1.5, cat.dist=0.05, ext.text=0, margin=c(0.05), euler.d=1, scaled=1, ind=FALSE)

grid.arrange(gTree(children=plot1))
grid.arrange(gTree(children=plot2))
