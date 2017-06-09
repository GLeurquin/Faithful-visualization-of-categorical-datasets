library(heatmap3)
library(RColorBrewer)
library(grDevices)


args <- commandArgs(TRUE)
filename = as.character(args[1])
isCat = as.numeric(args[2])

# isCat = 0
# directory = "../resources/toR/"
# filename = "mushrooms-Scale(27x1)-Original"
# filename = paste(directory, filename, sep="")

input = paste(filename, ".rformat", sep="")

df = read.table(input, header=FALSE, row.names=NULL)

removeZeroVar <- function(dat) {
	# https://stackoverflow.com/questions/8805298/quickly-remove-zero-variance-variables-from-a-data-frame
    dat[sapply(dat, function(x) length(levels(factor(x)))>1)]
}

labelCode = as.numeric(as.character(df[1,1]))
rowSide = NULL
colSide = NULL

if(labelCode > 3){
	# Unrecognized code
	print("Unrecognized label code")
	print(labelCode)
	quit(save="no", status=1)
}

if(labelCode == 1 || labelCode == 3) {
	# Only need columns
	colLabels = as.character(df[1,seq(2, dim(df)[2])]) # first row is label
	nUniqueColLabels = dim(table(colLabels))[1]
	col2 <- brewer.pal(nUniqueColLabels, "Set3")
	colorIndicesCol = match(colLabels, levels(factor(colLabels)))
	colSide = sapply(colorIndicesCol, function(x) col2[x])
	if(labelCode == 1){
		rowSide = NULL
	}
	stopifnot(length(colSide) == dim(df)[2]-1)
}

if(labelCode == 2 || labelCode == 3) {
	# Only need rows
	rowLabels = as.character(df[seq(2, dim(df)[1]),1]) # first column is label
	nUniqueRowLabels = dim(table(rowLabels))[1]
	col1 <- brewer.pal(nUniqueRowLabels, "Set3")
	colorIndicesRow = match(rowLabels, levels(factor(rowLabels)))
	rowSide = sapply(colorIndicesRow, function(x) col1[x])
	if(labelCode == 2){
		colSide = NULL
	}
	stopifnot(length(rowSide) == dim(df)[1]-1)
}

mydata = df[-1,-1] # Take all but first column (labels)

m = data.matrix(mydata)
# png(filename=paste(filename,"-heatmap.png",sep=""), 5000,5000)

method = "complete"

if(isCat == 1) {
	palette = colorRampPalette(c("navy", "white","firebrick3"))(1024)
} else {
	palette = gray.colors(2, start = 1, end = 0, gamma = 2.2, alpha = NULL)
}

if(is.null(rowSide) && is.null(colSide)){
	timeTaken = system.time(hm <- heatmap3(m, distfun = function(x) dist(x, method = "manhattan", diag = FALSE, upper = FALSE, p = 2), scale="none", method=method, col=palette))
} else if(is.null(rowSide)){
	timeTaken = system.time(hm <- heatmap3(m, distfun = function(x) dist(x, method = "manhattan", diag = FALSE, upper = FALSE, p = 2), ColSideColors=colSide, scale="none", method=method,col=palette))
} else if(is.null(colSide)) {
	timeTaken = system.time(hm <- heatmap3(m, distfun = function(x) dist(x, method = "manhattan", diag = FALSE, upper = FALSE, p = 2), RowSideColors=rowSide, scale="none", method=method,col=palette))
} else {
	timeTaken = system.time(hm <- heatmap3(m, distfun = function(x) dist(x, method = "manhattan", diag = FALSE, upper = FALSE, p = 2), RowSideColors=rowSide, ColSideColors=colSide, scale="none", method=method, col=palette))
}


print(filename)
print(timeTaken)

write.csv(hm["rowInd"],paste(filename,"rowInd.order",sep=""), row.names=FALSE)
write.csv(hm["colInd"],paste(filename,"colInd.order",sep=""), row.names=FALSE)
dev.off()
