#TianR.Feb28, 2014
#Inspired by M.B.'s trick to plot error bars by using "arrows()"

#Given a m*n matrix, plot bar plot with
#error bars, values of mean, scatter points, pairwise t-test

colplot<-function(mat="matrix",path="~/Out.plot.pdf", digt=2){

	pdf(path)
	mat_mean<-apply(mat, 2, mean)
	mat_sd<-apply(mat, 2, sd)
	bar<-barplot(mat_mean, beside=T, ylim=c(0,(max(mat_mean)+max(mat_sd))))
	
	arrows(bar, (mat_mean+mat_sd), bar, (mat_mean-mat_sd), angle=90, code=3)
	
	text((bar+bar[1]/8),(mat_mean+min(mat_sd)/2), paste(as.character(round(mat_mean, digt)), "+-", as.character(round(mat_sd, digt))))
	
	for (col in 1:ncol(mat)){
		points(rep((bar[col]-bar[1]/8),nrow(mat)), mat[,col])
		}
	
	dev.off()
	
	}



mat<-cbind(rnorm(15,2,0.5), rnorm(15,6,2), rnorm(15,4,0.5), rnorm(15,2,0.8))
colplot(mat)
#system.time((mat))
