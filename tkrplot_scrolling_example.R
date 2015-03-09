library(tkrplot)
# https://stat.ethz.ch/pipermail/r-help/2007-September/140731.html

#y <- rnorm(10000, 10, 2) + 5*sin( (1:10000)/1000 )
y <- df$SYS1_Sapflow_SGA2_2_Avg

tt <- tktoplevel()
left <- tclVar(1)
oldleft <- tclVar(1)
right <- tclVar(10)

f1 <- function(){
        lleft <- as.numeric(tclvalue(left))
        rright <- as.numeric(tclvalue(right))
        #x <- seq(lleft,rright,by=1)
        x <- df$TIMESTAMP[seq(lleft,rright,by=1)]
        my.y <- y[seq(lleft,rright,by=1)]
        my.data <- data.frame(x = x, y = my.y)
        my.data <- my.data[is.finite(my.data$y) == TRUE, ]
        
        plot(x, y, type='b', data = my.data))#, ylim=range(my.y, finite = TRUE))
}

img <- tkrplot(tt, f1)

f2 <- function(...){
        ol <- as.numeric(tclvalue(oldleft))
        tclvalue(oldleft) <- tclvalue(left)
        r <- as.numeric(tclvalue(right))
        tclvalue(right) <- as.character(r + as.numeric(...) - ol)
        tkrreplot(img)
}

f3 <- function(...){
        tkrreplot(img)
}

f4 <- function(...){
        ol <- as.numeric(tclvalue(oldleft))
        tclvalue(left) <- as.character(ol+100)
        tclvalue(oldleft) <- as.character(ol+100)
        r <- as.numeric(tclvalue(right))
        tclvalue(right) <- as.character(r+100)
        tkrreplot(img)
}

s1 <- tkscale(tt, command=f2, from=1, to=length(y),
        variable=left, orient="horiz",label='left')
s2 <- tkscale(tt, command=f3, from=1, to=length(y),
        variable=right, orient="horiz",label='right')
b1 <- tkbutton(tt, text='->', command=f4)

tkpack(img,s1,s2,b1) 
