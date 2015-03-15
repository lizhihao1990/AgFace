# manual conversion from Rmd to pdf via html and pandoc

#require(knitr)
#require(markdown)

#knit("myfile.Rmd", "myfile.md")
#markdownToHTML("myfile.md", "myfile.html", options = c("use_xhml"))

#system("pandoc -s myfile.html -o out.pdf")

# from https://github.com/woobe/blenditbayes/blob/master/2013-08-easy-documentation/rmd2pdf.R
## Convert Rmd into pdf

## Set working diectory
#setwd("/media/woobe/SUPPORT/Repo/blenditbayes/2013-08-easy-documentation")

## Define filename
#FILE <- "report"

## Convert .Rmd into .md
#library(knitr)
#knit2html(paste(FILE, ".Rmd", sep=""))

## Convert .md into .pdf
#system(paste("pandoc -o ", FILE, ".pdf ", FILE, ".md", sep=""))

# pandoc -s test.md -t latex -o test.tex

# to get subscripts right:
# knit("Silverstar_report.Rmd", output = "out.md")
# then, on console:
#pandoc -s out.md -t latex -o out2.pdf

myRmd2Pdf <- function(myfile) {
               
               ## myfile is the filename of an existing .Rmd file in the current folder
               ## needs to be provided as character string, i.e. enclosed in ""
                
               require(knitr)
               require(markdown)
               
               out.md   <- paste(myfile, "md",   sep = ".")
               out.html <- paste(myfile, "html", sep = ".")
               out.pdf  <- paste(myfile, "pdf",  sep = ".")
               
               knit(myfile, output = out.md)
               markdownToHTML(out.md, out.html, options = c("use_xhml"))
               
               shell_command_a <- paste("pandoc -s ", out.html, sep = " ")
               shell_command_b <- paste("-o ", out.pdf, sep = "")
               shell_command_c <- paste(shell_command_a, shell_command_b, sep = " -t latex ")
               shell_command_d <- paste(shell_command_c, "", sep = " ")
               #shell_command_d <- paste(shell_command_c, "--css=markdown.css", sep = " ")
               system(paste(shell_command_d))
               system(paste("rm", out.md,   sep = " "))
               # system(paste("rm", out.html, sep = " "))
               cat("PDF file created\n")
}

myRmd2latex2Pdf <- function(myfile) {
# knit("Silverstar_report.Rmd", output = "out.md")
# then, on console:
#pandoc -s out.md -t latex -o out2.pdf
        require(knitr)
        require(markdown)
        
        out.md   <- paste(myfile, "md",   sep = ".")
        out2.md  <- paste(out.md, "2", sep = "")
        out.pdf  <- paste(myfile, "pdf",  sep = ".")

        knit(myfile, output = out.md)
        
        # jumping though some hoops to remove the automatic message from xtable
        # lines start with "%" get removed.
        
        sed_command_a <- paste("sed '/^%/d'", out.md, sep = " ")
        sed_command_b <- paste(">", out2.md, sep = " ")
        sed_command_c <- paste(sed_command_a, sed_command_b)
        
        system(paste(sed_command_c))
        system(paste("rm", out.md,  sep = " "))
        my_command_a <- paste("mv", out2.md,  sep = " ")
        system(paste(my_command_a, out.md, sep = " "))
        
        # calling pandoc on the command line
        shell_command_a <- paste("pandoc -s -V geometry:margin=2cm", out.md, sep = " ")
        shell_command_b <- paste("-t latex -o", out.pdf, sep = " ")
        shell_command_c <- paste(shell_command_a, shell_command_b, sep = " ")
        
        system(paste(shell_command_c))
        
        # removing intermediate files        
        #system(paste("rm", out.md,  sep = " "))
        #system(paste("rm", out2.md, sep = " "))
        
        cat("PDF file created\n")
}



#The solution is not too straightforward, maybe someone else will be able to streamline it.

#The basic steps. (Windows 7)

#    You can add the argument fig.pos="H" to the knitr options, either globally or for each individual chunk. NOTE the capital H. This instructs latex to place figure floats exactly where they are called in the Rmd file.

#    BUT, this requires the package to be used by latex, this you can specify in the template that pandoc uses to construct pdf files. You do this by adding the line \usepackage{float}

#    BUT, you first need to find the current template file to modify. I could not find this anywhere but you can get pandoc to print the contents of the template to the console with this command: pandoc -D latex

#    Cut and paste this template code into an empty text file.

#    Add the line: \usepackage{float}

#    Save under the filename "default.latex" in a directory such as C:\Users\YOURNAME\pandoc\templates

#    Add the option --data-dir=C:/Users/YOURNAME/pandoc/templates" to your call to pandoc OR Pandoc.convert("my file.md", format="pdf", options=c("--data-dir=C:/Users/YOURNAME/pandoc/templates")) if using pander in R.

#I hope this works for you.

