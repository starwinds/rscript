require(knitr)
require(markdown)

knit("My_Analysis.Rmd")
markdownToHTML('My_Analysis.md', 'My_Analysis.html', options=c("use_xhml"))
~                                        
