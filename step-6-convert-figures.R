
# convert pdf figs to svg using system commands

library(here)

# TR: the command line program pdf2svg may or may not exist for non-linux OS.
# I'm not sure of this. However, you should try importing the vector pdfs directly into
# Inkscape the way you save them out of R, and it should work just fine that way. 
# I think the bug I have (reading pdf w inkscape) is very particular to my situation.

convert_these_pdfs <- dir(here("plots"))
into_these_svgs <- gsub(convert_these_pdfs, pattern = ".pdf", replacement = ".svg")
for (i in 1:length(convert_these_pdfs)){
  convert_command <- 
    paste0("pdf2svg ",here("plots",convert_these_pdfs[i])," ",here("plots",into_these_svgs[i])," all")
  system(convert_command)
}





