library(miniBeamer)
library(rmarkdown)

# do wygenerowania raportu potrzebne okreslenie sciezki do grafiki w prawym dolnym rogu (argument br)
# dolaczone do folderu z prezentacja
rmarkdown::render('future_prezentacja.Rmd', miniBeamer::rmd_to_beamer(toc = FALSE,
                                                                      latex_engine = 'pdflatex',
                                                                      br = "/home/mckraqs/Studia/Semestr 5/R dla zaawansowanych/Prezentacja/mini_pw.png"))
