Praca Domowa 2: czas do północy 21.11

Przygotuj funkcję, która jako argument przyjmuje dowolną funkcję i zwraca funkcję, która wykonuje dokładnie to samo, tylko w NSE.

Przykłady:

```R
x <- list(a = c(1, 2, 30), b = c(2, 1))
min_NSE <- zadanieDomowe(min)
min_NSE(x, a)
```

Przygotuj kilka rozwiązań tego zadania i porównaj ich szybkość działania przy różnych funkcjach i różnych rozmiarach zbioru danych. Przy ocenie zadania bardzo będę doceniane rozwiązania szybkie, przyjazne dla zużycia pamięci i eleganckie.

Funkcje, które należy uwzględnić w benchmarku (można uwzględnić więcej):

 - min,
 - mean,
 - unlist (z use.names = TRUE i use.names = FALSE),
 - lm.
 
Prace należy zgłaszać w tym folderze w pliku .Rmd (i sparsowanym pliku html) w folderze NazwiskoImie.
