# simulating spatial MA process

Um unsere Implementierung validieren zu können, brauchen wir Daten,
welche sowohl unter \\H_0\\ als auch unter der Alternative erzeugt
wurden. Wie bereits in der Einleitung erwähnt, wollen wir alles so
simpel wie möglich halten. Der Prozess ist daher eine einfache
Linearkombination von normalverteilten Variablen und stark von der
Simulation eines \\MA(q)\\ Prozesses inspiriert. Der Grundgedanke
hierbei ist, dass wir die Koeffizienten \\\varphi_k\\ des Lag-Polynoms
reinterpretieren von "linearer Einfluss von Residuen zu Lag \\k\\" zu
"linearer Einfluss von Residuen zum Abstand \\k\\". Also heißt dass für
\\X\_{ij}\\, dass jedes Residuum mit Abstand \\k\\ einen linearen
Einfluss von \\\varphi_k\\ auf \\X\_{ij}\\ nimmt. Dieser Prozess lässt
sich mit einer 2D Konvolution umsetzen und ist nicht nur stationär,
sondern hat auch eine isotrope Kovarianzfunktion.

## Usage

``` r
gridMA(N, M, K, distribution = "normal")
```

## Arguments

- N:

  rows

- M:

  columns of grid

- K:

  numeric matrix of kernel weights

- distribution:

  Type of distribution must be one of "normal", "uniform", "cauchy",
  "chisq"

## Value

matrix with N rows and M columns
