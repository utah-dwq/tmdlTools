library(plotly)
plot_ly(x = ~ecoli$Date, y = ~ecoli$E.coli_Geomean, type = "scatter",color = ~ecoli$ML_Name, mode = "lines+markers")
