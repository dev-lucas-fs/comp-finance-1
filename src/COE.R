library("readxl")
# LUCAS FERNANDES SILVA
COMECO = "2015-06-01"
FIM = "2018-06-01"
CODIGOS = c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBAS3.SA")

# LENDOS OS DADOS DA PLANILHA CLOSE.XLSX
DADOS <- read_excel(paste0(getwd(), "/src/assets/close.xlsx"))

