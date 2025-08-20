library(sf)
library(dplyr)

# Pasta de destino
dir_setores <- "setores_censitarios"
dir.create(dir_setores, showWarnings = FALSE, recursive = TRUE)

# Lista de UFs (códigos IBGE/IBGE sigla oficial)
ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
         "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
         "RO","RR","RS","SC","SE","SP","TO")

# URL base do IBGE
ibge_root <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2022/setores/gpkg/UF"

# Função para verificar e baixar se necessário
verificar_uf <- function(uf) {
  arq <- file.path(dir_setores, paste0(uf, "_setores_CD2022.gpkg"))
  url <- sprintf("%s/%s/%s_setores_CD2022.gpkg", ibge_root, uf, uf)
  
  precisa_baixar <- FALSE
  
  if (!file.exists(arq)) {
    message("❌ Arquivo não existe para ", uf, " → baixando.")
    precisa_baixar <- TRUE
  } else {
    # Testa se o arquivo abre
    ok <- tryCatch({ st_layers(arq); TRUE }, error = function(e) FALSE)
    if (!ok) {
      message("⚠️ Arquivo corrompido para ", uf, " → rebaixando.")
      precisa_baixar <- TRUE
    } else {
      message("✔ Arquivo válido para ", uf)
    }
  }
  
  if (precisa_baixar) {
    tryCatch({
      download.file(url, arq, mode = "wb", quiet = TRUE)
      message("✅ Baixado: ", uf)
    }, error = function(e) {
      warning("Falha ao baixar ", uf, " → ", conditionMessage(e))
    })
  }
  
  return(data.frame(uf = uf, arquivo = arq, existe = file.exists(arq)))
}

# Rodar para todas as UFs
resultados <- lapply(ufs, verificar_uf)
resultados <- bind_rows(resultados)

# Salvar log
write.csv(resultados, file.path(dir_setores, "verificacao_setores.csv"), row.names = FALSE)

message("Concluído! Veja o relatório em setores_censitarios/verificacao_setores.csv")
