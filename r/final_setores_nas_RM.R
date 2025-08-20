library(sf)
library(dplyr)
library(purrr)
library(readr)

dir_setores <- "setores_censitarios"  # onde estão os arquivos UF_setores_CD2022.gpkg
dir_out <- "RMs_setores_2022"
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

# Função para recortar setores por RM
recortar_setores_por_rm <- function(rm_geom, uf, nome_base) {
  arq_uf <- file.path(dir_setores, paste0(uf, "_setores_CD2022.gpkg"))
  if (!file.exists(arq_uf)) stop("Arquivo não encontrado: ", arq_uf)
  
  setores_uf <- st_read(arq_uf, quiet = TRUE)
  if (st_crs(setores_uf) != st_crs(rm_geom)) {
    setores_uf <- st_transform(setores_uf, st_crs(rm_geom))
  }
  rm_union <- st_union(rm_geom)
  keep <- lengths(st_intersects(setores_uf, rm_union, sparse = TRUE)) > 0
  setores_rm <- setores_uf[keep, , drop = FALSE]
  
  # padronizar coluna de município
  if ("CD_MUN" %in% names(setores_rm)) {
    setores_rm <- dplyr::rename(setores_rm, code_muni = CD_MUN)
  }
  
  # salvar
  saida_gpkg <- paste0(nome_base, ".gpkg")
  st_write(setores_rm, saida_gpkg, delete_dsn = TRUE, quiet = TRUE)
  
  saida_shp <- paste0(nome_base, ".shp")
  st_write(setores_rm, saida_shp, delete_layer = TRUE, quiet = TRUE)
  
  tibble(
    name_rm = rm_geom$NOME_AC[1],
    uf = uf,
    tipo = rm_geom$TIPO[1],
    polo = rm_geom$POLO[1],
    municipios = length(unique(setores_rm$code_muni)),
    setores = nrow(setores_rm),
    arquivo_gpkg = saida_gpkg,
    arquivo_shp = saida_shp
  )
}

# Ler shapefile das RMs (CEM)
cem_rm <- st_read("CEMbrRMEa20/CEMbrRMEa20.shp", options = "ENCODING=LATIN1") |> st_make_valid()

# Rodar recorte para todas as RMs
meta <- map_dfr(seq_len(nrow(cem_rm)), function(i) {
  nm <- cem_rm$NOME_AC[i]
  uf <- cem_rm$UF_SIGLA[i]
  nm_slug <- gsub("\\s+", "_", nm)
  nome_base <- file.path(dir_out, paste0("setores_RM_", nm_slug, "_Censo2022"))
  
  message("[", i, "/", nrow(cem_rm), "] ", nm, " | UF: ", uf)
  tryCatch({
    recortar_setores_por_rm(cem_rm[i, ], uf, nome_base)
  }, error = function(e) {
    warning("Falhou em ", nm, ": ", conditionMessage(e))
    tibble(name_rm = nm, uf = uf, tipo = cem_rm$TIPO[i], polo = cem_rm$POLO[i],
           municipios = NA, setores = NA, arquivo_gpkg = NA, arquivo_shp = NA)
  })
})

write_csv(meta, file.path(dir_out, "metadados_RMs_setores2022.csv"))


