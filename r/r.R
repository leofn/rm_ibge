# ==============================
# Setores Censitários 2022 recortados por RM (CEM + IBGE)
# Exporta GPKG, SHP e CSV com metadados
# ==============================

# ---- Pacotes ----
pkgs <- c("sf","dplyr","purrr","stringr","readr")
inst <- rownames(installed.packages())
if (length(setdiff(pkgs, inst)) > 0) install.packages(setdiff(pkgs, inst))
lapply(pkgs, library, character.only = TRUE)

# ---- Pastas ----
dir_out <- "RMs_setores_2022"
dir_setores <- "setores_censitarios"   # cache local
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_setores, showWarnings = FALSE, recursive = TRUE)

# ---- Link oficial IBGE ----
ibge_root <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2022/setores/gpkg/UF"

# ---- Função: baixar malha de setores de uma UF ----
baixar_ibge_uf <- function(uf, outdir = dir_setores) {
  uf <- toupper(uf)
  url <- sprintf("%s/%s/%s_setores_CD2022.gpkg", ibge_root, uf, uf)
  dest <- file.path(outdir, paste0(uf, "_setores_CD2022.gpkg"))
  if (!file.exists(dest)) {
    message("→ Baixando ", url)
    tryCatch({
      download.file(url, dest, mode = "wb", quiet = TRUE)
    }, error = function(e) {
      stop("Falha ao baixar: ", url)
    })
  }
  dest
}

# ---- Função: recortar setores para uma RM ----
recortar_setores_por_rm <- function(rm_geom, uf, nome_base) {
  arq_uf <- baixar_ibge_uf(uf)
  setores_uf <- st_read(arq_uf, quiet = TRUE)
  if (st_crs(setores_uf) != st_crs(rm_geom)) {
    setores_uf <- st_transform(setores_uf, st_crs(rm_geom))
  }
  rm_union <- st_union(rm_geom)
  keep <- lengths(st_intersects(setores_uf, rm_union, sparse = TRUE)) > 0
  setores_rm <- setores_uf[keep, , drop = FALSE]
  
  # Padronizar coluna de município
  if ("CD_MUN" %in% names(setores_rm)) {
    setores_rm <- dplyr::rename(setores_rm, code_muni = CD_MUN)
  }
  if (!"code_muni" %in% names(setores_rm)) {
    setores_rm$code_muni <- NA_character_
  }
  
  setores_rm <- setores_rm |> dplyr::select(code_muni, everything())
  
  # ---- Exportar em GPKG e SHP ----
  saida_gpkg <- paste0(nome_base, ".gpkg")
  saida_shp  <- paste0(nome_base, ".shp")
  
  st_write(setores_rm, saida_gpkg, delete_dsn = TRUE, quiet = TRUE)
  st_write(setores_rm, saida_shp,  delete_layer = TRUE, quiet = TRUE)
  
  tibble(
    name_rm = rm_geom$NOME_AC[1],
    uf = uf,
    tipo = rm_geom$TIPO[1],
    polo = rm_geom$POLO[1],
    municipios = length(unique(setores_rm$code_muni)),
    setores = nrow(setores_rm),
    codigos_municipios = paste(sort(unique(setores_rm$code_muni)), collapse = ","),
    arquivo_gpkg = saida_gpkg,
    arquivo_shp = saida_shp
  )
}

# ---- Ler shapefile do CEM ----
cem_rm <- st_read("CEMbrRMEa20/CEMbrRMEa20.shp", options = "ENCODING=LATIN1") |> st_make_valid()
cat("RMs carregadas do CEM:", nrow(cem_rm), "\n")

# ---- Loop em todas as RMs ----
meta <- map_dfr(seq_len(nrow(cem_rm)), function(i) {
  nm <- cem_rm$NOME_AC[i]
  uf <- cem_rm$UF_SIGLA[i]
  nm_slug <- gsub("\\s+", "_", nm)
  nome_base <- file.path(dir_out, paste0("setores_RM_", nm_slug, "_Censo2022"))
  
  message("[", i, "/", nrow(cem_rm), "] ", nm, " | UF: ", uf)
  
  tryCatch({
    recortar_setores_por_rm(cem_rm[i, ], uf, nome_base)
  }, error = function(e) {
    warning("Falhou em ", nm, " (UF=", uf, "): ", conditionMessage(e))
    tibble(
      name_rm = nm, uf = uf, tipo = cem_rm$TIPO[i], polo = cem_rm$POLO[i],
      municipios = NA, setores = NA,
      codigos_municipios = NA, arquivo_gpkg = NA, arquivo_shp = NA
    )
  })
})

# ---- Salvar metadados ----
write_csv(meta, file.path(dir_out, "metadados_RMs_setores2022.csv"))
cat("Concluído! Resultados em:", normalizePath(dir_out), "\n")
# ---- Fim do script ----