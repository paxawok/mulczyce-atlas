# Створює сторінку для кожного запису: objects/<ID>.qmd
# Запускається автоматично перед quarto render (pre-render у _quarto.yml).
# Вручну, з кореня проєкту: Rscript scripts/make_pages.R
# Згенеровані сторінки не редагуйте: їх буде перезаписано.

if (!file.exists("_quarto.yml")) stop("Запускайте скрипт з кореня проєкту.")
source("R/helpers.R")

yq <- function(x) paste0("\"", gsub("\"", "\\\\\"", gsub("\\\\", "\\\\\\\\", x)), "\"")
md <- function(x) gsub("([\\\\`*_\\[\\]<>|])", "\\\\\\1", x, perl = TRUE)
has <- function(x) length(x) == 1 && !is.na(x) && nzchar(as.character(x))
fact <- function(label, val) {
  if (!has(val)) return(NULL)
  paste0("| ", label, " | ", md(as.character(val)), " |")
}

ins  <- read_inscriptions()
sett <- if (file.exists("data/settlements.csv")) read_settlements() else NULL
src  <- if (file.exists("data/sources.csv")) read_text_csv("data/sources.csv") else NULL

dir.create("objects", showWarnings = FALSE)
for (f in list.files("objects", pattern = "\\.qmd$", full.names = TRUE)) {
  if (basename(f) != "index.qmd") unlink(f)
}
bad <- !grepl("^[A-Za-z0-9_-]+$", ins$id)
if (any(bad)) stop("ID має містити лише літери, цифри, '-' і '_': ", paste(ins$id[bad], collapse = ", "))

for (i in seq_len(nrow(ins))) {
  r <- ins[i, ]; id <- r$id
  desc <- paste(c(if (has(r$type)) r$type,
                  if (!is.na(r$houses)) paste0(r$houses, " дворів на карті")), collapse = ", ")
  fm <- c("---", paste0("title: ", yq(r$name_hist)),
          if (has(r$name_modern)) paste0("subtitle: ", yq(r$name_modern)),
          if (nzchar(desc)) paste0("description: ", yq(desc)),
          if (!is.na(r$status)) paste0("categories: [", yq(status_label(r$status)), "]"),
          "toc: false", "---")

  nav <- "[← Усі об'єкти](index.qmd)"
  if (!is.na(r$lat) && !is.na(r$lon)) nav <- paste0(nav, " · [Показати на карті](../karta.html#", id, ")")
  if ("post_url" %in% names(r) && has(r$post_url) && grepl("^https://", r$post_url))
    nav <- paste0(nav, " · [Пост у каналі](", r$post_url, ")")

  s <- if (!is.null(sett)) sett[which(sett$inscription_id == id), ] else NULL
  has_s <- !is.null(s) && nrow(s) > 0
  facts <- Filter(Negate(is.null), list(
    fact("ID", id), fact("Напис на карті", r$name_hist),
    fact("Сучасний відповідник", r$name_modern),
    if (has_s) fact("Варіанти написання", s$variants[1]),
    fact("Тип", r$type), fact("Дворів на карті", r$houses),
    fact("Статус", status_label(r$status)),
    fact("Впевненість відповідника", if (!is.na(r$confidence)) paste0(r$confidence, " з 3")),
    if (has_s) fact("Перша згадка", s$first_mention[1]),
    fact("Аркуш карти", r$sheet),
    fact("Координати", if (!is.na(r$lat) && !is.na(r$lon)) sprintf("%.5f, %.5f", r$lat, r$lon))
  ))
  body <- c(nav, "", "| Поле | Значення |", "|---|---|", unlist(facts))
  if (has(r$note)) body <- c(body, "", "## Примітки", "", md(r$note))

  ids <- if (has(r$source_id)) trimws(strsplit(r$source_id, "[;,]")[[1]]) else character(0)
  if (length(ids) > 0) {
    lines <- vapply(ids, function(sid) {
      m <- if (!is.null(src)) src[which(src$id == sid), ] else NULL
      if (is.null(m) || nrow(m) == 0) return(paste0("- ", md(sid)))
      per <- if (has(m$period[1])) paste0(" (", md(m$period[1]), ")") else ""
      url <- if (has(m$url[1]) && grepl("^https?://", m$url[1])) paste0(" [посилання](", m$url[1], ")") else ""
      paste0("- ", md(sid), ": ", md(m$title[1]), per, url)
    }, character(1))
    body <- c(body, "", "## Джерела", "", unname(lines))
  }
  con <- file(file.path("objects", paste0(id, ".qmd")), "w", encoding = "UTF-8")
  writeLines(c(fm, "", body), con); close(con)
}
message("Створено сторінок об'єктів: ", nrow(ins))
