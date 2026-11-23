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
tl   <- if (file.exists("data/timeline.csv")) read_text_csv("data/timeline.csv") else NULL

# ---- Хронологічна лінія ---------------------------------------------------
# Рядки data/timeline.csv, у яких object_id містить ID запису (кілька ID
# розділяються ";"). Поля: date (YYYY, YYYY-MM або YYYY-MM-DD, порожнє = без
# дати), date_label (підпис), category, title, summary, url, post (номери постів
# каналу через ";"), scope ("околиці" для подій поруч із селом), verified
# ("так" прибирає позначку "не перевірено"), note.
CAT_CLASS <- c("документ" = "doc", "преса" = "press", "карта" = "map",
               "подія" = "event", "особа" = "person", "усна історія" = "oral",
               "публікація" = "pub", "місце" = "place", "сучасність" = "modern")
CHANNEL <- "https://t.me/mulczyce_oo/"

timeline_rows <- function(id) {
  if (is.null(tl) || nrow(tl) == 0) return(NULL)
  hit <- vapply(strsplit(ifelse(is.na(tl$object_id), "", tl$object_id), "\\s*;\\s*"),
                function(v) id %in% v, logical(1))
  x <- tl[hit, , drop = FALSE]
  if (nrow(x) == 0) return(NULL)
  x$date[is.na(x$date)] <- ""
  x[order(x$date == "", x$date), , drop = FALSE]
}

timeline_html <- function(x) {
  e <- function(v) if (has(v)) htmlEscape(v) else ""
  cats <- unique(x$category[!is.na(x$category)])
  chips <- paste0("<button type=\"button\" class=\"tl-chip active\" data-cat=\"all\">Усі</button>",
    paste0("<button type=\"button\" class=\"tl-chip\" data-cat=\"", htmlEscape(cats, attribute = TRUE),
           "\">", htmlEscape(cats), "</button>", collapse = ""))
  out <- character(0); last_head <- ""
  for (i in seq_len(nrow(x))) {
    r <- x[i, ]
    yr <- suppressWarnings(as.integer(substr(r$date, 1, 4)))
    head <- if (is.na(yr)) "Без точної дати" else paste0(as.character(utils::as.roman((yr - 1) %/% 100 + 1)), " століття")
    if (head != last_head) { out <- c(out, paste0("<li class=\"tl-century\">", head, "</li>")); last_head <- head }
    cls <- unname(CAT_CLASS[r$category]); if (is.na(cls)) cls <- "other"
    badges <- paste0(
      if (has(r$scope)) paste0("<span class=\"tl-badge\">", e(r$scope), "</span>") else "",
      if (!identical(tolower(trimws(r$verified)), "так")) "<span class=\"tl-badge tl-unv\">не перевірено</span>" else "")
    links <- character(0)
    if (has(r$url) && grepl("^https?://", r$url))
      links <- c(links, paste0("<a href=\"", htmlEscape(r$url, attribute = TRUE), "\" target=\"_blank\" rel=\"noopener\">Джерело</a>"))
    posts <- if (has(r$post)) trimws(strsplit(r$post, ";")[[1]]) else character(0)
    posts <- posts[grepl("^[0-9]+$", posts)]
    for (pn in posts)
      links <- c(links, paste0("<a href=\"", CHANNEL, pn, "\" target=\"_blank\" rel=\"noopener\">",
                               if (length(posts) == 1) "Пост у каналі" else paste0("Пост ", pn), "</a>"))
    out <- c(out, paste0(
      "<li class=\"tl-item tl-", cls, "\" data-cat=\"", htmlEscape(ifelse(is.na(r$category), "", r$category), attribute = TRUE), "\">",
      "<span class=\"tl-dot\"></span><div class=\"tl-card\">",
      "<div class=\"tl-date\">", e(ifelse(has(r$date_label), r$date_label, r$date)), "</div>",
      "<div class=\"tl-meta\"><span class=\"tl-cat\">", e(r$category), "</span>", badges, "</div>",
      "<div class=\"tl-title\">", e(r$title), "</div>",
      if (has(r$summary)) paste0("<p class=\"tl-sum\">", e(r$summary), "</p>") else "",
      if (has(r$note)) paste0("<p class=\"tl-note\">", e(r$note), "</p>") else "",
      if (length(links)) paste0("<div class=\"tl-links\">", paste(links, collapse = ""), "</div>") else "",
      "</div></li>"))
  }
  js <- paste0("<script>(function(){var w=document.currentScript.parentNode;",
    "var b=w.querySelectorAll('.tl-chip'),it=w.querySelectorAll('.tl-item');",
    "b.forEach(function(c){c.addEventListener('click',function(){",
    "b.forEach(function(o){o.classList.remove('active')});c.classList.add('active');",
    "var k=c.getAttribute('data-cat');",
    "it.forEach(function(i){i.style.display=(k==='all'||i.getAttribute('data-cat')===k)?'':'none'});",
    "});});})();</script>")
  c("```{=html}", paste0("<div class=\"tl-wrap\"><div class=\"tl-filters\">", chips, "</div>",
      "<ol class=\"tl\">", paste(out, collapse = ""), "</ol>", js, "</div>"), "```")
}

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

  trows <- timeline_rows(id)
  if (!is.null(trows)) {
    body <- c(body, "", "## Хронологія згадок", "",
              "Події, документи, карти й публікації, пов'язані з об'єктом. Позначка «не перевірено» означає, що запис зібрано з постів каналу й ще не звірено з першоджерелом.",
              "", timeline_html(trows), "")
  }

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
