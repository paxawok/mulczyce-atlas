# Завантажує вкладки Google-таблиці в data/*.csv.
#
# Використання (з кореня проєкту):
#   Rscript scripts/sync_sheet.R
#
# Якщо таблиця відкрита за посиланням ("Усі, хто має посилання"), вхід не потрібен.
# Якщо таблиця приватна, запустіть з GS4_AUTH=1 і увійдіть через браузер:
#   GS4_AUTH=1 Rscript scripts/sync_sheet.R
# Інший варіант: вручну експортувати вкладки в CSV (Файл > Завантажити > CSV)
# і покласти їх як data/inscriptions.csv та data/settlements.csv.

suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
  library(readr)
})

if (!file.exists("_quarto.yml")) {
  stop("Запускайте скрипт з кореня проєкту (там, де _quarto.yml).")
}

SHEET <- Sys.getenv(
  "MULCZYCE_SHEET",
  "https://docs.google.com/spreadsheets/d/1W-qr4bJFzjFhkZWWGJCvfbqD9q5yoQl9TOSbo1s5AE8/edit"
)

if (nzchar(Sys.getenv("GS4_AUTH"))) gs4_auth() else gs4_deauth()

# Назви колонок задаються за позицією, тому заголовки в таблиці можна
# змінювати, але порядок колонок має лишатися.
INS_COLS <- c("id", "name_hist", "type", "sheet", "houses", "name_modern",
              "status", "confidence", "lat", "lon", "obsidian_note",
              "source_id", "note", "post_url")
SET_COLS <- c("id", "inscription_id", "name_hist", "name_modern", "variants",
              "type", "houses", "first_mention", "status", "confidence",
              "lat", "lon", "obsidian_note", "source_ids", "note")
SRC_COLS <- c("id", "title", "period", "type", "url", "archive_ref", "notes",
              "obsidian_note")

read_tab <- function(sheet, cols) {
  x <- tryCatch(
    read_sheet(SHEET, sheet = sheet, col_types = "c", .name_repair = "minimal"),
    error = function(e) {
      stop("Не вдалося прочитати вкладку «", sheet, "»: ", conditionMessage(e),
           "\nПеревірте доступ до таблиці або запустіть з GS4_AUTH=1.", call. = FALSE)
    }
  )
  x <- x[, seq_len(min(ncol(x), length(cols))), drop = FALSE]
  names(x) <- cols[seq_len(ncol(x))]
  for (m in setdiff(cols, names(x))) x[[m]] <- NA_character_
  x[, cols] |>
    mutate(across(everything(), ~ na_if(trimws(.x), ""))) |>
    filter(!is.na(id))
}

to_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))

check_coords <- function(x, label) {
  lat <- to_num(x$lat); lon <- to_num(x$lon)
  bad_parse <- (!is.na(x$lat) & is.na(lat)) | (!is.na(x$lon) & is.na(lon))
  # Груба перевірка, що точка лежить у межах Волині та Полісся.
  out <- !is.na(lat) & !is.na(lon) & (lat < 49 | lat > 53 | lon < 23 | lon > 28)
  swapped <- !is.na(lat) & !is.na(lon) & lat > 20 & lon < 30 & lat > lon
  for (i in which(bad_parse | out | swapped)) {
    warning(label, " ", x$id[i], ": підозрілі координати (", x$lat[i], ", ", x$lon[i],
            "). Широта має бути близько 51, довгота близько 25-26.", call. = FALSE)
  }
}

check_status <- function(x, label) {
  bad <- !is.na(x$status) & !(x$status %in% c("S", "M", "A", "N", "U"))
  for (i in which(bad)) {
    warning(label, " ", x$id[i], ": невідомий статус «", x$status[i], "».", call. = FALSE)
  }
}

ins <- read_tab("Написи на карті", INS_COLS)
sett <- read_tab("Населені пункти", SET_COLS)
srcs <- read_tab("Джерела", SRC_COLS)

check_coords(ins, "Напис"); check_status(ins, "Напис")
check_coords(sett, "Пункт"); check_status(sett, "Пункт")

dir.create("data", showWarnings = FALSE)
write_csv(ins, "data/inscriptions.csv", na = "")
write_csv(sett, "data/settlements.csv", na = "")
write_csv(srcs, "data/sources.csv", na = "")

message("Збережено: data/inscriptions.csv (", nrow(ins), " рядків), ",
        "data/settlements.csv (", nrow(sett), " рядків).")
message("З координатами: ", sum(!is.na(to_num(ins$lat)) & !is.na(to_num(ins$lon))),
        " записів у написах.")
