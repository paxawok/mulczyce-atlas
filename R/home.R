# Дані й HTML-блоки головної сторінки.
# Підключається після helpers.R:
#   source("R/helpers.R"); source("R/home.R")
#
# Нових таблиць не потрібно:
#   населені пункти  <- data/inscriptions.csv (записи з type = "населений пункт")
#   наповненість     <- data/timeline.csv (колонка object_id)
#   каталог джерел   <- data/sources.csv
# Додали пункт у реєстр, і він з'явиться на головній карті та серед карток.

PLACE_TYPES <- c("населений пункт")
CHANNEL_URL <- "https://t.me/mulczyce_oo"

h_esc  <- function(x) htmlEscape(ifelse(is.na(x), "", as.character(x)))
h_attr <- function(x) htmlEscape(ifelse(is.na(x), "", as.character(x)), attribute = TRUE)
has_val <- function(x) !is.na(x) & nzchar(as.character(x))

# Українська множина: 1 двір, 2 двори, 5 дворів.
plural_uk <- function(n, one, few, many) {
  n <- abs(as.integer(n)); m10 <- n %% 10L; m100 <- n %% 100L
  ifelse(m100 >= 11L & m100 <= 14L, many,
         ifelse(m10 == 1L, one, ifelse(m10 >= 2L & m10 <= 4L, few, many)))
}

place_title <- function(d) ifelse(has_val(d$name_modern), d$name_modern, d$name_hist)

# ---- Дані ---------------------------------------------------------------
home_data <- function() {
  ins <- read_inscriptions()
  tl  <- if (file.exists("data/timeline.csv")) read_text_csv("data/timeline.csv") else NULL
  src <- if (file.exists("data/sources.csv"))  read_text_csv("data/sources.csv")  else NULL

  ids <- if (is.null(tl)) list() else
    strsplit(ifelse(is.na(tl$object_id), "", tl$object_id), "\\s*;\\s*")
  rows_of <- function(id) which(vapply(ids, function(v) id %in% v, logical(1)))

  places <- ins |> filter(type %in% PLACE_TYPES)
  rows <- lapply(places$id, rows_of)
  places$n_rec <- vapply(rows, length, integer(1))
  years <- lapply(rows, function(ix) {
    y <- suppressWarnings(as.integer(substr(tl$date[ix], 1, 4)))
    y[!is.na(y)]
  })
  places$y_min <- vapply(years, function(y) if (length(y)) min(y) else NA_integer_, integer(1))
  places$y_max <- vapply(years, function(y) if (length(y)) max(y) else NA_integer_, integer(1))

  # Спершу пункти з даними, далі за кількістю дворів, потім за назвою.
  places <- places |> arrange(desc(n_rec > 0), desc(n_rec), desc(houses), name_hist)
  list(ins = ins, places = places, tl = tl, src = src)
}

# ---- Блок із цифрами ----------------------------------------------------
stats_html <- function(h) {
  n_p <- nrow(h$places)
  n_d <- sum(h$places$n_rec > 0)
  n_t <- if (is.null(h$tl)) 0L else nrow(h$tl)
  n_s <- if (is.null(h$src)) 0L else nrow(h$src)
  item <- function(n, label) paste0('<div class="home-stat"><b>', n, '</b><span>', label, '</span></div>')
  paste0('<div class="home-stats">',
    item(n_p, plural_uk(n_p, "населений пункт", "населені пункти", "населених пунктів")),
    item(n_d, plural_uk(n_d, "пункт із хронологією", "пункти з хронологією", "пунктів із хронологією")),
    item(n_t, plural_uk(n_t, "запис у хронології", "записи в хронології", "записів у хронології")),
    item(n_s, plural_uk(n_s, "джерело в каталозі", "джерела в каталозі", "джерел у каталозі")),
    '</div>')
}

# ---- Оглядова карта -----------------------------------------------------
place_popup <- function(d) {
  title  <- place_title(d)
  modern <- has_val(d$name_modern)
  st  <- as.character(d$status)
  col <- status_color(st); col[is.na(col)] <- "#999999"
  hs  <- as.integer(d$houses)
  row <- function(label, val) paste0("<tr><th>", label, "</th><td>", val, "</td></tr>")
  paste0(
    '<div class="obj-card"><div class="obj-title">', h_esc(title), '</div>',
    ifelse(modern, paste0('<div class="obj-sub">на карті 1930-х: ', h_esc(d$name_hist), '</div>'), ''),
    '<table>',
    ifelse(is.na(st), '', row("Статус", paste0('<span class="obj-dot" style="background:', col, '"></span>', status_label(st)))),
    ifelse(is.na(hs), '', row("Дворів", hs)),
    ifelse(d$n_rec > 0,
           row("Хронологія", paste0(d$n_rec, " ", plural_uk(d$n_rec, "запис", "записи", "записів"))),
           row("Хронологія", "дані збираються")),
    '</table><div class="obj-links"><a class="obj-btn" href="objects/', h_attr(d$id), '.html">Відкрити сторінку</a></div></div>'
  )
}

overview_map <- function(places, height = 480) {
  p <- places |> filter(!is.na(lat), !is.na(lon))
  map <- leaflet(height = height, width = "100%",
                 options = leafletOptions(minZoom = 8, maxZoom = CFG$max_zoom)) |>
    setView(CFG$center[["lng"]], CFG$center[["lat"]], CFG$zoom) |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Сучасний знімок") |>
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") |>
    add_old_map(opacity = 0.65) |>
    hideGroup("Карта 1930-х")
  if (nrow(p) == 0) return(map)

  p$r_px <- ifelse(is.na(p$houses), 6, pmin(16, 6 + 0.8 * sqrt(p$houses)))
  p$pop  <- place_popup(p)
  p$lab  <- place_title(p)
  has <- p$n_rec > 0

  pop_opts <- popupOptions(maxWidth = 300, minWidth = 220)
  groups <- character(0)
  if (any(!has)) {
    map <- addCircleMarkers(map, data = p[!has, ], lng = ~lon, lat = ~lat, radius = ~r_px,
      layerId = ~id, weight = 2.5, color = "#1F3864", fillColor = "#ffffff", fillOpacity = 0.9,
      popup = ~pop, popupOptions = pop_opts, label = ~lab, group = "Дані збираються")
    groups <- c(groups, "Дані збираються")
  }
  if (any(has)) {
    map <- addCircleMarkers(map, data = p[has, ], lng = ~lon, lat = ~lat, radius = ~r_px,
      layerId = ~id, weight = 2.5, color = "#ffffff", fillColor = "#1F3864", fillOpacity = 0.95,
      popup = ~pop, popupOptions = pop_opts, label = ~lab, group = "Є хронологія")
    groups <- c(groups, "Є хронологія")
  }
  map <- map |>
    addLabelOnlyMarkers(data = p, lng = ~lon, lat = ~lat, label = ~lab, group = "Підписи",
      labelOptions = labelOptions(noHide = TRUE, direction = "right", textOnly = TRUE,
                                  className = "mulczyce-label", offset = c(10, 0))) |>
    groupOptions("Підписи", zoomLevels = 12:18) |>
    addLayersControl(baseGroups = c("Сучасний знімок", "OpenStreetMap"),
                     overlayGroups = c("Карта 1930-х", groups, "Підписи"),
                     options = layersControlOptions(collapsed = TRUE))
  if (nrow(p) > 1) {
    map <- fitBounds(map, min(p$lon), min(p$lat), max(p$lon), max(p$lat),
                     options = list(padding = c(30, 30), maxZoom = 14))
  }
  map
}

map_caption_html <- function(places) {
  n <- nrow(places)
  n_xy <- sum(!is.na(places$lat) & !is.na(places$lon))
  miss <- if (n_xy < n) paste0(
    "<span>Ще ", n - n_xy, " ", plural_uk(n - n_xy, "пункт", "пункти", "пунктів"),
    " без координат у реєстрі, тому на карті їх немає.</span>") else ""
  paste0('<div class="map-note">',
    '<span><i class="lg-dot on"></i>Є хронологія</span>',
    '<span><i class="lg-dot off"></i>Дані збираються</span>',
    '<span>Розмір кружечка залежить від кількості дворів на карті 1930-х.</span>',
    miss, '</div>')
}

# ---- Картки населених пунктів ------------------------------------------
place_cards_html <- function(places) {
  if (nrow(places) == 0) return("")
  title  <- place_title(places)
  modern <- has_val(places$name_modern)
  sub <- ifelse(modern, paste0("на карті 1930-х: ", places$name_hist), "сучасну назву ще не встановлено")

  hs   <- as.integer(places$houses)
  line <- ifelse(is.na(hs), "",
                 paste0(hs, " ", plural_uk(hs, "двір", "двори", "дворів"), " на карті 1930-х"))

  st  <- as.character(places$status)
  col <- status_color(st); col[is.na(col)] <- "#999999"
  badge <- ifelse(is.na(st), "",
                  paste0('<span class="pc-status"><i style="background:', col, '"></i>', status_label(st), '</span>'))

  yr <- ifelse(is.na(places$y_min), "",
        ifelse(places$y_min == places$y_max, paste0(" · ", places$y_min),
               paste0(" · ", places$y_min, "–", places$y_max)))
  cov <- ifelse(places$n_rec > 0,
    paste0('<div class="pc-cov">', places$n_rec, " ",
           plural_uk(places$n_rec, "запис", "записи", "записів"), " у хронології", yr, '</div>'),
    '<div class="pc-cov pc-empty">Дані збираються</div>')

  q <- paste(ifelse(is.na(places$name_hist), "", places$name_hist),
             ifelse(is.na(places$name_modern), "", places$name_modern), places$id)

  paste0('<a class="pc ', ifelse(places$n_rec > 0, "has", "none"), '" href="objects/', h_attr(places$id),
         '.html" data-has="', as.integer(places$n_rec > 0), '" data-q="', h_attr(q), '">',
         '<div class="pc-title">', h_esc(title), '</div>',
         '<div class="pc-sub">', h_esc(sub), '</div>',
         ifelse(nzchar(line), paste0('<div class="pc-line">', line, '</div>'), ""),
         cov, '<div class="pc-badges">', badge, '</div></a>')
}

places_section_html <- function(places) {
  n  <- nrow(places)
  n1 <- sum(places$n_rec > 0)
  js <- paste0("<script>(function(){",
    "var root=document.getElementById('places');if(!root){return;}",
    "var input=root.querySelector('.pc-search'),chips=root.querySelectorAll('.tl-chip'),",
    "cards=root.querySelectorAll('.pc'),msg=root.querySelector('.pc-none-msg'),mode='all';",
    "function apply(){var q=input.value.trim().toLowerCase(),shown=0;",
    "cards.forEach(function(c){var ok=(mode==='all')||(mode==='has'&&c.getAttribute('data-has')==='1')||(mode==='none'&&c.getAttribute('data-has')==='0');",
    "if(ok&&q){ok=(c.getAttribute('data-q')||'').toLowerCase().indexOf(q)>-1;}",
    "c.style.display=ok?'':'none';if(ok){shown++;}});",
    "msg.style.display=shown?'none':'block';}",
    "input.addEventListener('input',apply);",
    "chips.forEach(function(b){b.addEventListener('click',function(){",
    "chips.forEach(function(o){o.classList.remove('active');});b.classList.add('active');",
    "mode=b.getAttribute('data-mode');apply();});});})();</script>")
  paste0('<div id="places"><div class="pc-tools">',
    '<input type="search" class="pc-search" placeholder="Знайти населений пункт" aria-label="Знайти населений пункт">',
    '<button type="button" class="tl-chip active" data-mode="all">Усі ', n, '</button>',
    '<button type="button" class="tl-chip" data-mode="has">Є хронологія ', n1, '</button>',
    '<button type="button" class="tl-chip" data-mode="none">Дані збираються ', n - n1, '</button>',
    '</div><div class="pc-grid">', paste(place_cards_html(places), collapse = ""), '</div>',
    '<div class="pc-none-msg">Нічого не знайдено.</div>', js, '</div>')
}

# ---- Відкриті питання ---------------------------------------------------
# Береться з нотаток хронології: «Уточнити …», «… звірити …».
open_questions_html <- function(h, max_n = 6) {
  tl <- h$tl
  if (is.null(tl) || nrow(tl) == 0) return("")
  note <- ifelse(is.na(tl$note), "", tl$note)
  hit <- head(which(grepl("^Уточнити|звірити", note, ignore.case = TRUE)), max_n)
  if (!length(hit)) return("")
  nm <- setNames(place_title(h$ins), h$ins$id)

  li <- vapply(hit, function(i) {
    oid <- trimws(strsplit(ifelse(is.na(tl$object_id[i]), "", tl$object_id[i]), ";")[[1]])[1]
    place <- if (!is.na(oid) && oid %in% names(nm)) nm[[oid]] else ""
    when  <- if (has_val(tl$date_label[i])) tl$date_label[i] else if (has_val(tl$date[i])) tl$date[i] else ""
    meta  <- paste(Filter(nzchar, c(place, when)), collapse = " · ")
    posts <- if (has_val(tl$post[i])) trimws(strsplit(tl$post[i], ";")[[1]]) else character(0)
    posts <- posts[grepl("^[0-9]+$", posts)]
    links <- c(
      if (length(posts)) paste0('<a href="', CHANNEL_URL, '/', posts[1], '" target="_blank" rel="noopener">Обговорити в каналі</a>'),
      if (!is.na(oid) && nzchar(oid)) paste0('<a href="objects/', h_attr(oid), '.html">Сторінка об\'єкта</a>'))
    paste0('<li><div class="oq-meta">', h_esc(meta), '</div>',
           '<div class="oq-title">', h_esc(tl$title[i]), '</div>',
           '<div class="oq-note">', h_esc(note[i]), '</div>',
           if (length(links)) paste0('<div class="oq-links">', paste(links, collapse = " "), '</div>') else "",
           '</li>')
  }, character(1))

  paste0('<h2>Що ще треба з\'ясувати</h2>',
         '<p class="home-sub">Знаєте відповідь або маєте документ? Напишіть у <a href="', CHANNEL_URL,
         '" target="_blank" rel="noopener">каналі</a>.</p>',
         '<ul class="oq">', paste(li, collapse = ""), '</ul>')
}

# ---- Про проєкт ---------------------------------------------------------
about_html <- function(h) {
  done <- place_title(h$places[h$places$n_rec > 0, ])
  now <- if (length(done)) paste0(
    '<p>Докладно описано: <b>', paste(h_esc(done), collapse = ", "),
    '</b>. Решта пунктів з\'являться в міру збору даних.</p>') else ""
  paste0(
    '<p>Тут в одному місці збираються дані про села й урочища Вараської громади та їхні околиці: ',
    'карти різних років, документи, преса, спогади й публікації. Основа проєкту: порівняння аркушів ',
    'польської карти WIG (1:100 000, 1930-ті) із сучасними знімками.</p>',
    '<p>Записи хронології зібрано з архівних документів, преси, карт і постів каналу. ',
    'Позначка «не перевірено» означає, що запис ще не звірено з першоджерелом.</p>',
    now,
    '<p><a href="metodyka.html">Методика й обмеження</a> · ',
    '<a href="', CHANNEL_URL, '" target="_blank" rel="noopener">Канал у Telegram</a></p>')
}
