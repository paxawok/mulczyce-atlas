# Спільні налаштування й функції для карти та реєстру.
# Підключається з кожного .qmd: source("R/helpers.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(leaflet)
  library(htmltools)
})

# ---- Налаштування карти ------------------------------------------------
# tiles_url можна підмінити змінною середовища TILES_URL, якщо тайли
# лежать на окремому хостингу, наприклад:
#   TILES_URL="https://example.com/tiles/wig1931/{z}/{x}/{y}.png"
CFG <- list(
  tiles_url  = Sys.getenv("TILES_URL", "tiles/wig1931/{z}/{x}/{y}.png"),
  # Початковий вигляд. Замініть на координати Мульчиць або центр з build_tiles.sh.
  center     = c(lng = 25.80, lat = 51.4678),
  zoom       = 12,
  min_zoom   = 10,   # нижче цього рівня стара карта не показується
  max_native = 15,   # має збігатися з максимальним зумом у build_tiles.sh
  max_zoom   = 18,
  opacity    = 0.65,
  height     = 650,
  attribution = "Карта WIG, 1930-ті"
)

# ---- Статуси -----------------------------------------------------------
STATUS <- tibble::tribble(
  ~status, ~label,              ~color,
  "S",     "Збереглося",        "#2E7D32",
  "M",     "Змінилося",         "#EF6C00",
  "A",     "Зникло",            "#C62828",
  "N",     "З'явилося пізніше", "#1565C0",
  "U",     "Не встановлено",    "#757575"
)

status_label <- function(x) STATUS$label[match(as.character(x), STATUS$status)]
status_color <- function(x) STATUS$color[match(as.character(x), STATUS$status)]

# ---- Читання даних -----------------------------------------------------
read_text_csv <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()),
           na = c("", "NA"), show_col_types = FALSE)
}

to_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))

read_inscriptions <- function(path = "data/inscriptions.csv") {
  read_text_csv(path) |>
    mutate(
      houses     = suppressWarnings(as.integer(houses)),
      confidence = suppressWarnings(as.integer(confidence)),
      lat        = to_num(lat),
      lon        = to_num(lon),
      status     = factor(status, levels = STATUS$status)
    )
}

read_settlements <- function(path = "data/settlements.csv") {
  read_text_csv(path) |>
    mutate(
      houses     = suppressWarnings(as.integer(houses)),
      confidence = suppressWarnings(as.integer(confidence)),
      lat        = to_num(lat),
      lon        = to_num(lon),
      status     = factor(status, levels = STATUS$status)
    )
}

# ---- Карта -------------------------------------------------------------
base_map <- function() {
  leaflet(height = CFG$height, width = "100%",
          options = leafletOptions(minZoom = 8, maxZoom = CFG$max_zoom)) |>
    setView(CFG$center[["lng"]], CFG$center[["lat"]], CFG$zoom) |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Сучасний знімок") |>
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap")
}

add_old_map <- function(map, opacity = CFG$opacity) {
  addTiles(
    map,
    urlTemplate = CFG$tiles_url,
    layerId     = "wig",
    group       = "Карта 1930-х",
    attribution = CFG$attribution,
    options     = tileOptions(
      opacity = opacity, minZoom = CFG$min_zoom, maxZoom = CFG$max_zoom,
      maxNativeZoom = CFG$max_native, noWrap = TRUE
    )
  )
}

make_popup <- function(d) {
  esc <- function(x) htmlEscape(ifelse(is.na(x), "", x))
  paste0(
    "<b>", esc(d$name_hist), "</b>",
    ifelse(is.na(d$name_modern), "", paste0(" &rarr; ", esc(d$name_modern))),
    "<br>", esc(d$type),
    ifelse(is.na(d$houses), "", paste0("<br>Дворів на карті: ", d$houses)),
    "<br>Статус: ", esc(status_label(d$status)),
    ifelse(is.na(d$note), "", paste0("<br><i>", esc(d$note), "</i>"))
  )
}

# Мітки записів, які мають координати. Без координат мітки не малюються.
add_inscriptions <- function(map, d) {
  d <- d |> filter(!is.na(lat), !is.na(lon))
  if (nrow(d) == 0) return(map)
  d$fill <- status_color(d$status)
  map |>
    addCircleMarkers(
      data = d, lng = ~lon, lat = ~lat, radius = 6,
      weight = 1.5, color = "#ffffff", fillColor = ~fill, fillOpacity = 0.9,
      popup = make_popup(d), label = ~name_hist, group = "Написи"
    ) |>
    addLegend(position = "bottomleft", colors = STATUS$color,
              labels = STATUS$label, title = "Статус", opacity = 0.9)
}

# Повзунок прозорості для шару "wig".
add_opacity_slider <- function(map, layer_id = "wig", value = CFG$opacity) {
  js <- sprintf("
function(el, x) {
  var map = this;
  var ctl = L.control({position: 'topright'});
  ctl.onAdd = function() {
    var d = L.DomUtil.create('div', 'leaflet-bar opacity-ctl');
    d.innerHTML = '<label>Прозорість карти 1930-х</label><input type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"%s\">';
    L.DomEvent.disableClickPropagation(d);
    d.querySelector('input').addEventListener('input', function(e) {
      var layer = map.layerManager.getLayer('tile', '%s');
      if (layer) { layer.setOpacity(parseFloat(e.target.value)); }
    });
    return d;
  };
  ctl.addTo(map);
}", value, layer_id)
  htmlwidgets::onRender(map, js)
}
