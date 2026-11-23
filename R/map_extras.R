# Додаткові функції карти: мітки за статусами, підписи на великих зумах, шторка.
# Підключається після helpers.R:
#   source("R/helpers.R"); source("R/map_extras.R")
# Не потребує пакета leaflet.extras2.

# Спливне вікно: поля з helpers.R::make_popup() + посилання на пост, якщо
# в даних є колонка post_url і значення починається з https://
popup_html <- function(d) {
  link <- rep("", nrow(d))
  if ("post_url" %in% names(d)) {
    ok <- !is.na(d$post_url) & grepl("^https://", d$post_url)
    link[ok] <- paste0(
      "<br><a href=\"", htmlEscape(d$post_url[ok], attribute = TRUE),
      "\" target=\"_blank\" rel=\"noopener\">Читати в каналі</a>"
    )
  }
  paste0(make_popup(d), link)
}

# Назви груп, які створює add_markers(), для addLayersControl().
marker_groups <- function(d) {
  d <- d |> filter(!is.na(lat), !is.na(lon))
  if (nrow(d) == 0) return(character(0))
  present <- STATUS$status[STATUS$status %in% as.character(d$status)]
  c("Підписи", paste0("Написи: ", status_label(present)))
}

# Кружечки: колір за статусом, розмір за кількістю дворів, окремий шар
# на кожен статус, підписи назв з'являються з певного зуму.
add_markers <- function(map, d, label_zoom = 13:18) {
  d <- d |> filter(!is.na(lat), !is.na(lon))
  if (nrow(d) == 0) return(map)

  d$r_px  <- ifelse(is.na(d$houses), 5, pmin(18, 5 + 0.9 * sqrt(d$houses)))
  d$fill  <- status_color(d$status)
  d$pop   <- popup_html(d)

  for (s in STATUS$status) {
    part <- d[!is.na(d$status) & as.character(d$status) == s, ]
    if (nrow(part) == 0) next
    map <- addCircleMarkers(
      map, data = part, lng = ~lon, lat = ~lat, radius = ~r_px,
      weight = 2, color = "#ffffff", fillColor = ~fill, fillOpacity = 0.9,
      popup = ~pop, popupOptions = popupOptions(maxWidth = 280),
      label = ~name_hist, group = paste0("Написи: ", status_label(s))
    )
  }

  map |>
    addLabelOnlyMarkers(
      data = d, lng = ~lon, lat = ~lat, label = ~name_hist, group = "Підписи",
      labelOptions = labelOptions(noHide = TRUE, direction = "right",
                                  textOnly = TRUE, className = "mulczyce-label",
                                  offset = c(10, 0))
    ) |>
    groupOptions("Підписи", zoomLevels = label_zoom) |>
    addLegend(position = "bottomleft", colors = STATUS$color,
              labels = STATUS$label, title = "Статус", opacity = 0.9)
}

# Карта для шторки: лише сучасний знімок і стара карта поверх (непрозора).
swipe_map <- function() {
  leaflet(height = CFG$height, width = "100%",
          options = leafletOptions(minZoom = 8, maxZoom = CFG$max_zoom)) |>
    setView(CFG$center[["lng"]], CFG$center[["lat"]], CFG$zoom) |>
    addProviderTiles(providers$Esri.WorldImagery) |>
    add_old_map(opacity = 1)
}

# Шторка на чистому Leaflet: шар "wig" обрізається CSS clip праворуч від роздільника.
add_swipe <- function(map, layer_id = "wig", left = "Сучасність",
                      right = "1930-ті", start = 0.5) {
  js <- sprintf("
function(el, x) {
  var map = this;
  var layer = map.layerManager.getLayer('tile', '%s');
  if (!layer) { return; }
  var ratio = %s;
  var holder = map.getContainer();

  var divider = L.DomUtil.create('div', 'swipe-divider', holder);
  L.DomUtil.create('div', 'swipe-handle', divider);
  var capL = L.DomUtil.create('div', 'swipe-cap swipe-cap-left', holder);
  capL.textContent = '%s';
  var capR = L.DomUtil.create('div', 'swipe-cap swipe-cap-right', holder);
  capR.textContent = '%s';
  L.DomEvent.disableClickPropagation(divider);

  function apply() {
    var size = map.getSize();
    var nw = map.containerPointToLayerPoint([0, 0]);
    var se = map.containerPointToLayerPoint(size);
    var cx = nw.x + (se.x - nw.x) * ratio;
    var c = layer.getContainer();
    if (c) { c.style.clip = 'rect(' + [nw.y, se.x, se.y, cx].join('px,') + 'px)'; }
    divider.style.left = (size.x * ratio) + 'px';
  }

  var dragging = false;
  function move(e) {
    if (!dragging) { return; }
    var r = holder.getBoundingClientRect();
    ratio = Math.min(1, Math.max(0, (e.clientX - r.left) / r.width));
    apply();
  }
  function stop() {
    if (!dragging) { return; }
    dragging = false;
    map.dragging.enable();
  }
  divider.addEventListener('pointerdown', function(e) {
    dragging = true;
    map.dragging.disable();
    if (divider.setPointerCapture) { divider.setPointerCapture(e.pointerId); }
    e.preventDefault();
  });
  divider.addEventListener('pointermove', move);
  divider.addEventListener('pointerup', stop);
  divider.addEventListener('pointercancel', stop);

  map.on('move zoom resize', apply);
  map.whenReady(apply);
  setTimeout(apply, 0);
}", layer_id, format(start), left, right)
  htmlwidgets::onRender(map, js)
}
