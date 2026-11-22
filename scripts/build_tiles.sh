#!/usr/bin/env bash
# Перетворює GeoTIFF зі старою картою на XYZ-тайли для Leaflet.
#
# Використання (з кореня проєкту):
#   scripts/build_tiles.sh шлях/до/карта.tif [minzoom] [maxzoom] [nodata]
#
# Приклади:
#   scripts/build_tiles.sh data/raw/wig_merged.tif
#   scripts/build_tiles.sh data/raw/wig_merged.tif 11 15
#   scripts/build_tiles.sh data/raw/wig_merged.tif 11 15 "255 255 255"
#
# Четвертий аргумент потрібен, якщо растр не має альфа-каналу, а поля навколо
# карти залиті суцільним кольором: цей колір стане прозорим.
# Потрібні GDAL (gdalwarp, gdalinfo, gdal2tiles).

set -euo pipefail

IN="${1:?Вкажіть вхідний GeoTIFF: scripts/build_tiles.sh файл.tif [minzoom] [maxzoom] [nodata]}"
MINZ="${2:-11}"
MAXZ="${3:-15}"
NODATA="${4:-}"
OUT="tiles/wig1931"

[[ -f "$IN" ]] || { echo "Файл не знайдено: $IN" >&2; exit 1; }
command -v gdalwarp >/dev/null 2>&1 || { echo "Не знайдено gdalwarp. Встановіть GDAL." >&2; exit 1; }
command -v gdalinfo >/dev/null 2>&1 || { echo "Не знайдено gdalinfo. Встановіть GDAL." >&2; exit 1; }

if command -v gdal2tiles.py >/dev/null 2>&1; then
  G2T="gdal2tiles.py"
elif command -v gdal2tiles >/dev/null 2>&1; then
  G2T="gdal2tiles"
else
  echo "Не знайдено gdal2tiles. Встановіть GDAL з Python-скриптами." >&2
  exit 1
fi

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

echo "==> Перепроєктування в EPSG:3857"
WARP=(-t_srs EPSG:3857 -r bilinear -dstalpha -multi
      -wo NUM_THREADS=ALL_CPUS -co TILED=YES -co COMPRESS=DEFLATE)
if gdalinfo "$IN" | grep -q "ColorInterp=Alpha"; then
  WARP+=(-srcalpha)
fi
if [[ -n "$NODATA" ]]; then
  WARP+=(-srcnodata "$NODATA")
fi
gdalwarp "${WARP[@]}" "$IN" "$TMP/warped.tif"

echo "==> Генерація тайлів (зуми ${MINZ}-${MAXZ}) у $OUT"
rm -rf "$OUT"
mkdir -p "$(dirname "$OUT")"
PROCS="$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2)"
"$G2T" --xyz -z "${MINZ}-${MAXZ}" -r bilinear --processes="$PROCS" -w none \
  "$TMP/warped.tif" "$OUT"

echo
echo "==> Готово"
echo "Тайлів: $(find "$OUT" -name '*.png' | wc -l)"
echo "Розмір: $(du -sh "$OUT" | cut -f1)"
echo
echo "Охоплення (для CFG\$center у R/helpers.R):"
gdalinfo "$TMP/warped.tif" | grep -E "Upper Left|Lower Right|Center" || true
echo
echo "У R/helpers.R має бути max_native = ${MAXZ}."
