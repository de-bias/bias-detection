#!/usr/bin/env python3
"""Build responsive crops from the accepted radial feature-importance figure."""

from __future__ import annotations

import hashlib
import json
import sys
import tempfile
from pathlib import Path
from typing import Any

try:
    from PIL import Image, ImageDraw
except ImportError as exc:  # pragma: no cover
    raise SystemExit("Pillow is required to build the accepted-figure radial crops.") from exc


SCRIPT_DIR = Path(__file__).resolve().parent
SITE_DIR = SCRIPT_DIR.parent
REPOSITORY_ROOT = SITE_DIR.parents[3]
MANIFEST_PATH = SCRIPT_DIR / "radial_crops_manifest.json"
OUTPUT_DIR = SITE_DIR / "media" / "radial"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def load_manifest() -> dict[str, Any]:
    with MANIFEST_PATH.open(encoding="utf-8") as stream:
        manifest = json.load(stream)
    if manifest.get("schema_version") != 1:
        raise ValueError("Unsupported radial crop manifest schema")
    return manifest


def validated_rect(value: list[int], source_size: tuple[int, int]) -> tuple[int, int, int, int]:
    if len(value) != 4 or not all(isinstance(item, int) for item in value):
        raise ValueError(f"Invalid crop rectangle: {value!r}")
    left, top, right, bottom = value
    width, height = source_size
    if not (0 <= left < right <= width and 0 <= top < bottom <= height):
        raise ValueError(f"Crop rectangle is outside the accepted figure: {value!r}")
    return left, top, right, bottom


def flatten_on_white(image: Image.Image) -> Image.Image:
    rgba = image.convert("RGBA")
    background = Image.new("RGBA", rgba.size, "white")
    return Image.alpha_composite(background, rgba).convert("RGB")


def compose_crop(source: Image.Image, panel: dict[str, Any], composition: dict[str, Any]) -> Image.Image:
    crop = source.crop(validated_rect(panel["source_rect"], source.size))
    if panel.get("blank_rects"):
        crop_draw = ImageDraw.Draw(crop)
        for blank_rect in panel["blank_rects"]:
            crop_draw.rectangle(validated_rect(blank_rect, crop.size), fill="white")
    canvas_width, canvas_height = composition["canvas_dimensions_px"]
    left, top, right, bottom = composition["plot_bounds_px"]
    available_width = right - left
    available_height = bottom - top
    scale = min(available_width / crop.width, available_height / crop.height)
    crop_size = (max(1, round(crop.width * scale)), max(1, round(crop.height * scale)))
    crop = crop.resize(crop_size, Image.Resampling.LANCZOS)

    canvas = Image.new("RGB", (canvas_width, canvas_height), composition["background"])
    x = left + ((available_width - crop.width) // 2)
    y = top + ((available_height - crop.height) // 2)
    canvas.paste(crop, (x, y))
    return canvas


def save_assets(image: Image.Image, stem: str, destination: Path, encoding: dict[str, Any]) -> list[Path]:
    webp_path = destination / f"{stem}.webp"
    png_path = destination / f"{stem}.png"
    image.save(webp_path, format="WEBP", quality=encoding["webp"]["quality"], method=encoding["webp"]["method"])
    image.quantize(
        colors=encoding["png"]["palette_colours"],
        method=Image.Quantize.MEDIANCUT,
        dither=Image.Dither.NONE,
    ).save(
        png_path,
        format="PNG",
        optimize=encoding["png"]["optimize"],
        compress_level=encoding["png"]["compress_level"],
    )
    return [webp_path, png_path]


def main() -> int:
    manifest = load_manifest()
    source_spec = manifest["source"]
    source_path = REPOSITORY_ROOT / source_spec["path_from_repository_root"]
    if not source_path.is_file():
        raise FileNotFoundError(f"Accepted radial figure not found: {source_path}")
    if sha256(source_path) != source_spec["sha256"]:
        raise ValueError("Accepted radial figure checksum does not match the manifest")

    with Image.open(source_path) as opened:
        expected_size = (source_spec["width_px"], source_spec["height_px"])
        if opened.size != expected_size:
            raise ValueError(f"Accepted radial figure dimensions changed: {opened.size}")
        source = flatten_on_white(opened)

    encoding = manifest["encoding"]
    built: list[Path] = []
    with tempfile.TemporaryDirectory(prefix="accepted-radial-crops-") as temporary:
        temporary_dir = Path(temporary)
        for panel in manifest["panels"]:
            image = compose_crop(source, panel, manifest["composition"])
            expected_dimensions = tuple(manifest["composition"]["canvas_dimensions_px"])
            if image.size != expected_dimensions:
                raise ValueError(f"Generated dimensions changed for {panel['id']}")
            assets = save_assets(image, panel["output_stem"], temporary_dir, encoding)
            built.extend(assets)
            for path in assets:
                extension = path.suffix.removeprefix(".")
                expected = panel.get("expected_outputs", {}).get(extension)
                if expected:
                    if path.name != Path(expected["path"]).name:
                        raise ValueError(f"Generated filename changed for {panel['id']} / {extension}")
                    if path.stat().st_size != expected["bytes"] or sha256(path) != expected["sha256"]:
                        raise ValueError(f"Generated output changed for {path.name}")

        oversized = [path for path in built if path.stat().st_size > encoding["maximum_file_bytes"]]
        if oversized:
            raise ValueError(f"Radial crops exceed the per-file budget: {', '.join(path.name for path in oversized)}")
        total_bytes = sum(path.stat().st_size for path in built)
        if total_bytes > encoding["maximum_total_bytes"]:
            raise ValueError(f"Radial crops total {total_bytes} bytes; budget is {encoding['maximum_total_bytes']}")

        OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
        for path in built:
            path.replace(OUTPUT_DIR / path.name)

    for path in sorted(OUTPUT_DIR.glob("*")):
        if path.suffix in {".png", ".webp"}:
            print(f"{path.name}: {path.stat().st_size} bytes · sha256 {sha256(path)}")
    print(f"Total generated bytes: {total_bytes}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, KeyError, OSError, TypeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1) from error
