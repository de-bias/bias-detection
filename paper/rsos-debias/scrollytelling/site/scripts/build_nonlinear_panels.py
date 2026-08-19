#!/usr/bin/env python3
"""Build responsive and media-ready nonlinear panels from the accepted figure.

The JSON manifest is the source of truth for crop geometry and encoding. The
script verifies the accepted figure checksum before doing any work, writes all
assets to a temporary directory, checks the byte budgets, and only then replaces
the files in ``media/nonlinear``. Empirical marks, axes and fitted curves remain
accepted-figure pixels; the builder only crops, scales and remaps their palette.
Each crop is composed twice: as a plot-forward story asset and as a self-contained
DEBIAS media asset with editorial context, metric guidance and credit.
"""

from __future__ import annotations

import hashlib
import json
import sys
import tempfile
from pathlib import Path
from typing import Any

try:
    import numpy as np
    from PIL import Image, ImageDraw, ImageFont
except ImportError as exc:  # pragma: no cover - depends on the caller's runtime
    raise SystemExit(
        "Pillow and NumPy are required. Run this script with a Python environment "
        "that provides PIL and numpy (the Codex workspace Python includes both)."
    ) from exc


SCRIPT_DIR = Path(__file__).resolve().parent
SITE_DIR = SCRIPT_DIR.parent
REPOSITORY_ROOT = SITE_DIR.parents[3]
MANIFEST_PATH = SCRIPT_DIR / "nonlinear_panels_manifest.json"
OUTPUT_DIR = SITE_DIR / "media" / "nonlinear"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def load_manifest() -> dict[str, Any]:
    with MANIFEST_PATH.open(encoding="utf-8") as stream:
        manifest = json.load(stream)
    if manifest.get("schema_version") != 2:
        raise ValueError("Unsupported nonlinear panel manifest schema")
    return manifest


def rect(value: list[int], source_size: tuple[int, int]) -> tuple[int, int, int, int]:
    if len(value) != 4 or not all(isinstance(item, int) for item in value):
        raise ValueError(f"Invalid crop rectangle: {value!r}")
    left, top, right, bottom = value
    width, height = source_size
    if not (0 <= left < right <= width and 0 <= top < bottom <= height):
        raise ValueError(f"Crop rectangle is outside the source image: {value!r}")
    return left, top, right, bottom


def flatten_on_white(image: Image.Image) -> Image.Image:
    rgba = image.convert("RGBA")
    background = Image.new("RGBA", rgba.size, "white")
    return Image.alpha_composite(background, rgba).convert("RGB")


def rgb(value: str) -> np.ndarray:
    value = value.removeprefix("#")
    if len(value) != 6:
        raise ValueError(f"Invalid RGB colour: {value!r}")
    return np.array([int(value[index : index + 2], 16) for index in (0, 2, 4)], dtype=np.float32)


def blend(start: np.ndarray, end: np.ndarray, amount: np.ndarray) -> np.ndarray:
    return start + ((end - start) * amount[..., None])


def apply_template_palette(image: Image.Image, palette: dict[str, str]) -> Image.Image:
    """Map accepted pixels to the restrained DEBIAS palette without moving them."""
    pixels = np.asarray(image.convert("RGB"), dtype=np.float32)
    maximum = pixels.max(axis=2)
    minimum = pixels.min(axis=2)
    saturation = maximum - minimum
    luminance = (0.2126 * pixels[:, :, 0]) + (0.7152 * pixels[:, :, 1]) + (0.0722 * pixels[:, :, 2])

    paper = rgb(palette["paper"])
    navy = rgb(palette["navy"])
    grey = rgb(palette["grey"])
    line = rgb(palette["line"])
    teal = rgb(palette["teal"])

    output = np.broadcast_to(paper, pixels.shape).copy()
    neutral = saturation < 18
    dark_neutral = neutral & (luminance < 165)
    light_neutral = neutral & (luminance >= 165) & (luminance < 249)
    coloured = ~neutral

    dark_amount = np.clip(luminance / 165, 0, 1)
    output[dark_neutral] = blend(navy, grey, dark_amount)[dark_neutral]

    light_amount = np.clip((luminance - 165) / 84, 0, 1)
    output[light_neutral] = blend(line, paper, light_amount)[light_neutral]

    colour_amount = np.clip((luminance - 25) / 205, 0, 1)
    output[coloured] = blend(navy, teal, colour_amount)[coloured]
    return Image.fromarray(np.clip(output, 0, 255).astype(np.uint8), mode="RGB")


def template_font(path: Path, size: int, weight: int) -> ImageFont.FreeTypeFont:
    font = ImageFont.truetype(path, size=size)
    try:
        font.set_variation_by_axes([weight])
    except (AttributeError, OSError):
        pass
    return font


def right_aligned_x(draw: ImageDraw.ImageDraw, text: str, font: ImageFont.FreeTypeFont, right: int) -> int:
    left, _, right_edge, _ = draw.textbbox((0, 0), text, font=font)
    return right - (right_edge - left)


def prepare_plot(
    source: Image.Image, panel: dict[str, Any], settings: dict[str, Any]
) -> Image.Image:
    source_size = source.size
    palette = settings["palette"]
    plot = source.crop(rect(panel["source_plot_rect"], source_size))
    if panel.get("blank_plot_rects"):
        plot_draw = ImageDraw.Draw(plot)
        for blank_rect in panel["blank_plot_rects"]:
            plot_draw.rectangle(rect(blank_rect, plot.size), fill="white")
    return apply_template_palette(plot, palette)


def paste_fitted_plot(
    canvas: Image.Image, plot: Image.Image, bounds: list[int]
) -> None:
    plot_left, plot_top, plot_right, plot_bottom = bounds
    plot_width = plot_right - plot_left
    plot_height = plot_bottom - plot_top
    scale = min(plot_width / plot.width, plot_height / plot.height)
    plot_size = (max(1, round(plot.width * scale)), max(1, round(plot.height * scale)))
    plot = plot.resize(plot_size, Image.Resampling.LANCZOS)

    plot_x = plot_left + ((plot_width - plot.width) // 2)
    plot_y = plot_top + ((plot_height - plot.height) // 2)
    canvas.paste(plot, (plot_x, plot_y))


def compose_story_plot(
    plot: Image.Image, settings: dict[str, Any]
) -> Image.Image:
    variant = settings["variants"]["story"]
    canvas_width, canvas_height = variant["canvas_dimensions_px"]
    canvas = Image.new("RGB", (canvas_width, canvas_height), settings["palette"]["paper"])
    paste_fitted_plot(canvas, plot, variant["plot_bounds_px"])
    return canvas


def compose_media_panel(
    plot: Image.Image, panel: dict[str, Any], settings: dict[str, Any]
) -> Image.Image:
    palette = settings["palette"]
    variant = settings["variants"]["media"]
    canvas_width, canvas_height = variant["canvas_dimensions_px"]
    canvas = Image.new("RGB", (canvas_width, canvas_height), palette["paper"])
    paste_fitted_plot(canvas, plot, variant["plot_bounds_px"])

    draw = ImageDraw.Draw(canvas)
    font_path = SITE_DIR / settings["font_path_from_site_root"]
    if not font_path.is_file():
        raise FileNotFoundError(f"Template font not found: {font_path}")
    eyebrow_font = template_font(font_path, 15, 850)
    source_font = template_font(font_path, 46, 900)
    shape_font = template_font(font_path, 18, 900)
    feature_font = template_font(font_path, 25, 850)
    footer_font = template_font(font_path, 15, 850)
    axis_guide_font = template_font(font_path, 17, 800)

    draw.rectangle((0, 0, canvas_width, 6), fill=palette["teal"])
    eyebrow = f"AREA-LEVEL MODEL · {panel['context_label'].upper()} · 2021 CENSUS"
    draw.text((32, 20), eyebrow, font=eyebrow_font, fill=palette["teal_text"])
    draw.text((32, 42), panel["source_label"].upper(), font=source_font, fill=palette["navy"])
    shape_label = f"ILLUSTRATIVE {panel['shape_label'].upper()}"
    shape_x = right_aligned_x(draw, shape_label, shape_font, canvas_width - 32)
    draw.text((shape_x, 61), shape_label, font=shape_font, fill=palette["coral_text"])
    draw.text((32, 101), panel["feature_display_label"].upper(), font=feature_font, fill=palette["navy_soft"])
    draw.line((32, 140, canvas_width - 32, 140), fill=palette["line"], width=2)

    footer_y = canvas_height - 68
    draw.line((32, footer_y - 11, canvas_width - 32, footer_y - 11), fill=palette["line"], width=2)
    footer_left = "CROP FROM THE ACCEPTED SHAP DEPENDENCE FIGURE"
    footer_right = "CABRERA & ROWE / DEBIAS"
    draw.text((32, footer_y), footer_left, font=footer_font, fill=palette["teal_text"])
    footer_x = right_aligned_x(draw, footer_right, footer_font, canvas_width - 32)
    draw.text((footer_x, footer_y), footer_right, font=footer_font, fill=palette["grey"])
    axis_guide = "X: STANDARDISED AREA CHARACTERISTIC · Y: SHAP CONTRIBUTION TO PREDICTED COVERAGE BIAS"
    draw.text((32, footer_y + 24), axis_guide, font=axis_guide_font, fill=palette["navy_soft"])
    return canvas


def save_assets(image: Image.Image, stem: str, destination: Path, encoding: dict[str, Any]) -> list[Path]:
    webp_path = destination / f"{stem}.webp"
    png_path = destination / f"{stem}.png"

    webp = encoding["webp"]
    image.save(
        webp_path,
        format="WEBP",
        quality=webp["quality"],
        method=webp["method"],
        lossless=False,
    )

    png = encoding["png"]
    palette_image = image.quantize(
        colors=png["palette_colours"],
        method=Image.Quantize.MEDIANCUT,
        dither=Image.Dither.NONE,
    )
    palette_image.save(
        png_path,
        format="PNG",
        optimize=png["optimize"],
        compress_level=png["compress_level"],
    )
    return [webp_path, png_path]


def main() -> int:
    manifest = load_manifest()
    source_spec = manifest["source"]
    source_path = REPOSITORY_ROOT / source_spec["path_from_repository_root"]
    if not source_path.is_file():
        raise FileNotFoundError(f"Accepted source figure not found: {source_path}")

    actual_digest = sha256(source_path)
    if actual_digest != source_spec["sha256"]:
        raise ValueError(
            "Accepted source figure checksum does not match the manifest: "
            f"expected {source_spec['sha256']}, found {actual_digest}"
        )

    with Image.open(source_path) as opened:
        expected_size = (source_spec["width_px"], source_spec["height_px"])
        if opened.size != expected_size:
            raise ValueError(
                f"Accepted source dimensions changed: expected {expected_size}, found {opened.size}"
            )
        source = flatten_on_white(opened)

    encoding = manifest["encoding"]
    maximum_file_bytes = encoding["maximum_file_bytes"]
    maximum_total_bytes = encoding["maximum_total_bytes"]
    built: list[Path] = []

    with tempfile.TemporaryDirectory(prefix="nonlinear-panels-") as temporary:
        temporary_dir = Path(temporary)
        for panel in manifest["panels"]:
            plot = prepare_plot(source, panel, manifest["composition"])
            images = {
                "media": compose_media_panel(plot, panel, manifest["composition"]),
                "story": compose_story_plot(plot, manifest["composition"]),
            }
            for variant_name, image in images.items():
                variant = manifest["composition"]["variants"][variant_name]
                expected_dimensions = tuple(variant["canvas_dimensions_px"])
                if image.size != expected_dimensions:
                    raise ValueError(
                        f"Generated dimensions changed for {panel['id']} / {variant_name}: "
                        f"expected {expected_dimensions}, found {image.size}"
                    )
                stem = panel["output_stems"][variant_name]
                panel_assets = save_assets(image, stem, temporary_dir, encoding)
                built.extend(panel_assets)
                for path in panel_assets:
                    output_format = path.suffix.removeprefix(".")
                    expected = (
                        panel.get("expected_outputs", {})
                        .get(variant_name, {})
                        .get(output_format)
                    )
                    if expected:
                        if path.name != Path(expected["path"]).name:
                            raise ValueError(
                                f"Generated filename changed for {panel['id']} / "
                                f"{variant_name} / {output_format}"
                            )
                        actual_bytes = path.stat().st_size
                        if actual_bytes != expected["bytes"]:
                            raise ValueError(
                                f"Generated bytes changed for {path.name}: "
                                f"expected {expected['bytes']}, found {actual_bytes}"
                            )
                        actual_sha256 = sha256(path)
                        if actual_sha256 != expected["sha256"]:
                            raise ValueError(
                                f"Generated checksum changed for {path.name}: "
                                f"expected {expected['sha256']}, found {actual_sha256}"
                            )

        oversized = [path for path in built if path.stat().st_size > maximum_file_bytes]
        if oversized:
            details = ", ".join(f"{path.name} ({path.stat().st_size} bytes)" for path in oversized)
            raise ValueError(f"Generated assets exceed the per-file byte budget: {details}")

        total_bytes = sum(path.stat().st_size for path in built)
        if total_bytes > maximum_total_bytes:
            raise ValueError(
                f"Generated assets total {total_bytes} bytes; budget is {maximum_total_bytes} bytes"
            )

        OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
        for path in built:
            path.replace(OUTPUT_DIR / path.name)

    for path in sorted(OUTPUT_DIR.glob("*")):
        if path.is_file() and path.suffix in {".png", ".webp"}:
            print(f"{path.name}: {path.stat().st_size} bytes · sha256 {sha256(path)}")
    print(f"Total generated bytes: {total_bytes}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, KeyError, OSError, TypeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1) from error
