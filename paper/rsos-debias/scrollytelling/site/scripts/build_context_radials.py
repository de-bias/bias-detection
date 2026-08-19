#!/usr/bin/env python3
"""Build deterministic site-native radial profiles from pinned R1 outputs.

This is a site-derived analysis. It intentionally uses the revised R1 main-model
feature-importance files (random holdout, no lagged covariates), including
``fb_tts`` for Meta. It does not claim to reproduce the accepted paper figure,
whose archived build chain uses a different input lineage.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import html
import json
import math
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable


SCRIPT_PATH = Path(__file__).resolve()
SITE_DIR = SCRIPT_PATH.parents[1]
REPOSITORY_ROOT = SCRIPT_PATH.parents[5]
MANIFEST_PATH = SCRIPT_PATH.with_name("context_radials_manifest.json")

PAPER = "#fbfaf6"
INK = "#082b55"
INK_SOFT = "#304b68"
MUTED = "#5f6872"
GRID = "#d8d9d5"
GRID_STRONG = "#bfc5c4"


@dataclass(frozen=True)
class Feature:
    feature_id: str
    label: str
    short_label: str
    model_domain: str
    display_group_id: str
    metadata_order: int


# This is the variable metadata table in code/06_radial-plots.qmd, with stable
# public labels added locally because its external Census dictionary path is not
# portable. The metadata order is retained separately from the QMD axis order.
FEATURES: tuple[Feature, ...] = (
    Feature("per_nonukborn", "Population born outside the UK", "Non-UK born", "mobility", "mobility_geography", 1),
    Feature("pop_density", "Population density", "Population density", "geographic", "mobility_geography", 2),
    Feature("per_age_0_9", "Population aged 0–9", "Age 0–9", "demographic", "demographic", 3),
    Feature("per_age_10_19", "Population aged 10–19", "Age 10–19", "demographic", "demographic", 4),
    Feature("per_age_20_29", "Population aged 20–29", "Age 20–29", "demographic", "demographic", 5),
    Feature("per_age_30_39", "Population aged 30–39", "Age 30–39", "demographic", "demographic", 6),
    Feature("per_age_40_49", "Population aged 40–49", "Age 40–49", "demographic", "demographic", 7),
    Feature("per_age_50_59", "Population aged 50–59", "Age 50–59", "demographic", "demographic", 8),
    Feature("per_age_60_69", "Population aged 60–69", "Age 60–69", "demographic", "demographic", 9),
    Feature("per_age_70plus", "Population aged 70 and over", "Age 70+", "demographic", "demographic", 10),
    Feature("per_female", "Female population", "Female", "demographic", "demographic", 11),
    Feature("per_hh_notdeprived", "Households not deprived in any dimension", "Not deprived", "resource_access", "resource_access", 12),
    Feature("per_recent_migrant", "Population resident in the UK for less than two years", "Recent migrant", "mobility", "mobility_geography", 13),
    Feature("per_large_households", "Households with six or more people", "Large households", "demographic", "demographic", 14),
    Feature("per_hh_no_car", "Households without a car or van", "No car", "resource_access", "resource_access", 15),
    Feature("per_hh_no_centralheat", "Households without central heating", "No central heating", "resource_access", "resource_access", 16),
    Feature("per_hh_owned", "Households owned", "Owned", "resource_access", "resource_access", 17),
    Feature("per_home_work", "Employed residents working mainly at or from home", "Work from home", "mobility", "mobility_geography", 18),
    Feature("per_NS_SeC_L123_higher_managerial_administrative_professional", "Higher managerial, administrative and professional occupations", "Higher managerial", "socioeconomic", "socioeconomic", 19),
    Feature("per_NS_SeC_L456_lower_managerial_administrative_professional", "Lower managerial, administrative and professional occupations", "Lower managerial", "socioeconomic", "socioeconomic", 20),
    Feature("per_NS_SeC_L7_intermediate", "Intermediate occupations", "Intermediate", "socioeconomic", "socioeconomic", 21),
    Feature("per_NS_SeC_L89_small_employers_own_account", "Small employers and own-account workers", "Small employers", "socioeconomic", "socioeconomic", 22),
    Feature("per_NS_SeC_L1011_lower_supervisory_technical", "Lower supervisory and technical occupations", "Lower supervisory", "socioeconomic", "socioeconomic", 23),
    Feature("per_NS_SeC_L12_semi_routine", "Semi-routine occupations", "Semi-routine", "socioeconomic", "socioeconomic", 24),
    Feature("per_NS_SeC_L13_routine", "Routine occupations", "Routine", "socioeconomic", "socioeconomic", 25),
    Feature("per_NS_SeC_L141142_never_worked_unemployed", "Never worked and long-term unemployed", "Never worked / unemployed", "socioeconomic", "socioeconomic", 26),
    Feature("per_NS_SeC_L15_ft_students", "Full-time students", "Students", "socioeconomic", "socioeconomic", 27),
    Feature("per_no_qualifications", "Population with no qualifications", "No qualifications", "socioeconomic", "socioeconomic", 28),
    Feature("per_level4", "Population with Level 4 qualifications or above", "Level 4+", "socioeconomic", "socioeconomic", 29),
    Feature("rural_pct", "Rural population", "Rural", "geographic", "mobility_geography", 30),
)

DISPLAY_GROUPS: tuple[dict[str, Any], ...] = (
    {
        "id": "demographic",
        "label": "Demographic",
        "order": 1,
        "model_domains": ["demographic"],
        "note": "Census area-level age, sex and household-size characteristics.",
    },
    {
        "id": "socioeconomic",
        "label": "Socioeconomic",
        "order": 2,
        "model_domains": ["socioeconomic"],
        "note": "Census area-level occupation and qualification characteristics.",
    },
    {
        "id": "resource_access",
        "label": "Resource accessibility",
        "order": 3,
        "model_domains": ["resource_access"],
        "note": "Census household proxies; these are not direct measures of internet or device access.",
    },
    {
        "id": "mobility_geography",
        "label": "Mobility & geography",
        "order": 4,
        "model_domains": ["mobility", "geographic"],
        "note": "Census area characteristics, not observed movements or attributes of individual users.",
    },
)

EVIDENCE_BOUNDARIES: tuple[str, ...] = (
    "Each raw score is a mean absolute SHAP feature-importance value from an R1 random-holdout model without lagged covariates.",
    "Normalisation is performed within each source across the same 30 features. A value of 1 is that source's highest-scoring feature and 0 its lowest-scoring feature; 0 does not mean no effect.",
    "The profiles describe which LAD area characteristics helped the fitted model explain variation in population coverage bias. They do not measure the composition or inclusion rate of individual users or population groups.",
    "Mean absolute SHAP importance conveys magnitude, not the direction of an association, and does not establish causality.",
    "Raw SHAP magnitudes are not compared across the four separately fitted source models; compare the within-source profiles.",
    "Resource accessibility features are Census household proxies, not direct measures of internet or device access.",
    "The site-derived profiles use the revised R1 main-model outputs and do not exactly reproduce the accepted paper's radial figure, whose archived build chain uses a different input lineage.",
    "Axis order reproduces the layout rule in code/06_radial-plots.qmd and is not an additional importance estimate.",
)


def sha256_bytes(content: bytes) -> str:
    return hashlib.sha256(content).hexdigest()


def sha256_file(path: Path) -> str:
    return sha256_bytes(path.read_bytes())


def relative_to_repository(path: Path) -> str:
    return path.resolve().relative_to(REPOSITORY_ROOT).as_posix()


def load_manifest() -> dict[str, Any]:
    try:
        manifest = json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise ValueError(f"Cannot read manifest {MANIFEST_PATH}: {exc}") from exc
    if manifest.get("schema_version") != 1:
        raise ValueError("Manifest schema_version must be 1")
    return manifest


def validate_contract(manifest: dict[str, Any]) -> None:
    expected = manifest["expected"]
    source_ids = [item["source_id"] for item in manifest["inputs"]]
    group_ids = [item["id"] for item in DISPLAY_GROUPS]
    feature_ids = [item.feature_id for item in FEATURES]
    if source_ids != manifest["source_order"]:
        raise ValueError("Manifest inputs must follow source_order exactly")
    if group_ids != manifest["display_group_order"]:
        raise ValueError("Display groups do not match manifest display_group_order")
    if len(source_ids) != expected["source_count"] or len(set(source_ids)) != len(source_ids):
        raise ValueError("Source count or source IDs are inconsistent")
    if len(group_ids) != expected["display_group_count"] or len(set(group_ids)) != len(group_ids):
        raise ValueError("Display-group count or IDs are inconsistent")
    if len(feature_ids) != expected["feature_count"] or len(set(feature_ids)) != len(feature_ids):
        raise ValueError("Feature count or feature IDs are inconsistent")
    for group_id, expected_count in expected["group_feature_counts"].items():
        observed = sum(feature.display_group_id == group_id for feature in FEATURES)
        if observed != expected_count:
            raise ValueError(f"{group_id}: expected {expected_count} features, found {observed}")
    if sorted(feature.metadata_order for feature in FEATURES) != list(range(1, len(FEATURES) + 1)):
        raise ValueError("metadata_order must be the consecutive range 1..30")


def read_source(input_item: dict[str, Any]) -> dict[str, float]:
    path = REPOSITORY_ROOT / input_item["path"]
    if not path.is_file():
        raise ValueError(f"Missing source input: {path}")
    observed_hash = sha256_file(path)
    if observed_hash != input_item["sha256"]:
        raise ValueError(
            f"Input hash mismatch for {input_item['source_id']}: "
            f"expected {input_item['sha256']}, found {observed_hash}"
        )
    values: dict[str, float] = {}
    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        if reader.fieldnames != ["feature", "value"]:
            raise ValueError(f"{path}: expected CSV header feature,value; found {reader.fieldnames}")
        for row_number, row in enumerate(reader, start=2):
            feature_id = row["feature"]
            if not feature_id or feature_id in values:
                raise ValueError(f"{path}:{row_number}: missing or duplicate feature ID {feature_id!r}")
            try:
                value = float(row["value"])
            except (TypeError, ValueError) as exc:
                raise ValueError(f"{path}:{row_number}: invalid numeric value {row['value']!r}") from exc
            if not math.isfinite(value) or value < 0:
                raise ValueError(f"{path}:{row_number}: feature importance must be finite and non-negative")
            values[feature_id] = value
    expected_ids = {feature.feature_id for feature in FEATURES}
    observed_ids = set(values)
    if observed_ids != expected_ids:
        missing = sorted(expected_ids - observed_ids)
        extra = sorted(observed_ids - expected_ids)
        raise ValueError(f"{path}: inconsistent feature set; missing={missing}, extra={extra}")
    if len(values) != len(FEATURES):
        raise ValueError(f"{path}: expected {len(FEATURES)} features, found {len(values)}")
    if min(values.values()) == max(values.values()):
        raise ValueError(f"{path}: min-max normalisation is undefined for a constant vector")
    return values


def format_number(value: float, digits: int = 6) -> str:
    if abs(value) < 5e-15:
        return "0"
    return f"{value:.{digits}f}".rstrip("0").rstrip(".")


def escape(value: str) -> str:
    return html.escape(value, quote=True)


def points_attribute(points: Iterable[tuple[float, float]]) -> str:
    return " ".join(f"{x:.2f},{y:.2f}" for x, y in points)


def adjust_label_y(candidates: list[dict[str, Any]], minimum: float, maximum: float, gap: float) -> None:
    """Apply a small deterministic vertical collision pass on one side."""

    if not candidates:
        return
    candidates.sort(key=lambda candidate: (candidate["target_y"], candidate["axis_order"]))
    for index, candidate in enumerate(candidates):
        lower = minimum if index == 0 else candidates[index - 1]["label_y"] + gap
        candidate["label_y"] = max(candidate["target_y"], lower)
    overflow = candidates[-1]["label_y"] - maximum
    if overflow > 0:
        for candidate in candidates:
            candidate["label_y"] -= overflow
    for index in range(len(candidates) - 2, -1, -1):
        candidates[index]["label_y"] = min(
            candidates[index]["label_y"], candidates[index + 1]["label_y"] - gap
        )
    underflow = minimum - candidates[0]["label_y"]
    if underflow > 0:
        for candidate in candidates:
            candidate["label_y"] += underflow


def svg_for_panel(
    source: dict[str, Any],
    group: dict[str, Any],
    features: list[dict[str, Any]],
    width: int,
    height: int,
) -> bytes:
    source_label = source["label"]
    group_label = group["label"]
    color = source["color"]
    value_label_color = "#066d6b"
    panel_id = f"{source['id']}-{group['id']}"
    title_id = f"title-{panel_id}"
    desc_id = f"desc-{panel_id}"
    labelled = [feature for feature in features if feature["is_labelled"]]
    if labelled:
        labelled_text = "; ".join(
            f"{feature['label']} ({feature['normalised_importance']:.2f})" for feature in labelled
        )
    else:
        labelled_text = "No feature in this group scores above 0.5."
    title = f"{source_label}: {group_label} area-context profile"
    description = (
        "A radial profile of within-source relative mean absolute SHAP importance. "
        "Zero is the source's lowest-scoring feature among 30 and one is its highest; "
        "the score gives magnitude, not direction or causality. "
        f"Features above 0.5: {labelled_text}"
    )

    centre_x = width / 2
    centre_y = 269.0
    radius = 178.0
    angles = [(-math.pi / 2) + (2 * math.pi * index / len(features)) for index in range(len(features))]
    outer_points = [
        (centre_x + radius * math.cos(angle), centre_y + radius * math.sin(angle)) for angle in angles
    ]
    value_points = [
        (
            centre_x + radius * feature["normalised_importance"] * math.cos(angle),
            centre_y + radius * feature["normalised_importance"] * math.sin(angle),
        )
        for feature, angle in zip(features, angles)
    ]

    lines: list[str] = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        (
            f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" '
            f'width="{width}" height="{height}" role="img" aria-labelledby="{title_id} {desc_id}">'
        ),
        f"  <title id=\"{title_id}\">{escape(title)}</title>",
        f"  <desc id=\"{desc_id}\">{escape(description)}</desc>",
        f'  <rect width="{width}" height="{height}" rx="18" fill="{PAPER}"/>',
        (
            f'  <text x="24" y="30" fill="{MUTED}" font-family="Roboto Condensed, Arial, sans-serif" '
            'font-size="13" font-weight="700" letter-spacing="1.2">RELATIVE IMPORTANCE</text>'
        ),
        '  <g aria-hidden="true">',
    ]
    for ring_value, ring_color, stroke_width in ((1.0, GRID_STRONG, 1.4), (0.5, GRID, 1.1)):
        lines.append(
            f'    <circle cx="{centre_x:.1f}" cy="{centre_y:.1f}" r="{radius * ring_value:.1f}" '
            f'fill="none" stroke="{ring_color}" stroke-width="{stroke_width}"/>'
        )
    lines.append(
        f'    <circle cx="{centre_x:.1f}" cy="{centre_y:.1f}" r="2.5" fill="none" '
        f'stroke="{GRID_STRONG}" stroke-width="1.2"/>'
    )
    for outer_x, outer_y in outer_points:
        lines.append(
            f'    <line x1="{centre_x:.1f}" y1="{centre_y:.1f}" x2="{outer_x:.2f}" y2="{outer_y:.2f}" '
            f'stroke="{GRID}" stroke-width="1"/>'
        )
    for ring_value, label in ((1.0, "1"), (0.5, "0.5"), (0.0, "0")):
        label_y = centre_y - (radius * ring_value) + (14 if ring_value == 0 else -7)
        lines.append(
            f'    <text x="{centre_x + 8:.1f}" y="{label_y:.1f}" fill="{MUTED}" '
            f'font-family="Roboto Condensed, Arial, sans-serif" font-size="12">{label}</text>'
        )
    lines.extend(
        [
            "  </g>",
            (
                f'  <polygon points="{points_attribute(value_points)}" fill="{color}" fill-opacity="0.17" '
                f'stroke="{color}" stroke-width="3" stroke-linejoin="round"/>'
            ),
            f'  <g fill="{color}" stroke="{PAPER}" stroke-width="2">',
        ]
    )
    for x, y in value_points:
        lines.append(f'    <circle cx="{x:.2f}" cy="{y:.2f}" r="4.3"/>')
    lines.append("  </g>")

    label_candidates: list[dict[str, Any]] = []
    for feature, angle, (point_x, point_y) in zip(features, angles, value_points):
        if not feature["is_labelled"]:
            continue
        side = "right" if math.cos(angle) >= 0 else "left"
        label_candidates.append(
            {
                "feature": feature,
                "axis_order": feature["axis_order"],
                "point_x": point_x,
                "point_y": point_y,
                "target_y": centre_y + math.sin(angle) * (radius + 27),
                "side": side,
            }
        )
    for side in ("left", "right"):
        adjust_label_y(
            [candidate for candidate in label_candidates if candidate["side"] == side],
            minimum=62,
            maximum=476,
            gap=28,
        )
    if label_candidates:
        lines.append('  <g font-family="Roboto Condensed, Arial, sans-serif">')
    for candidate in sorted(label_candidates, key=lambda item: item["axis_order"]):
        is_right = candidate["side"] == "right"
        # Keep callouts inside the 600 px viewBox. Long public labels such as
        # “Population density” and “Lower supervisory” must not be clipped.
        label_x = width - 132 if is_right else 132
        elbow_x = centre_x + radius + 12 if is_right else centre_x - radius - 12
        text_x = label_x + 5 if is_right else label_x - 5
        anchor = "start" if is_right else "end"
        score = format_number(candidate["feature"]["normalised_importance"], 2)
        lines.extend(
            [
                (
                    f'    <path d="M {candidate["point_x"]:.2f} {candidate["point_y"]:.2f} '
                    f'L {elbow_x:.2f} {candidate["label_y"]:.2f} L {label_x:.2f} {candidate["label_y"]:.2f}" '
                    f'fill="none" stroke="{color}" stroke-width="1.5" stroke-opacity="0.72"/>'
                ),
                (
                    f'    <text x="{text_x:.2f}" y="{candidate["label_y"] - 2:.2f}" text-anchor="{anchor}" '
                    f'fill="{INK}" font-size="13.5" font-weight="700" paint-order="stroke" '
                    f'stroke="{PAPER}" stroke-width="5" stroke-linejoin="round">'
                    f'{escape(candidate["feature"]["short_label"])}</text>'
                ),
                (
                    f'    <text x="{text_x:.2f}" y="{candidate["label_y"] + 15:.2f}" text-anchor="{anchor}" '
                    f'fill="{value_label_color}" font-size="11.5" font-weight="700" paint-order="stroke" '
                    f'stroke="{PAPER}" stroke-width="4">{escape(score)}</text>'
                ),
            ]
        )
    if label_candidates:
        lines.append("  </g>")
    lines.extend(
        [
            (
                f'  <text x="{centre_x:.1f}" y="535" text-anchor="middle" fill="{INK_SOFT}" '
                'font-family="Roboto Condensed, Arial, sans-serif" font-size="13">0 = lowest of 30 · 1 = highest of 30</text>'
            ),
            "</svg>",
            "",
        ]
    )
    return "\n".join(lines).encode("utf-8")


def make_artifacts(manifest: dict[str, Any]) -> tuple[dict[Path, bytes], dict[str, Any]]:
    validate_contract(manifest)
    values_by_source: dict[str, dict[str, float]] = {}
    for input_item in manifest["inputs"]:
        values_by_source[input_item["source_id"]] = read_source(input_item)

    feature_lookup = {feature.feature_id: feature for feature in FEATURES}
    # Reproduce the axis-order rule in code/06_radial-plots.qmd exactly:
    # descending four-source mean raw importance within each display group.
    overall_means = {
        feature.feature_id: sum(values[feature.feature_id] for values in values_by_source.values())
        / len(values_by_source)
        for feature in FEATURES
    }
    axis_order_by_group: dict[str, list[str]] = {}
    axis_rank: dict[str, int] = {}
    for group in DISPLAY_GROUPS:
        group_features = [feature for feature in FEATURES if feature.display_group_id == group["id"]]
        ordered = sorted(
            group_features,
            key=lambda feature: (-overall_means[feature.feature_id], feature.metadata_order),
        )
        axis_order_by_group[group["id"]] = [feature.feature_id for feature in ordered]
        for rank, feature in enumerate(ordered, start=1):
            axis_rank[feature.feature_id] = rank

    outputs = manifest["outputs"]
    data_path = REPOSITORY_ROOT / outputs["data"]
    svg_directory = REPOSITORY_ROOT / outputs["svg_directory"]
    artifacts: dict[Path, bytes] = {}
    sources: list[dict[str, Any]] = []
    panels: list[dict[str, Any]] = []
    width = int(outputs["svg_width"])
    height = int(outputs["svg_height"])

    for input_item in manifest["inputs"]:
        source_id = input_item["source_id"]
        raw_values = values_by_source[source_id]
        source_min = min(raw_values.values())
        source_max = max(raw_values.values())
        source_range = source_max - source_min
        normalised = {
            feature_id: round((value - source_min) / source_range, 12)
            for feature_id, value in raw_values.items()
        }
        source_ranking = sorted(
            FEATURES,
            key=lambda feature: (-raw_values[feature.feature_id], feature.metadata_order),
        )
        source_rank: dict[str, int] = {}
        previous_value: float | None = None
        current_rank = 0
        for position, feature in enumerate(source_ranking, start=1):
            value = raw_values[feature.feature_id]
            if previous_value is None or value != previous_value:
                current_rank = position
                previous_value = value
            source_rank[feature.feature_id] = current_rank
        source_summary = {
            "id": source_id,
            "label": input_item["label"],
            "dataset_key": input_item["dataset_key"],
            "color": input_item["color"],
            "input_path": input_item["path"],
            "input_sha256": input_item["sha256"],
            "raw_min": source_min,
            "raw_max": source_max,
            "feature_count": len(raw_values),
            "top_feature_ids": [feature.feature_id for feature in source_ranking[:5]],
        }
        sources.append(source_summary)

        for group in DISPLAY_GROUPS:
            ordered_ids = axis_order_by_group[group["id"]]
            panel_features: list[dict[str, Any]] = []
            for feature_id in ordered_ids:
                feature = feature_lookup[feature_id]
                normalised_value = normalised[feature_id]
                panel_features.append(
                    {
                        "id": feature_id,
                        "label": feature.label,
                        "short_label": feature.short_label,
                        "model_domain": feature.model_domain,
                        "metadata_order": feature.metadata_order,
                        "axis_order": axis_rank[feature_id],
                        "raw_mean_abs_shap": raw_values[feature_id],
                        "normalised_importance": normalised_value,
                        "source_rank": source_rank[feature_id],
                        "is_labelled": normalised_value > manifest["label_threshold"]["value"],
                    }
                )
            svg_name = f"{source_id}-{group['id']}.svg"
            svg_path = svg_directory / svg_name
            svg_bytes = svg_for_panel(source_summary, group, panel_features, width, height)
            artifacts[svg_path] = svg_bytes
            panels.append(
                {
                    "id": f"{source_id}-{group['id']}",
                    "source_id": source_id,
                    "display_group_id": group["id"],
                    "svg_path": relative_to_repository(svg_path).removeprefix(
                        "paper/rsos-debias/scrollytelling/site/"
                    ),
                    "svg_sha256": sha256_bytes(svg_bytes),
                    "svg_bytes": len(svg_bytes),
                    "feature_count": len(panel_features),
                    "labelled_feature_count": sum(feature["is_labelled"] for feature in panel_features),
                    "features": panel_features,
                }
            )

    group_payload = []
    for group in DISPLAY_GROUPS:
        group_payload.append(
            {
                **group,
                "feature_count": sum(feature.display_group_id == group["id"] for feature in FEATURES),
                "axis_feature_ids": axis_order_by_group[group["id"]],
            }
        )

    expected_svg_outputs = manifest.get("expected_svg_outputs")
    if not isinstance(expected_svg_outputs, list):
        raise ValueError("Manifest must pin expected_svg_outputs")
    expected_svg_by_path = {item["path"]: item for item in expected_svg_outputs}
    observed_svg_by_path = {
        panel["svg_path"]: {"bytes": panel["svg_bytes"], "sha256": panel["svg_sha256"]}
        for panel in panels
    }
    if len(expected_svg_by_path) != len(expected_svg_outputs):
        raise ValueError("Manifest expected_svg_outputs contains a duplicate path")
    if set(expected_svg_by_path) != set(observed_svg_by_path):
        raise ValueError(
            "Pinned SVG file set differs from generated set: "
            f"missing={sorted(set(observed_svg_by_path) - set(expected_svg_by_path))}, "
            f"extra={sorted(set(expected_svg_by_path) - set(observed_svg_by_path))}"
        )
    for svg_path, observed in observed_svg_by_path.items():
        expected_output = expected_svg_by_path[svg_path]
        if expected_output["bytes"] != observed["bytes"] or expected_output["sha256"] != observed["sha256"]:
            raise ValueError(
                f"Pinned SVG output mismatch for {svg_path}: expected "
                f"{expected_output['bytes']} bytes/{expected_output['sha256']}, found "
                f"{observed['bytes']} bytes/{observed['sha256']}"
            )

    payload: dict[str, Any] = {
        "schema_version": 1,
        "artifact_type": "new_site_derived_analysis",
        "metadata": {
            "title": "Area-context fingerprints across four digital data sources",
            "analysis_label": "New site-derived radial profiles from the revised R1 main-model specification.",
            "unit_of_analysis": "2021 local authority district in England and Wales",
            "outcome": "Population coverage bias",
            "importance_metric": "Mean absolute SHAP value",
            "model_specification": "XGBoost; random holdout; no lagged covariates",
            "source_rank_method": "Competition rank by raw mean absolute SHAP value; equal values share a rank.",
            "source_count": len(sources),
            "display_group_count": len(group_payload),
            "feature_count": len(FEATURES),
            "panel_count": len(panels),
            "accepted_figure_match": False,
            "accepted_figure_note": "These profiles do not exactly reproduce the accepted paper's radial figure.",
        },
        "provenance": {
            "generator": relative_to_repository(SCRIPT_PATH),
            "generator_sha256": sha256_file(SCRIPT_PATH),
            "manifest": relative_to_repository(MANIFEST_PATH),
            "manifest_sha256": sha256_file(MANIFEST_PATH),
            "feature_metadata_source": manifest["feature_metadata_source"],
            "input_lineage": "R1 main-model feature-importance outputs",
            "inputs": [
                {
                    "source_id": item["source_id"],
                    "dataset_key": item["dataset_key"],
                    "path": item["path"],
                    "sha256": item["sha256"],
                    "row_count": len(FEATURES),
                }
                for item in manifest["inputs"]
            ],
        },
        "normalization": {
            **manifest["normalization"],
            "zero_meaning": "Lowest-scoring feature among the source's 30 features; not no effect.",
            "one_meaning": "Highest-scoring feature among the source's 30 features.",
            "comparison_boundary": "Compare relative patterns within a source, not raw SHAP magnitudes between separately fitted source models.",
        },
        "axis_order": {
            **manifest["axis_order"],
            "interpretation": "Layout only; not an additional cross-source importance estimate.",
        },
        "label_threshold": manifest["label_threshold"],
        "visual_encoding": {
            "palette": "DEBIAS single-root teal visual system",
            "source_colors": {source["id"]: source["color"] for source in sources},
            "font_family": "Roboto Condensed, Arial, sans-serif",
            "rings": [0.0, 0.5, 1.0],
            "polygon": "DEBIAS teal fill, outline and points",
            "labels": "Feature labels are displayed only where normalised_importance > 0.5.",
        },
        "evidence_boundaries": list(EVIDENCE_BOUNDARIES),
        "display_groups": group_payload,
        "sources": sources,
        "panels": panels,
    }
    data_bytes = (
        json.dumps(payload, ensure_ascii=False, separators=(",", ":")) + "\n"
    ).encode("utf-8")
    artifacts[data_path] = data_bytes
    if len(panels) != outputs["panel_count"]:
        raise ValueError(f"Expected {outputs['panel_count']} panels, generated {len(panels)}")
    return artifacts, payload


def write_artifacts(artifacts: dict[Path, bytes]) -> None:
    for path in sorted(artifacts, key=lambda candidate: candidate.as_posix()):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(artifacts[path])


def check_artifacts(artifacts: dict[Path, bytes]) -> None:
    failures: list[str] = []
    for path in sorted(artifacts, key=lambda candidate: candidate.as_posix()):
        if not path.is_file():
            failures.append(f"missing: {relative_to_repository(path)}")
        elif path.read_bytes() != artifacts[path]:
            failures.append(f"differs: {relative_to_repository(path)}")
    svg_directory = REPOSITORY_ROOT / load_manifest()["outputs"]["svg_directory"]
    expected_svg_names = {path.name for path in artifacts if path.suffix == ".svg"}
    observed_svg_names = {path.name for path in svg_directory.glob("*.svg")} if svg_directory.is_dir() else set()
    if observed_svg_names != expected_svg_names:
        failures.append(
            "SVG file set differs: "
            f"missing={sorted(expected_svg_names - observed_svg_names)}, "
            f"extra={sorted(observed_svg_names - expected_svg_names)}"
        )
    if failures:
        raise ValueError("Generated artifacts are not current:\n- " + "\n- ".join(failures))


def print_summary(artifacts: dict[Path, bytes], payload: dict[str, Any], mode: str) -> None:
    data_path = REPOSITORY_ROOT / load_manifest()["outputs"]["data"]
    print(
        f"{mode}: {payload['metadata']['source_count']} sources, "
        f"{payload['metadata']['feature_count']} features, {payload['metadata']['panel_count']} SVG panels"
    )
    print(
        f"data {relative_to_repository(data_path)} "
        f"{len(artifacts[data_path])} bytes sha256={sha256_bytes(artifacts[data_path])}"
    )
    for source in payload["sources"]:
        top = source["top_feature_ids"][:3]
        print(f"{source['id']}: top3={','.join(top)}")
    combined_svg_hash = sha256_bytes(
        b"".join(
            artifacts[path]
            for path in sorted(artifacts, key=lambda candidate: candidate.as_posix())
            if path.suffix == ".svg"
        )
    )
    print(f"svg-set sha256={combined_svg_hash}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="Generate in memory and fail unless all checked-in outputs are byte-identical.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    try:
        manifest = load_manifest()
        artifacts, payload = make_artifacts(manifest)
        if args.check:
            check_artifacts(artifacts)
            mode = "verified"
        else:
            write_artifacts(artifacts)
            mode = "built"
        print_summary(artifacts, payload, mode)
    except (OSError, ValueError, KeyError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
