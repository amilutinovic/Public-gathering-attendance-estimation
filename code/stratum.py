#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Auto-assign strata for grid cells:
- Marks near-black cells as include=0 (HSV V < v_black across >= black_frac of pixels)
- Computes a composite density score (bright_frac, edge_density, texture_lapvar)
- Optionally drops the lowest-score tail as include=0 (low-score-q)
- Assigns strata=1 (top), 2 (mid), 3 (bottom) by tertiles among include==1
- Can assign tertiles globally or per-image (--per-image-quantiles)
- Optional debug overlays

Usage:
  python auto_strata.py \
    --csv grid_output/strata_map.csv \
    --out grid_output/strata_map_auto.csv \
    --debug-overlays

Recommended stricter defaults:
  --v-black 0.10 --black-frac 0.90 --low-score-q 0.08
"""

import argparse
from pathlib import Path
import math
import numpy as np
import pandas as pd
from PIL import Image
from tqdm import tqdm

try:
    import cv2
except ImportError:
    raise SystemExit("This script requires opencv-python (cv2). Install: pip install opencv-python")


# ---------- Image helpers ----------

def read_image(img_path: str) -> np.ndarray:
    with Image.open(img_path) as im:
        return np.array(im.convert('RGB'))  # H, W, 3


def rgb_to_hsv(rgb: np.ndarray):
    bgr = cv2.cvtColor(rgb, cv2.COLOR_RGB2BGR)
    hsv = cv2.cvtColor(bgr, cv2.COLOR_BGR2HSV).astype(np.float32)
    h = hsv[..., 0] / 179.0
    s = hsv[..., 1] / 255.0
    v = hsv[..., 2] / 255.0
    return h, s, v


def crop_region(img: np.ndarray, x0, x1, y0, y1):
    # 1-based inclusive R-coordinates -> Python slices
    H, W = img.shape[:2]
    x0i = max(0, int(x0) - 1)
    x1i = min(W, int(x1))
    y0i = max(0, int(y0) - 1)
    y1i = min(H, int(y1))
    if x0i >= x1i or y0i >= y1i:
        return None
    return img[y0i:y1i, x0i:x1i, :]


# ---------- Metrics ----------

def cell_metrics(rgb: np.ndarray, v_thresh=0.90, s_thresh=0.25):
    """
    Heuristic metrics:
      - bright_frac: fraction with V>v_thresh & S>s_thresh
      - edge_density: fraction of Canny edges (auto thresholds from median)
      - texture_lapvar: Laplacian variance (log-compressed)
    """
    _, s, v = rgb_to_hsv(rgb)

    bright_mask = (v > v_thresh) & (s > s_thresh)
    bright_frac = float(bright_mask.mean()) if bright_mask.size else 0.0

    gray = cv2.cvtColor(rgb, cv2.COLOR_RGB2GRAY)
    med = np.median(gray)
    lower = int(max(0, 0.66 * med))
    upper = int(min(255, 1.33 * med))
    edges = cv2.Canny(gray, lower, upper)
    edge_density = float((edges > 0).mean()) if edges.size else 0.0

    lap = cv2.Laplacian(gray, cv2.CV_64F, ksize=3)
    lapvar = float(lap.var())
    texture_lapvar = math.log1p(lapvar) / 10.0

    return {
        'bright_frac': bright_frac,
        'edge_density': edge_density,
        'texture_lapvar': texture_lapvar,
    }


def is_near_black(rgb: np.ndarray, v_black=0.10, black_frac=0.90):
    """True if >= black_frac pixels have V < v_black."""
    _, _, v = rgb_to_hsv(rgb)
    frac_below = float((v < v_black).mean()) if v.size else 1.0
    return (frac_below >= black_frac), frac_below


def composite_score(m, w_bright=0.4, w_edge=0.4, w_texture=0.2):
    return (
        w_bright * m['bright_frac'] +
        w_edge   * m['edge_density'] +
        w_texture* m['texture_lapvar']
    )


# ---------- Strata assignment ----------

def assign_tertiles(scores: np.ndarray, mask: np.ndarray) -> np.ndarray:
    """
    Assign 1/2/3 by tertiles for scores[mask==True].
    Returns float array with NaN outside mask.
    """
    scores = np.asarray(scores, dtype=float)
    mask = np.asarray(mask, dtype=bool)
    strata = np.full(scores.shape, np.nan, dtype=float)
    if mask.sum() == 0:
        return strata

    s = scores[mask]
    q1 = np.quantile(s, 1/3)
    q2 = np.quantile(s, 2/3)

    # bottom third -> 3, middle -> 2, top -> 1
    strata[(scores < q1) & mask] = 3
    strata[(scores >= q1) & (scores < q2) & mask] = 2
    strata[(scores >= q2) & mask] = 1
    return strata


def assign_strata_global(df: pd.DataFrame) -> np.ndarray:
    mask = (df['include'] == 1) & np.isfinite(df['strata_score'])
    return assign_tertiles(df['strata_score'].to_numpy(), mask.to_numpy())


def assign_strata_per_image(df: pd.DataFrame) -> np.ndarray:
    out = np.full(len(df), np.nan, dtype=float)
    for img_path, sub_idx in df.groupby('img_path').groups.items():
        sub = df.loc[sub_idx]
        mask = (sub['include'] == 1) & np.isfinite(sub['strata_score'])
        strata = assign_tertiles(sub['strata_score'].to_numpy(), mask.to_numpy())
        out[list(sub_idx)] = strata
    return out


# ---------- Debug overlays ----------

def save_debug_overlays(df: pd.DataFrame, out_dir: str):
    try:
        import matplotlib.pyplot as plt
        from matplotlib.patches import Rectangle
    except Exception as e:
        print('Skipping debug overlays (matplotlib not available):', e)
        return

    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    def read_rgb(p):
        with Image.open(p) as im:
            return np.array(im.convert('RGB'))

    for img_path, sub in df.groupby('img_path'):
        try:
            rgb = read_rgb(img_path)
        except Exception as e:
            print(f"Overlay skip (cannot read {img_path}): {e}")
            continue

        H, W = rgb.shape[:2]
        fig, ax = plt.subplots(figsize=(W/200, H/200), dpi=200)
        ax.imshow(rgb)
        ax.set_axis_off()

        sc = sub['strata_score'].to_numpy()
        if np.all(~np.isfinite(sc)):
            print(f'No scores for overlay: {img_path}')
            plt.close(fig)
            continue
        sc_min = np.nanmin(sc)
        sc_max = np.nanmax(sc)
        rng = max(1e-6, sc_max - sc_min)

        for _, r in sub.iterrows():
            if not np.isfinite(r['strata_score']):
                continue
            alpha = (r['strata_score'] - sc_min) / rng
            colmap = {
                1: (1, 0, 0, 0.25 + 0.5 * alpha),   # red
                2: (1, 1, 0, 0.20 + 0.4 * alpha),   # yellow
                3: (0, 1, 0, 0.15 + 0.3 * alpha),   # green
            }
            s = int(r['stratum']) if pd.notna(r['stratum']) else 3
            col = colmap.get(s, (0, 1, 0, 0.2))
            rect = Rectangle(
                (r['x0'], r['y0']),
                r['x1'] - r['x0'],
                r['y1'] - r['y0'],
                fill=True, edgecolor=(1, 1, 1, 0.1), facecolor=col, linewidth=0.1
            )
            ax.add_patch(rect)

        out_path = out_dir / (Path(img_path).stem + '_overlay.png')
        fig.savefig(out_path, bbox_inches='tight', pad_inches=0)
        plt.close(fig)


# ---------- Main ----------

def main():
    ap = argparse.ArgumentParser(description='Auto-assign strata with stricter black mask and low-score filtering.')
    ap.add_argument('--csv', required=True, help='Path to strata_map.csv from R pipeline')
    ap.add_argument('--out', required=True, help='Path to write updated CSV (e.g., grid_output/strata_map_auto.csv)')

    # Stricter defaults for “empty” regions
    ap.add_argument('--v-black', type=float, default=0.10, dest='v_black', help='HSV V threshold for near-black [0..1]')
    ap.add_argument('--black-frac', type=float, default=0.90, dest='black_frac', help='Fraction below V to mark include=0')

    # Score thresholds/weights
    ap.add_argument('--v-thresh', type=float, default=0.90, dest='v_thresh', help='HSV V bright threshold [0..1]')
    ap.add_argument('--s-thresh', type=float, default=0.25, dest='s_thresh', help='HSV S threshold [0..1]')
    ap.add_argument('--w-bright', type=float, default=0.4, dest='w_bright', help='Weight for bright fraction')
    ap.add_argument('--w-edge', type=float, default=0.4, dest='w_edge', help='Weight for edge density')
    ap.add_argument('--w-texture', type=float, default=0.2, dest='w_texture', help='Weight for Laplacian variance')

    # New: drop the lowest-score tail as include=0
    ap.add_argument('--low-score-q', type=float, default=0.08,
                    help='Quantile (0..1) below which cells become include=0 (among currently include==1).')
    ap.add_argument('--keep-low-score-included', action='store_true',
                    help='Do NOT flip low-score cells to include=0 (only near-black decides include).')

    # Respect existing marks
    ap.add_argument('--respect-existing', action='store_true',
                    help='Keep existing include==0 and/or non-NA stratum; only fill missing where needed.')

    # Per-image tertiles instead of global
    ap.add_argument('--per-image-quantiles', action='store_true',
                    help='Assign tertiles per image rather than globally.')

    # Debug overlays
    ap.add_argument('--debug-overlays', action='store_true', help='Save per-image heatmap overlays')
    ap.add_argument('--overlay-dir', default=None, help='Where to save overlays (default: alongside output CSV)')
    args = ap.parse_args()

    df = pd.read_csv(args.csv)

    required = {'cell_id','zone_id','include','stratum','x0','x1','y0','y1','img_path'}
    missing = required - set(df.columns)
    if missing:
        raise SystemExit(f"Missing required columns in CSV: {missing}")

    df['include'] = df['include'].fillna(1).astype(int)

    scores = []
    black_flags = []
    black_fracs = []
    cache = {}

    # Compute scores and black flags
    for idx in tqdm(range(len(df)), desc='Processing cells'):
        row = df.iloc[idx]
        img_path = row['img_path']

        # Cache images
        try:
            if img_path not in cache:
                cache[img_path] = read_image(img_path)
            rgb_full = cache[img_path]
        except Exception:
            scores.append(np.nan)
            black_flags.append(False)
            black_fracs.append(np.nan)
            continue

        crop = crop_region(rgb_full, row['x0'], row['x1'], row['y0'], row['y1'])
        if crop is None or crop.size == 0:
            scores.append(np.nan)
            black_flags.append(True)
            black_fracs.append(1.0)
            continue

        is_black, frac_below = is_near_black(crop, v_black=args.v_black, black_frac=args.black_frac)
        black_flags.append(is_black)
        black_fracs.append(frac_below)

        if is_black:
            scores.append(-np.inf)
            continue

        m = cell_metrics(crop, v_thresh=args.v_thresh, s_thresh=args.s_thresh)
        score = composite_score(m, w_bright=args.w_bright, w_edge=args.w_edge, w_texture=args.w_texture)
        scores.append(score)

    df['strata_score'] = scores
    df['black_frac'] = black_fracs

    # 1) Near-black -> include=0
    if args.respect_existing:
        new_inc = df['include'].copy()
        new_inc[(new_inc != 0) & (np.array(black_flags))] = 0
        df['include'] = new_inc
    else:
        df['include'] = np.where(np.array(black_flags), 0, 1)

    # 2) Low-score tail -> include=0 (unless explicitly kept)
    if not args.keep_low_score_included and args.low_score_q > 0:
        mask_inc = (df['include'] == 1) & np.isfinite(df['strata_score'])
        if mask_inc.any():
            thr = np.quantile(df.loc[mask_inc, 'strata_score'].to_numpy(), args.low_score_q)
            low_mask = mask_inc & (df['strata_score'] <= thr)
            df.loc[low_mask, 'include'] = 0

    # 3) Assign strata among include==1
    if args.per_image_quantiles:
        strata = assign_strata_per_image(df)
    else:
        strata = assign_strata_global(df)

    if args.respect_existing and 'stratum' in df.columns:
        keep_mask = df['stratum'].notna()
        df['stratum_auto'] = strata
        df.loc[~keep_mask, 'stratum'] = strata[~keep_mask]
        df['stratum'] = df['stratum'].astype('Int64')
    else:
        df['stratum'] = pd.Series(strata).astype('Int64')
        df['stratum_auto'] = df['stratum']

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out_path, index=False)
    print(f"Wrote: {out_path}")

    # Optional overlays
    if args.debug_overlays:
        overlay_dir = args.overlay_dir or str(out_path.parent / 'overlays')
        save_debug_overlays(df, overlay_dir)
        print(f"Overlays saved to: {overlay_dir}")


if __name__ == '__main__':
    main()
