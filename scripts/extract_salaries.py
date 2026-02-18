"""
extract_salaries.py
===================
AGENT ROLE: DATA EXTRACTOR
--------------------------
Reads all 4 negotiated PhD salary PDFs (2022–2025) from ../documents/
and outputs a tidy dataset to ../data/salaries.csv.

Column schema:
  year          – int  (effective year: 2022–2025)
  faculty       – str  (faculty / facultet group name)
  group_letter  – str  (A, B, C, …)
  group_name    – str  (description of the PhD group)
  salary_0pct   – int  (salary at 0% completion)
  salary_50pct  – int  (salary at 50% completion)
  salary_80pct  – int  (salary at 80% completion)
  salary_100pct – int  (salary at 100% / defended; NULL if individuellt)
  individuell   – bool (True if 100% level is set individually)
  effective_date– str  (YYYY-MM-DD of the pay revision)
"""

import re
import pathlib
from typing import Optional
import pypdf
import fitz          # pymupdf – used as fallback for image-based PDFs
import pandas as pd
import pytesseract
from PIL import Image

# Path to Tesseract binary (installed via winget)
pytesseract.pytesseract.tesseract_cmd = r"C:\Program Files\Tesseract-OCR\tesseract.exe"

BASE_DIR  = pathlib.Path(__file__).parent.parent
DOCS_DIR  = BASE_DIR / "documents"
DATA_DIR  = BASE_DIR / "data"
DATA_DIR.mkdir(exist_ok=True)

# ── Regex helpers ──────────────────────────────────────────────────────────────
# Swedish salary format: "31 100" (space thousands-sep) OR plain 5-digit number
SALARY_RE  = re.compile(r'\b(\d{2} \d{3}|\d{5})\b')

# Group entry: single letter A-F followed by ) and text
GROUP_RE   = re.compile(r'^([A-F])\)\s+(.*)')


def is_faculty_header(line: str) -> bool:
    """
    Detect a faculty/section header line.
    Criteria:
      - Not a group entry (A) … F))
      - No salary numbers
      - Contains at least one all-caps word of 4+ chars (e.g. SAHLGRENSKA, FAKULTETEN)
      - Does not start with a digit
    Works for both native PDF text (all-caps) and 2025 OCR output (mixed case).
    """
    stripped = line.strip()
    if not stripped:
        return False
    if GROUP_RE.match(stripped):
        return False
    if SALARY_RE.search(stripped):
        return False
    if stripped[0].isdigit():
        return False
    caps_words = re.findall(r'[A-ZÅÄÖÜ]{4,}', stripped)
    return len(caps_words) >= 1

# Effective date in the text
DATE_RE    = re.compile(r'(\d{4}-\d{2}-\d{2})')


def parse_int(s: str) -> int:
    return int(s.replace(' ', ''))


def extract_text_pypdf(pdf_path: pathlib.Path) -> list[str]:
    """Extract text from each page using pypdf."""
    reader = pypdf.PdfReader(str(pdf_path))
    return [page.extract_text() or '' for page in reader.pages]


def extract_text_fitz(pdf_path: pathlib.Path) -> list[str]:
    """Extract text from each page using pymupdf (fitz). Better for complex layouts."""
    doc = fitz.open(str(pdf_path))
    return [page.get_text() for page in doc]


def extract_text_ocr(pdf_path: pathlib.Path) -> list[str]:
    """
    Render each PDF page to an image and run Tesseract OCR.
    Used as a last resort for image-based (scanned) PDFs.
    Also fixes a common OCR transposition: e.g. '373 00' -> '37 300'.
    """
    doc = fitz.open(str(pdf_path))
    results = []
    for page in doc:
        mat = fitz.Matrix(3, 3)   # 3x zoom ~216 dpi, enough for Tesseract
        pix = page.get_pixmap(matrix=mat)
        img = Image.frombytes('RGB', (pix.width, pix.height), pix.samples)
        raw = pytesseract.image_to_string(img, lang='swe+eng', config='--psm 6')
        results.append(raw)
    return results


def find_table_page(pages: list) -> Optional[str]:
    """Return the text of the page containing the salary table (Bilaga 1)."""
    for text in pages:
        if '% av fullgjorda' in text and 'SAHLGRENSKA' in text:
            return text
    return None


def find_effective_date(pages: list[str]) -> str:
    """Find the first YYYY-MM-DD date that looks like the effective salary date."""
    for text in pages:
        # Look for "giltighet från och med" followed by a date
        m = re.search(r'giltighet från och med\s+(\d{4}-\d{2}-\d{2})', text)
        if m:
            return m.group(1)
    # Fallback: grab first date in any page that looks like an October date
    for text in pages:
        for d in DATE_RE.findall(text):
            if '-10-' in d:
                return d
    return ''


def flush_group(group: dict, rows: list) -> None:
    """Finalise a pending group dict and append a row to *rows*."""
    combined = ' '.join(group['parts'])
    salaries  = SALARY_RE.findall(combined)

    # Targeted OCR transposition fix: if we got only 3 salaries and the text
    # ends with a trailing 'NNN NN' pattern (e.g. '373 00' instead of '37 300'),
    # reconstruct the missing 4th salary.
    if len(salaries) == 3:
        trailing = re.search(r'\b(\d{3}) (\d{2})\s*$', combined.strip())
        if trailing:
            n3, n2 = trailing.group(1), trailing.group(2)
            # Reconstruct: shift the space one position right
            # e.g. '373 00' -> '37' + ' ' + '300'
            reconstructed = n3[:-1] + ' ' + n3[-1] + n2
            if re.match(r'^\d{2} \d{3}$', reconstructed):
                salaries.append(reconstructed)
    has_indiv = bool(re.search(r'individuellt', combined, re.IGNORECASE))

    # Clean group name: remove salary tokens and "individuellt"
    name = combined
    for s in salaries:
        name = name.replace(s, '')
    name = re.sub(r'\bindividuellt\b', '', name, flags=re.IGNORECASE)
    name = re.sub(r'\s+', ' ', name).strip()

    vals = [parse_int(s) for s in salaries]
    rows.append({
        'year':           group['year'],
        'effective_date': group['effective_date'],
        'faculty':        group['faculty'],
        'group_letter':   group['letter'],
        'group_name':     name,
        'salary_0pct':    vals[0] if len(vals) > 0 else None,
        'salary_50pct':   vals[1] if len(vals) > 1 else None,
        'salary_80pct':   vals[2] if len(vals) > 2 else None,
        'salary_100pct':  vals[3] if len(vals) > 3 else None,
        'individuell':    has_indiv,
    })


def parse_table(text: str, year: int, effective_date: str) -> list[dict]:
    """
    Parse the salary table from the 'Bilaga 1' page text.
    Returns a list of row dicts.
    """
    lines = text.splitlines()

    # Find the line containing the column headers (0  50  80  100)
    start = 0
    for i, line in enumerate(lines):
        if re.search(r'\b0\b.*\b50\b.*\b80\b.*\b100\b', line):
            start = i + 1
            break

    rows: list[dict] = []
    current_faculty_parts: list[str] = []
    current_faculty: str             = ''
    pending_group: Optional[dict]    = None

    for line in lines[start:]:
        stripped = line.strip()

        # ── Stop conditions ────────────────────────────────────────────────────
        if not stripped:
            continue
        # Match 'Bedömning' and OCR variants like 'Bed\xe4mning', 'Bed?mning' etc.
        if re.match(r'^Bed.{0,2}mning', stripped, re.IGNORECASE) or stripped.startswith('Certifierad'):
            if pending_group:
                flush_group(pending_group, rows)
                pending_group = None
            break

        # ── Faculty header ─────────────────────────────────────────────────────
        # Works for all-caps (2022-2024) and mixed-case OCR output (2025)
        if is_faculty_header(stripped):
            if pending_group:
                flush_group(pending_group, rows)
                pending_group = None
            # Multiple consecutive faculty names get joined (shared group section)
            current_faculty_parts.append(stripped.rstrip(':').strip())
            continue

        # ── Group entry ────────────────────────────────────────────────────────
        m = GROUP_RE.match(stripped)
        if m:
            if pending_group:
                flush_group(pending_group, rows)

            # Materialise accumulated faculty names on first group of a section
            if current_faculty_parts:
                current_faculty       = ' / '.join(current_faculty_parts)
                current_faculty_parts = []

            pending_group = {
                'year':           year,
                'effective_date': effective_date,
                'faculty':        current_faculty,
                'letter':         m.group(1),
                'parts':          [m.group(2).strip()],
            }
            continue

        # ── Continuation line (multi-line name or salaries on own line) ────────
        if pending_group is not None:
            pending_group['parts'].append(stripped)

    # Flush last group
    if pending_group:
        flush_group(pending_group, rows)

    return rows


def extract_year(year: int) -> list[dict]:
    pdf_path = DOCS_DIR / f"{year}.pdf"
    print(f"Processing {pdf_path.name} …", end=' ')

    # Try pypdf first, fall back to fitz
    pages = extract_text_pypdf(pdf_path)
    table_text = find_table_page(pages)

    if not table_text:
        print("pypdf found no table — trying fitz …", end=' ')
        pages = extract_text_fitz(pdf_path)
        table_text = find_table_page(pages)

    if not table_text:
        print("fitz found no table — trying OCR (Tesseract) …", end=' ')
        pages = extract_text_ocr(pdf_path)
        table_text = find_table_page(pages)

    if not table_text:
        print(f"WARNING: No salary table found in {year}.pdf — skipping.")
        return []

    effective_date = find_effective_date(pages)
    rows = parse_table(table_text, year, effective_date)
    print(f"OK  {len(rows)} rows extracted (effective {effective_date or 'unknown'})")
    return rows


def main():
    all_rows = []
    for year in [2022, 2023, 2024, 2025]:
        all_rows.extend(extract_year(year))

    df = pd.DataFrame(all_rows)

    # Normalise faculty names (strip, title-case comparison for deduplication later)
    df['faculty'] = df['faculty'].str.strip()

    out_path = DATA_DIR / "salaries.csv"
    df.to_csv(out_path, index=False, encoding='utf-8-sig')
    print(f"\nSaved {len(df)} rows -> {out_path}")
    print("\nPreview:")
    print(df.to_string(max_rows=20))


if __name__ == '__main__':
    main()
