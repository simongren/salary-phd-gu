"""Quick inspector to understand the raw text layout of the salary PDFs."""
import pypdf
import pathlib

BASE = pathlib.Path(__file__).parent.parent / "documents"

for year in [2022, 2023, 2024, 2025]:
    pdf_path = BASE / f"{year}.pdf"
    print(f"\n{'='*70}")
    print(f"  {year}.pdf")
    print(f"{'='*70}")
    reader = pypdf.PdfReader(str(pdf_path))
    print(f"  Pages: {len(reader.pages)}")
    for i, page in enumerate(reader.pages):
        text = page.extract_text() or ""
        print(f"\n--- Page {i+1} ---")
        print(text[:3000] if text else "[no text]")
        if i >= 3:
            print("  ... (truncated, showing first 4 pages only)")
            break
