"""Render 2025.pdf pages to PNG for OCR inspection."""
import fitz, pathlib

BASE = pathlib.Path(r"c:\Users\xlunsi\OneDrive - Göteborgs Universitet\Skrivbordet\Privat\Data_viz\salary_phd_gu")
doc = fitz.open(str(BASE / "documents" / "2025.pdf"))

for i, page in enumerate(doc):
    mat = fitz.Matrix(3, 3)   # 3x zoom → ~216 dpi, good for OCR
    pix = page.get_pixmap(matrix=mat)
    out = BASE / "data" / f"2025_page{i+1}.png"
    pix.save(str(out))
    print(f"Saved {out}")
