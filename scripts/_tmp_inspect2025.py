import fitz
doc = fitz.open(r'c:\Users\xlunsi\OneDrive - Göteborgs Universitet\Skrivbordet\Privat\Data_viz\salary_phd_gu\documents\2025.pdf')
print('Pages:', len(doc))
for i, page in enumerate(doc):
    text = page.get_text()
    print(f'Page {i+1} chars: {len(text)}')
    print(text[:500] if text else '[empty]')
    print('Image count:', len(page.get_images()))
    blocks = page.get_text('blocks')
    print('Block count:', len(blocks))
    if blocks:
        for b in blocks[:3]:
            print(' block:', b)
