import glob, os
from PIL import Image
import numpy as np

bg_color = (255, 255, 255)
replacement_color = (191, 191, 191)
size = (523, 373)
for infile in glob.glob("*_right.png"):
    file, ext = os.path.splitext(infile)
    img = Image.open(infile)
    img = img.convert('RGB')
    data = np.array(img)
    data[(data != bg_color).all(axis = -1)] = replacement_color
    img2 = Image.fromarray(data, mode='RGB')
    img2.thumbnail(size, Image.ANTIALIAS)
    bg = Image.new('RGB', size, (255, 255, 255))
    bg.paste(img2, 
    	((size[0] - img2.size[0]) // 2, (size[1] - img2.size[1]) // 2))
    bg.save(file + "_thumb2" + ext, "PNG")
