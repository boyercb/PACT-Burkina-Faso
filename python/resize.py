# resize.py
# this script pre-processes the image files
import glob, os
from PIL import Image
import numpy as np

# final size and color specifications
bg_color = (255, 255, 255)
replacement_color = (191, 191, 191)
size = (523, 373)

# loop through files in img directory 
for infile in glob.glob("img/*_right.png"):

    # define file and extension
    file, ext = os.path.splitext(infile)

    # open file as image
    img = Image.open(infile)

    # convert to RGB
    img = img.convert('RGB')

    # convert to numpy array
    data = np.array(img)

    # standardize background color
    data[(data != bg_color).all(axis = -1)] = replacement_color

    # build new image from array
    img2 = Image.fromarray(data, mode='RGB')

    # resize to specification, but maintain aspect ratio
    img2.thumbnail(size, Image.ANTIALIAS)

    # create full size canvas
    bg = Image.new('RGB', size, (255, 255, 255))

    # paste unaliased image into full-size canvas
    bg.paste(img2, 
    	((size[0] - img2.size[0]) // 2, (size[1] - img2.size[1]) // 2))

    # save file
    bg.save(file + "_thumb2" + ext, "PNG")
