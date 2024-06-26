"""ID the images with trait and collection number
Arguments:
<image_name>    =   image name to start with (Optional)
<orientation>   =   'v' or 'h'

Please change the relative working directory on line .
"""
import cv2
import numpy as np
import matplotlib.pyplot as plt
import os
import sys
import tkinter as tk
from tkinter import simpledialog
from PIL import Image
from PIL.ExifTags import TAGS


def rotate_bound(image, angle):
	# grab the dimensions of the image and then determine the
	# center
	(h, w) = image.shape[:2]
	(cX, cY) = (w // 2, h // 2)

	# grab the rotation matrix (applying the negative of the
	# angle to rotate clockwise), then grab the sine and cosine
	# (i.e., the rotation components of the matrix)
	M = cv2.getRotationMatrix2D((cX, cY), -angle, 1.0)
	cos = np.abs(M[0, 0])
	sin = np.abs(M[0, 1])

	# compute the new bounding dimensions of the image
	nW = int((h * sin) + (w * cos))
	nH = int((h * cos) + (w * sin))

	# adjust the rotation matrix to take into account translation
	M[0, 2] += (nW / 2) - cX
	M[1, 2] += (nH / 2) - cY

	# perform the actual rotation and return the image
	return cv2.warpAffine(image, M, (nW, nH))

def auto_rotate(image, orientation):
	width = image.shape[1]
	height = image.shape[0]
	if orientation == 'v':
		if width > height:
			image = rotate_bound(image, -90)
		else:
			if np.mean(image[image.shape[0] - 500:, image.shape[1] - 500:,: ]) < 128:
				image = rotate_bound(image, 180)
	if orientation == 'h':
		if width < height:
			image = rotate_bound(image, 90)
	return image

# Python method chdir() changes the current working directory to the given path.It returns None in all the cases.
# os.chdir("C:/Users/kuowe/OneDrive - Washington University in St. Louis/Occidentale_Pallescens/Leaf_area_analysis")

path = "./OneDrive_1_6-12-2024/"

print(len(sys.argv))
if len(sys.argv) == 2:
	log_file = open(os.path.join(path, "log.txt"), 'w')
	log_file.write('count\timage_name\tdate_time\tid\n')
	image_names = os.listdir(path)
	passed = 0
elif len(sys.argv) == 3:
	log_file = open(os.path.join(path, "log.txt"), 'a')
	image_names = os.listdir(path)
	print(image_names)
	passed = 0
	for name in image_names:
		if name != str(sys.argv[1]):
			passed += 1
		else:
			break
	image_names= image_names[passed: len(image_names)]
	print(image_names)
else:
	sys.exit(__doc__)
 
image_names = sorted(image_names)
print(image_names)

id = '???'
for i, image_name in enumerate(image_names):
	if image_name[-3:] != 'jpg':
		continue
	
	check = False
	print(image_name)

	try:
		id = int(id.lstrip('0'))
	except:
		print("id is not an integer") 
	
	if isinstance(id, int):
		id = id + 1
		id = f'{id:03}'
	else:
		id = '???'
	
	image = cv2.imread(os.path.join(path, image_name))
	image = auto_rotate(image, sys.argv[-1])
	# crop the image for easy-handling
	image = image[:, :, :]
	image_x = image.shape[1]
	image_y = image.shape[0]
	image_copy = image.copy()
	cv2.putText(image_copy,  id, (int(image_x/2 - 200), int(image_y*0.9 - 50)), cv2.FONT_HERSHEY_SIMPLEX, 24, (30, 30, 255), 40)
	cv2.putText(image_copy,  'unsaved', (int(image_x*0.75), int(image_y*0.15)), cv2.FONT_HERSHEY_SIMPLEX, 4, (30, 30, 255), 16)
	# cv2.namedWindow(image_name, cv2.WINDOW_KEEPRATIO)
	# cv2.moveWindow(image_name, 0, 0)

	# read the image data using PIL
	image_data = Image.open(os.path.join(path, image_name))
	# extract EXIF data
	exifdata = image_data._getexif()

	# for (k,v) in Image.open(os.path.join(path, image_name))._getexif().items():
	# 	print('%s = %s' % (TAGS.get(k), v))

	# iterating over all EXIF data fields
	exif_out = {}
	for tag_id in exifdata:
		# get the tag name, instead of human unreadable tag id
		tag = TAGS.get(tag_id, tag_id)
		data = exifdata.get(tag_id)
		# decode bytes 
		if isinstance(data, bytes):
			data = data.decode()
		exif_out[tag] = data

	DateTime = exif_out['DateTimeOriginal']

	
	# trait_V = None
	# trait_R = None

	ROOT = tk.Tk()
	# ROOT.focus_get()
	ROOT.withdraw()
	w = 400; h = 200; x = 10; y = 10
	ROOT.geometry('%dx%d+%d+%d' % (w, h, x, y))


	finish = False
	while not finish:
		cv2.namedWindow("clover", cv2.WINDOW_NORMAL)
		cv2.imshow("clover", image_copy)
		cv2.resizeWindow("clover", 800, 800) 
		key = cv2.waitKey(0)

		if key == ord('i'):
			id = simpledialog.askstring(title="id", prompt="What's the id?")
			# check it out
			print(id)
			image_copy = image.copy()
			cv2.putText(image_copy,  id, (int(image_x/2 - 200), int(image_y*0.9 - 50)), cv2.FONT_HERSHEY_SIMPLEX, 24, (30, 255, 30), 40)
			cv2.putText(image_copy,  'unsaved', (int(image_x*0.75), int(image_y*0.15)), cv2.FONT_HERSHEY_SIMPLEX, 4, (30, 30, 255), 16)
			cv2.namedWindow("clover", cv2.WINDOW_NORMAL)
			cv2.imshow("clover", image_copy)
			cv2.resizeWindow("clover", 800, 800)

		elif key == ord('q'):
			log_file.close()
			sys.exit('program ends manually')

		elif key == 13: # return key
			image_copy = image.copy()
			cv2.putText(image_copy,  id, (int(image_x/2 - 200), int(image_y*0.9 - 50)), cv2.FONT_HERSHEY_SIMPLEX, 24, (30, 255, 30), 40)
			cv2.putText(image_copy,  'saved', (int(image_x*0.75), int(image_y*0.15)), cv2.FONT_HERSHEY_SIMPLEX, 4, (30, 255, 30), 16)
			cv2.namedWindow("clover", cv2.WINDOW_NORMAL)
			cv2.imshow("clover", image_copy)
			cv2.resizeWindow("clover", 800, 800)
			check = True

		elif key == 27: # Esc key
			finish = True

	# print('DG_F3_' + str(id))
	# print(trait_V)
	# print(trait_R)


	if check == True:
		print(i+1 + passed, image_name, DateTime, str(id))
		print('............................................')
		log_file.write(str(i+1+passed) + '\t' + image_name + '\t' + DateTime + '\t' + str(id) + '\n')
log_file.close()
