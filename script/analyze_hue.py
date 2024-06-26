"""Analyze the green area of the images
Arguments:
<pixels_per_mm>    =   pixels_per_mm. Find this value by using ImageJ for every ColorPassport scale of the day.
"""

import cv2
import numpy as np
import matplotlib.pyplot as plt
import os
import sys


image_path = "./OneDrive_1_6-12-2024/"
output_path = "./OneDrive_1_6-12-2024/analyzed_darkgreen/"
pixels_per_mm = float(sys.argv[1])

os.makedirs(output_path, exist_ok = True)


def midpoint(ptA, ptB):
	return ((ptA[0] + ptB[0]) * 0.5, (ptA[1] + ptB[1]) * 0.5)

def HSV_color_selection(BGR_image, lower_color, upper_color):
	"""
	the function convert BGR to HSV color space, make color selection
	return a mask wtih black color on selected color
	"""
	HSV_image = cv2.cvtColor(BGR_image, cv2.COLOR_BGR2HSV)
	# define range of blue color in HSV
	lower_color = np.array(lower_color)
	upper_color = np.array(upper_color)
	# Threshold the HSV image to get only blue colors
	mask = cv2.inRange(HSV_image, lower_color, upper_color)

	# https://stackoverflow.com/questions/30369031/remove-spurious-small-islands-of-noise-in-an-image-python-opencv
	se1 = cv2.getStructuringElement(cv2.MORPH_RECT, (5,5))
	se2 = cv2.getStructuringElement(cv2.MORPH_RECT, (2,2))
	mask = cv2.morphologyEx(mask, cv2.MORPH_CLOSE, se1)
	mask = cv2.morphologyEx(mask, cv2.MORPH_OPEN, se2)

	rev_mask = cv2.bitwise_not(mask)
	return rev_mask

def get_leaf_area(image, pixels_per_mm):
	# Give a BGR values to define the color threshold

	# original
	# green = HSV_color_selection(image, [32, 51, 65], [80, 255, 250])

	# darker green
	green = HSV_color_selection(image, [32, 45, 40], [80, 255, 250])

	green_count = np.count_nonzero(green == 0)
	green_area = green_count/ float(pixels_per_mm)**2
	pink_image = np.zeros((image.shape[0], image.shape[1], 3), dtype = np.uint8)
	pink_image[:,:,0] = 255
	pink_image[:,:,1] = 90
	pink_image[:,:,2] = 255
	pink_mask = cv2.bitwise_and(pink_image, pink_image, mask = cv2.bitwise_not(green))
	# plt.imshow(cv2.cvtColor(pink_image, cv2.COLOR_BGR2RGB))
	# plt.show(block=False)
	# plt.waitforbuttonpress(0)
	# plt.close()
	green_mask_image = cv2.bitwise_and(image, image, mask = green)
	green_mask_image = green_mask_image + pink_mask
	return green_area, green_mask_image


# read all information from the log file and store in id_dict
id_dict = {}

# read image names and store in image_names
image_names = []

# start reading the log file
with open(os.path.join(image_path, "log.txt"), 'r') as log_file:
	header = log_file.readline()
	for i, line in enumerate(log_file):
		line = line.strip('\n').split('\t')

		# read image_name
		id_dict[i] = [line[1]]

		# read date_time
		id_dict[i].append(line[2])

		# read id
		id_dict[i].append(line[3])

		image_names.append(line[1])

# print(id_dict)

# image_names = os.listdir(image_path)
image_files = os.listdir(image_path)
# print(image_files)
data_output = open(os.path.join(output_path, 'data_output.txt'), 'w')
line = ['count', 'image_name', 'date_time', 'id', 'pixels_per_mm','leaf_area']
line = '\t'.join(line)
data_output.write(line + '\n')

if pixels_per_mm == None:
		stop('pixels_per_mm is not provided')

for i, image_name in enumerate(image_names):
	if image_name not in image_files:
		print('The file of ' + image_name + ' cannot be found')
		continue
	
	if os.path.exists(os.path.join(image_path, image_name)):
		image = cv2.imread(os.path.join(image_path, image_name))
	else:
		print("file not exists")
		continue
	# cv2.putText(image, sys.argv[1], (50, image.shape[0] - 50), cv2.FONT_HERSHEY_SIMPLEX, 1, (0, 0, 0), 2)


	orig = np.copy(image)
	
	orig_x = orig.shape[1]
	orig_y = orig.shape[0]

	five_cm = 50*pixels_per_mm


	cv2.line(image, (int((orig_x/2) - (five_cm/2)), int(orig_y*0.1)), (int((orig_x/2) + (five_cm/2)), int(orig_y*0.1)), (30, 255, 30), 10)
	cv2.putText(image, "{:.1f}cm".format(5), (int(orig_x/2 - 100), int(orig_y*0.1 - 50)), cv2.FONT_HERSHEY_SIMPLEX, 2, (30, 255, 30), 5)

	cv2.putText(image,  id_dict[i][0], (int(orig_x/2 - 200), int(orig_y*0.9 - 50)), cv2.FONT_HERSHEY_SIMPLEX, 2, (30, 255, 30), 5)
	cv2.putText(image,  id_dict[i][1], (int(orig_x/2 - 200), int(orig_y*0.9 + 50)), cv2.FONT_HERSHEY_SIMPLEX, 2, (30, 255, 30), 5)
	cv2.putText(image,  id_dict[i][2], (int(orig_x/2 - 200), int(orig_y*0.9 + 150)), cv2.FONT_HERSHEY_SIMPLEX, 2, (30, 255, 30), 5)

	date_time = id_dict[i][1]
	id_ = id_dict[i][2]

	green_area, green_mask_image = get_leaf_area(orig, pixels_per_mm)

	# total_area, red_area, yellow_area, red_mask_image, yellow_mask_image = dewlap_area_HSV(image, mask, species, pixels_per_mm)

	# scale_count, scale_area, image_contour1, scale_square = scale_density(image, mask, species, pixels_per_mm, 0)
	# image_contour2, head_len = head_length(image_contour1, cv2.imread(os.path.join(headlength_path, image_name), 0), pixels_per_mm)
	line = [i, image_name, date_time, id_, pixels_per_mm, green_area]
	line = '\t'.join([str(element) for element in line])
	data_output.write(line + '\n')
	# hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)
	# define range of blue color in HSV
	# lower_blue = np.array([110,50,50])
	# upper_blue = np.array([130,255,255])
	# # Threshold the HSV image to get only blue colors
	# mask = cv2.inRange(hsv, lower_blue, upper_blue)
	# # Bitwise-AND mask and original image
	# res = cv2.bitwise_and(frame,frame, mask= mask)
	# contours, hierarchy = cv2.findContours(mask, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
	# cnt_areas = []
	# for c in contours:
	#     area = cv2.contourArea(c)
	#     cnt_areas.append(area)
	# sort_conts = [cnt for _, cnt in sorted(zip(cnt_areas, contours), reverse = True, key = itemgetter(0))]
	# output_image = cv2.drawContours(image_contour2, sort_conts, 1, (0,0,255), 3)
	# # plt.imshow(cv2.cvtColor(output_image, cv2.COLOR_BGR2RGB))
	# # plt.show()
	# output_width, output_height = output_image.shape[1], output_image.shape[0]
	# scale_square_widh, scale_square_height = scale_square.shape[1], scale_square.shape[0]
	# output_image[output_height-scale_square_height: output_height, output_width - scale_square_widh: output_width, :] = scale_square
	# plt.imshow(cv2.cvtColor(output_image, cv2.COLOR_BGR2RGB))
	# plt.show()
	output_image = np.concatenate([image, green_mask_image], axis=1)
	cv2.imwrite(os.path.join(output_path, image_name), output_image)
data_output.close()
