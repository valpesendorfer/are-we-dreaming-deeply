#!/usr/bin/env python

'''

Function to download images from Google Earth Engine

Prequisites:

- Google Earth engine account and locally setup python support
- Valid ID string of a GEE Image
- Coordinates to an AOI rectangle (even though this is optional for the function, big datasets may be not
	downloadable without clipping)


Valentin Pesendorfer, 2016
'''


import os,sys
import argparse
import ee
import urllib

#--------------------------------------------------------------------------------


#Parser

parser = argparse.ArgumentParser(description='Download image from Google Earth Engine',usage='%(prog)s GEEimage [-a xmin ymin xmax ymax] [-h]')
parser.add_argument("Image",help="Google Earth Engine Image string")
parser.add_argument("-a", "--aoi",help="Area of interest specified by rectangle xmin ymin xmax ymax",metavar="",nargs=4,type=float)
parser.add_argument("-o", "--output-filename",help="optional output filename",metavar="")
parser.add_argument("-p", "--output-path",help="optional output directory",metavar="")

if len(sys.argv)==1:
    parser.print_help()
    sys.exit(1)

args = parser.parse_args()
#print args
#--------------------------------------------------------------------------------

ee.Initialize()

image = ee.Image(args.Image)

if args.aoi is not None:
	aoi = ee.Geometry.Rectangle(args.aoi[0],args.aoi[1],args.aoi[2],args.aoi[3])
	image = image.clip(aoi)

if args.output_path is not None:
	out = args.output_path
else:
	out = os.getcwd()

if args.output_filename is not None:
	url=image.getDownloadURL({'name':args.output_filename})
	fname = args.output_filename+".zip"
	print "\nDownloading %s to %s" % (fname,out)

else:
	url=image.getDownloadURL()
	fname = args.Image.replace('/','_')+".zip"
	print "\nDownloading %s to %s" % (fname,out)

fname = out + "\\" + fname
	
urllib.urlretrieve(url,fname)






