from ij import IJ
import math, os, time, sys,traceback
from loci.plugins import BF
from ij.gui import GenericDialog, WaitForUserDialog
from ij.plugin.frame import RoiManager as rm
from ij.plugin.filter import ParticleAnalyzer, Analyzer
from ij.plugin.filter import ThresholdToSelection as tts
from ij.plugin import ZProjector as zp
from ij.measure import ResultsTable
from ij.measure import Measurements as ms
from ij.process import ImageProcessor, AutoThresholder
from ij.gui import Roi, ShapeRoi, Overlay
from ij.plugin import RoiEnlarger, Concatenator

#@ String msg(value="Calcium Imaging Analysis Plugin", visibility=MESSAGE, required=False)
#@ File (label="Select the folder with videos", style="directory") myFile
#@ String ext(label="Video-file type", value=".tif", persist=false)
#@ Float radius(label="Select an enlargment radius for point selections", value=10, persist=false)

luts = ImageProcessor.OVER_UNDER_LUT
IJ.run("Set Measurements...", "")
def zStackIJ(imp):
	z = zp(imp)
	return z.run(imp,"max all")

def loadfilenames(folderpath, ext):
	filenames = {}
	outputPath = os.path.join(folderpath, "Output-Tables")
	if not os.path.isdir(outputPath):
		os.makedirs(outputPath)
		
	if ext[0] != ".":
		ext = "." + ext
	for root, dirs, files in os.walk(folderpath):
		group = os.path.split(root)[1]
		print group
		for j in files:
			if os.path.splitext(os.path.join(root, j))[1] == ext:
				if group in filenames:
					filenames[group].append(os.path.join(root, j))
				else:
					filenames[group] = [os.path.join(root, j)]
					
	if not filenames:
		WaitForUserDialog("No files have been found. Please, check for correct file-extension (file-type) or for presence of images in the folder").show()
		sys.exit("Analysis cancelled!")
	else:
		return filenames, outputPath

def manualROI(mask, imp, background=False):
	IJ.run(imp, "Enhance Contrast", "saturated=0.35")
	IJ.run(mask, "Enhance Contrast", "saturated=0.35")
	imp.show()
	mask.setTitle("Mask, Select cells on this image")
	mask.show()
	IJ.run(mask, "Select None", "")
	
	IJ.setTool("multipoint")
	WaitForUserDialog("Select the center of the cells in the mask then proceed with okay").show()
	#r = mask.getRoi()
	roi = mask.getRoi()
	if roi is not None:
		#mask.hide()
		
		#mask.changes = False
		#mask.close()
		imp.hide()
		return roi
	
	else:
		gd = GenericDialog("No ROIs selected!")
		gd.enableYesNoCancel()
		gd.hideCancelButton()
		gd.addMessage("Do you want to repeat the selection or proceed with the next image?")
		gd.showDialog()
		if gd.wasOKed():
			manualROI(imp)
		else:
			print "Video skipped"
			return False


def formatTime(start):
	t = time.time() - start
	u = " seconds"
	if t > 60:
		t /= 60
		u = " minutes"
		if t > 60:
			t /= 60
			u = " hours"
			if t > 24:
				t /= 24
				u = " days"
	return str(round(t, 2)) + u
			
start = time.time()
inputPath = myFile.getAbsolutePath()
filenames, outputPath = loadfilenames(inputPath, ext)
IJ.run("Close All", "")
errors = []
for k,files in filenames.items():
	print k
	for f in files:
		try:
			print "Extracting spikes in %s" %f
			imp = BF.openImagePlus(f)[0]
			
			mask = zStackIJ(imp)
			IJ.run("Brightness/Contrast...")
			maskRoi = manualROI(mask, imp, background=False)
			if maskRoi:
				rm1 = rm().getRoiManager()
				rm1.reset()
				maskRoi = ShapeRoi(maskRoi)
				
				maskRois = maskRoi.getRois()
				maskRois = [RoiEnlarger().enlarge(ShapeRoi(mr), mask.getCalibration().getRawX(radius)) for mr in maskRois]

				[rm1.addRoi(mr) for mr in maskRois]

			if maskRoi:
				rm1.setSelectedIndexes(range(0, rm1.getCount()))
				rt = rm1.multiMeasure(imp)
				
				title = os.path.splitext(imp.getTitle())[0]
				rm1.setSelectedIndexes(range(0, rm1.getCount()))
				outputTable = os.path.join(outputPath, k + "_" + title +".csv")
				rt.saveAs(outputTable)
				rt.reset()
				rm1.reset()
			
			imp.changes = False
			mask.changes = False
			imp.close()
			mask.close()
	
		except:
			exc_type, exc_value, exc_traceback = sys.exc_info()
			lines = traceback.format_exception(exc_type, exc_value, exc_traceback)
			print ''.join('!! ' + line for line in lines)
			print "Analysis of image %s failed" % os.path.split(f)[1]
			errors.append(f)

nFiles = [len(v) for k, v in filenames.items()]
nFiles = sum(nFiles)
WaitForUserDialog(
			"Analysis is done! \n Number of images analyzed: %s \n Running time: %s \n Number of failed images: %s"
			% (nFiles,
			   formatTime(start),
			   len(errors))).show()
print "Number of images analyzed: ", nFiles
print 'It took', formatTime(start), 'seconds.'
for e in errors:
	print "Failed Images: ", e



	