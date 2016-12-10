# Based on: https://gist.github.com/mattmcd/5905af957181a149833f
#
# Note: install libopencv-dev to get dependent libraries

VER = 2.4.9
SVER = 249

OPENCV_JAR = /usr/share/OpenCV/java/opencv-$(SVER).jar
OPENCV_SO = /usr/lib/jni/libopencv_java$(SVER).so

opt/opencv-$(SVER).jar: $(OPENCV_JAR)
	mkdir -p opt/
	cp $< opt/

opt/opencv-native-$(SVER).jar: $(OPENCV_SO) opt/opencv-$(SVER).jar
	mkdir -p opt/native/linux/x86_64
	cp $< opt/native/linux/x86_64/
	cd opt && jar -cMf ../$@ native

.PHONY: deps
deps: opt/opencv-native-$(SVER).jar
	lein localrepo install opt/opencv-$(SVER).jar opencv/opencv $(VER)
	lein localrepo install opt/opencv-native-$(SVER).jar opencv/opencv-native $(VER)

