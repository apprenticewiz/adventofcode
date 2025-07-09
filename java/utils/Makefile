all: geometry.jar

geometry.jar: 
	make -C aoc_utils/geometry
	jar cf $@ aoc_utils/geometry

clean:
	rm -f *.jar
	make -C aoc_utils/geometry clean
	make -C aoc_utils/md5 clean
