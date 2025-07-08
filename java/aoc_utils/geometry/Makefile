all: geometry.jar

geometry.jar : Position2D.class Position3D.class
	jar cf $@ $^

Position2D.class : Position2D.java
	javac Position2D.java

Position3D.class : Position3D.java
	javac Position3D.java

clean:
	rm -f *.class *.jar
