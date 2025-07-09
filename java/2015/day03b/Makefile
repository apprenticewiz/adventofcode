all: Program.jar

Program.jar : Program.class
	@echo 'Main-Class: Program' > manifest.txt
	@echo 'Class-Path: ../../utils/geometry.jar' >> manifest.txt
	jar cfm $@ manifest.txt $^

Program.class : Program.java
	javac -classpath ../../utils/geometry.jar Program.java

clean:
	rm -f *.class *.jar manifest.txt
