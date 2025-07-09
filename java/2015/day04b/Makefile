all: Program.jar

Program.jar : Program.class
	@echo 'Main-Class: Program' > manifest.txt
	jar cfm $@ manifest.txt $^

Program.class : Program.java
	javac Program.java

clean:
	rm -f *.class *.jar manifest.txt
