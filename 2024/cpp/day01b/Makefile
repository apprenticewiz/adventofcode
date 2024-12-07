PROJECT = day01b

CXXFLAGS = -O3 -std=c++23 -march=native

all: $(PROJECT).exe

$(PROJECT).exe : $(PROJECT).o
	$(CXX) -o $@ $<

$(PROJECT).o : $(PROJECT).cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean :
	rm -f *.o *.exe
