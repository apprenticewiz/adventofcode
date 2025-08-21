APP = day06a

OBJS = $(APP).o

CXXFLAGS = -O2 -std=c++17 -march=native -I../../utils

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(OBJS)
	$(CXX) -o $@ $(LDFLAGS) $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f *.o *.exe
