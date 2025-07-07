APP = day03a

OBJS = $(APP).o

CXXFLAGS = -O2 -march=native -std=c++17 -I../..

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(OBJS)
	$(CXX) -o $@ $(LDFLAGS) $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f *.o *.exe
