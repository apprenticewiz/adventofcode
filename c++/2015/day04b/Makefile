APP = day04b

OBJS = $(APP).o md5.o

CXXFLAGS = -O2 -march=native -std=c++17 -I../..

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(OBJS)
	$(CXX) -o $@ $(LDFLAGS) $^

md5.o: ../../utils/md5.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(APP).o: $(APP).cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f *.o *.exe
