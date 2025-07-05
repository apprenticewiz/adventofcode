APP = day01b

OBJS = $(APP).o

CXXFLAGS = -std=c++17 -O2

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(OBJS)
	$(CXX) -o $@ $(LDFLAGS) $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f *.o *.exe
