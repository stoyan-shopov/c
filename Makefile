
all:
	bison -d -v -Wall -Wno-yacc --report=all -o calc++-parser.cc calc++-parser.yy
	flex -o calc++-scanner.cc calc++-scanner.ll
	g++ -std=c++11 -g *.cc -o calc++

clean:
	-rm calc++-parser.cc calc++-parser.hh calc++-scanner.cc calc-scanner.cc location.hh position.hh stack.hh

