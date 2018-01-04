
all:
	bison -d -v -Werror -Wall -Wno-yacc --report=all -o calc++-parser.cc calc++-parser.yy
	flex -o calc++-scanner.cc calc++-scanner.ll
	g++ -g *.cc -o calc++

clean:
	-rm calc++-parser.cc calc++-parser.hh calc++-scanner.cc calc-scanner.cc location.hh position.hh stack.hh

