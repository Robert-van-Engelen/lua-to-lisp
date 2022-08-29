CXX       = c++
REFLEX    = reflex
REFLAGS   =
LIBREFLEX = -lreflex

BISON     = bison

CXXOFLAGS = -O2
CXXWFLAGS = -Wall -Wunused -Wextra
CXXIFLAGS =
CXXMFLAGS =
CXXFLAGS  = $(CXXWFLAGS) $(CXXOFLAGS) $(CXXIFLAGS) $(CXXMFLAGS)

all:		lua2lisp

lua2lisp:	lua.l lua.y lua.hpp
		$(BISON) -d lua.y
		$(REFLEX) $(REFLAGS) lua.l
		$(CXX) -std=c++11 $(CXXFLAGS) -Wno-potentially-evaluated-expression -o $@ LuaParser.cpp LuaScanner.cpp $(LIBREFLEX)

.PHONY:		clean

clean:
		-rm -f LuaParser.cpp LuaParser.hpp LuaScanner.cpp LuaScanner.hpp lua2lisp
