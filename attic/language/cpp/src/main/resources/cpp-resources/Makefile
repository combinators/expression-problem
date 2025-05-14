CPPUTEST = cpputest
CPPFLAGS = -std=c++11
LIBS = -lCppUTest -lCppUTestExt 

test : *.cpp
	g++ *.cpp  -I $(CPPUTEST)/include -L $(CPPUTEST)/cpputest_build/lib $(LIBS) $(CPPFLAGS) -o test


