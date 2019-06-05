main: double_cmp.o read.o
	g++ -Wall -Werror -Wpedantic -std=c++17 -pthread -g double_cmp.o read.o main.cpp

double_cmp.o:
	g++ -Wall -Werror -Wpedantic -std=c++17 -g -c double_cmp.cpp

read.o:
	g++ -Wall -Werror -Wpedantic -std=c++17 -g -c read.cpp

clean:
	rm *.o a.out