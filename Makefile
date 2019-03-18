all:
	clang++ -std=c++17 tuple_iterator.cc -O3 -o tuple_iterator.exe

clean:
	rm tuple_iterator.exe
