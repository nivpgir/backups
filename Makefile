CC=g++
CFLAGS=-std=c++11 -Wall -pthread -g


framework: MapReduceFramework.cpp MapReduceClient.h MapReduceFramework.h shortcuts.h debug_helper.h
	$(CC) $(CFLAGS) -c $^ -o framework.o

clean:
	rm -rf framework.o
