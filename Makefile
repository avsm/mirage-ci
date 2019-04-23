.PHONY: all clean

all:
	dune build

clean:
	rm -rf *.install _build
