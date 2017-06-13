.PHONY: all clean

all:
	jbuilder build

clean:
	rm -rf *.install _build
