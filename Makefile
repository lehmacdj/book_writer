TARGET=book_writer

all: $(TARGET)

$(TARGET): book_writer.hs
	ghc --make $(TARGET)

.PHONY: clean
clean:
	rm *.hi *.o $(TARGET)
