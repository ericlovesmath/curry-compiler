FNAME = output

# clang -arch x86_64 -c $(FNAME).s -o $(FNAME).o
# clang -arch x86_64 $(FNAME).o -o $(FNAME)
# rm $(FNAME).o
# arch -x86_64 ./$(FNAME)
# echo "Return Code: $$?"

all:
	nasm -f macho64 $(FNAME).asm
	gcc -arch x86_64 -ld_classic -o $(FNAME) $(FNAME).o
	rm $(FNAME).o
	./$(FNAME)

clean:
	rm $(FNAME)
