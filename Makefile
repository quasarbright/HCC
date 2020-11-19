UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  NASM_FORMAT=elf64
  CLANG_FLAGS=-mstackrealign -m64 -g -fstack-protector-all -Wstack-protector -fno-omit-frame-pointer
else
ifeq ($(UNAME), Darwin)
  NASM_FORMAT=macho64
  CLANG_FLAGS=-mstackrealign -m64 -g -fstack-protector-all -Wstack-protector -fno-omit-frame-pointer
endif
endif

%.run: %.o main.c
	clang $(CLANG_FLAGS) -o $@ main.c $<

%.o: %.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: %.s
%.s: %.c
	stack build
	stack exec HCC-exe -- $< > $@

clean:
	rm -r *.s *.o *.run *.run.dSYM