#NAME=dka-2-mka
#CC=ghc
#FLAGS=-o
#
#all: clean dka-2-mka
#
#dka-2-mka:
#	$(CC) $(FLAGS) $(NAME) Main.hs
#
#clean:
#	rm -f dka-2-mka
#
#

OUT := dka-2-mka
PACK := flp-fun-xmedek07
ARGS := -i

.PHONY: build
build: $(OUT)

$(OUT): *.hs
	ghc --make -o $@ $^

.PHONY: run
run: $(OUT)
	./$< $(ARGS)

.PHONY: pack
pack: $(PACK).zip

$(PACK).zip: Makefile README.md *.hs
	zip -r $@ $^

.PHONY: clean
clean:
	rm -f $(PACK).zip *.hi *.o $(OUT)

#
#EXECUTABLE=dka-2-mka
#SOURCE=dka-2-mka.hs FaModule.hs
#ZIPFILE=flp-fun-xkubis13.zip
#
#default: clear
#	ghc ${SOURCE} -o ${EXECUTABLE}
#
#zip:
#	zip ${ZIPFILE} ${SOURCE} README Makefile
#
#run: default
#	./${EXECUTABLE} ${arg1} ${arg2}
#
#clear:
#	rm -rf *.hi *.o ${EXECUTABLE} ${ZIPFILE}