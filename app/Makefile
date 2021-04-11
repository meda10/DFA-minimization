OUT := dka-2-mka
ZIP := flp-fun-xmedek07

.PHONY: build
build: $(OUT)

$(OUT): *.hs
	ghc --make -o $@ $^

.PHONY: zip
zip: $(ZIP).zip

$(ZIP).zip: Makefile README.md *.hs
	zip -r $@ $^

.PHONY: clean
clean:
	rm -f $(ZIP).zip *.hi *.o $(OUT)