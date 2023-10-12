FiveAlive.v:
	cabal run

.PHONY: clean
clean:
	make -C software clean
	make -C sim clean
	make -C tests clean
	make -C dhrystone clean
	make -C de10-pro-e clean
	cabal clean
	rm -f FiveAlive.v
