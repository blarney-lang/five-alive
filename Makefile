FiveAlive.v:
	cabal run

.PHONY: clean
clean:
	make -C software/hello clean
	make -C software/dhrystone clean
	make -C software/tests clean
	make -C sim clean
	make -C de10-pro-e clean
	cabal clean
	rm -f FiveAlive.v
