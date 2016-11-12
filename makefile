.PHONY: clean fresh
mastermind: mastermind.f95
	gfortran mastermind.f95 -cpp -o mastermind
clean:
	rm -f mastermind
fresh: clean mastermind
