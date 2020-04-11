all:
	happy -gca ParHarper.y
	alex -g LexHarper.x
	ghc --make TestHarper.hs -o TestHarper

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocHarper.* LexHarper.* ParHarper.* LayoutHarper.* SkelHarper.* PrintHarper.* TestHarper.* AbsHarper.* TestHarper ErrM.* SharedString.* ComposOp.* Harper.dtd XMLHarper.* Makefile*
	
