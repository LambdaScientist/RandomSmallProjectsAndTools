all:
	happy -gca ParVhdl.y
	alex -g LexVhdl.x
	ghc --make TestVhdl.hs -o TestVhdl

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocVhdl.* LexVhdl.* ParVhdl.* LayoutVhdl.* SkelVhdl.* PrintVhdl.* TestVhdl.* AbsVhdl.* TestVhdl ErrM.* SharedString.* ComposOp.* vhdl2.dtd XMLVhdl.* Makefile*
	

