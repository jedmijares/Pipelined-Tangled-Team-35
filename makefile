dump: dump.txt tangled
	gtkwave dump.txt

tangled: tangled.v assembly
	iverilog -o tangled tangled.v
	vvp tangled
	
floaty: floaty.v
	iverilog -o floaty floaty.v

assembly: tangled.aik testAssembly
	./aik tangled.aik testAssembly



clean:
	rm floaty *.text *.data
