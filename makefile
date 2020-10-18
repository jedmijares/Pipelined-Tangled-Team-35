tangled: tangled.v assembly
	iverilog -o tangled tangled.v
	vvp tangled

dump: dump.txt
	gtkwave dump.txt

assembly: tangled.aik testAssembly branchTest
	./aik tangled.aik testAssembly
	./aik tangled.aik branchTest

clean:
	rm floaty *.text *.data tangled
