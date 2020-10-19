tangled: tangled.v assembly
	iverilog -o tangled tangled.v
	vvp tangled

dump: dump.txt
	gtkwave dump.txt

assembly: tangled.aik testAssembly branchTest memoryTest
	./aik tangled.aik testAssembly
	./aik tangled.aik QatTest

clean:
	rm *.text *.data tangled
