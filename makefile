tangled: tangled.v assembly
	iverilog -o tangled tangled.v
	vvp tangled

dump: dump.txt
	gtkwave dump.txt

assembly: tangled.aik testAssembly branchTest memoryTest
	./aik tangled.aik testAssembly
	./aik tangled.aik branchTest
	./aik tangled.aik memoryTest
	./aik tangled.aik notNegateTest
	./aik tangled.aik floatTest
	./aik tangled.aik shiftTest

clean:
	rm floaty *.text *.data tangled
