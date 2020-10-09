run: onesub
	vvp onesub
	
floaty: floaty.v
	iverilog -o floaty floaty.v

tangled: tangled.v
	iverilog -o tangled tangled.v
	vvp tangled

assembly: tangled.aik testAssembly
	./aik tangled.aik testAssembly

clean:
	rm floaty *.text *.data
