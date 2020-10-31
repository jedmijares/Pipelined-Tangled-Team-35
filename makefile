# pipelineTangled: pipelineTangled.v assembly
# 	iverilog -o pipelineTangled pipelineTangled.v
# 	vvp pipelineTangled

tangled: tangled.v assembly
	iverilog -o tangled tangled.v
	vvp tangled

dump: dump.txt
	gtkwave dump.txt

assembly: tangled.aik testAssembly
	./aik tangled.aik testAssembly
	./aik tangled.aik QatTest

clean:
	rm *.text *.data tangled

pinky: pipelinePinky.v
	iverilog -o pinky pipelinePinky.v
	vvp pinky
