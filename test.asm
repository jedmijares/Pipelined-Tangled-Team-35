	.text
start:
	add	$1, $2
	addf	$1, $2
	and	$1, $2
	br	start
	brf	$1, start
	brt	$1, start
	copy	$1, $2
	float	$1
	int	$1
	jump	more
	jumpf	$1, start
	jumpr	$1
	jumpt	$1, start
	lex	$1, -1
	lhi	$1, 0x42
	load	$1, $2
	load	$1, 480
	mul	$1, $2
	mulf	$1, $2
	neg	$1
	negf	$1
	not	$1
	or	$1, $2
	recip	$1
	shift	$1, $2
	slt	$1, $2
	sltf	$1, $2
	store	$1, $2
	sys
	xor	$1,$2
	.data			; switch to data segment
	.word	0x42
	.text			; back to text segment
more:	and	@1, @2, @3
	ccnot	@1, @2, @3
	cnot	@1, @2
	cswap	@1, @2, @3
	had	@1, 0
	meas	$1, @1
	next	$1, @1
	not	@1
	or	@1, @2, @3
	one	@1
	swap	@1, @2
	xor	@1, @2, @3
	zero	@1

