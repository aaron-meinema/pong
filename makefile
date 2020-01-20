pong.smc: pong.asm pong.link
	wla-65816 -o pong.obj pong.asm
	wlalink pong.link pong.smc
