RADECO=../../target/debug/radeco
STAGES=r2,esil,cfg,ssa,const,dce

all:
	$(MAKE) clean
	$(CC) hello.c -o hello
	r2 -qc '#!pipe $(RADECO) -p $(STAGES)' hello
	find hello_out

clean:
	rm -rf hello_out
