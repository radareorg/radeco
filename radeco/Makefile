all:
	make -C . uninstall
	make -C . install
	make -C plugin all

run:
	cargo run

install:
	cargo install
	make -C plugin

uninstall:
	cargo uninstall
	make -C plugin uninstall

clean:
	cargo clean
