CFLAGS=$(shell pkg-config --cflags r_core)
PLUGDIR=$(shell r2 -H R2_USER_PLUGINS)
LIBEXT=$(shell r2 -H LIBEXT)
TARGET=radeco_pdd.$(LIBEXT)

all:
	$(MAKE) build
	$(MAKE) install

build:
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -fPIC core_pdd.c -o $(TARGET)

install:
	mkdir -p $(PLUGDIR)
	rm -rf $(PLUGDIR)/$(TARGET)
	cp -rf $(TARGET) $(PLUGDIR)/

uninstall:
	rm -rf $(PLUGDIR)/$(TARGET)

clean:
	rm -rf $(TARGET)
