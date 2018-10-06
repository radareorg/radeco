CFLAGS=$(shell pkg-config --cflags r_core)
LDFLAGS=$(shell pkg-config --libs r_core)
PLUGDIR=$(shell r2 -H R2_USER_PLUGINS)
LIBEXT=$(shell r2 -H LIBEXT)
PLUGNAME=radeco_pde
TARGET=$(PLUGNAME).$(LIBEXT)

all:
	$(MAKE) build
	$(MAKE) install

build:
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -fPIC $(PLUGNAME).c -o $(TARGET)

install:
	mkdir -p $(PLUGDIR)
	rm -rf $(PLUGDIR)/$(TARGET)
	cp -rf $(TARGET) $(PLUGDIR)/

uninstall:
	rm -rf $(PLUGDIR)/$(TARGET)

clean:
	rm -rf $(TARGET)
