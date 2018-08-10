#include <r_anal.h>
#include <r_cmd.h>
#include <r_cons.h>
#include <r_core.h>
#include <r_lib.h>
#include <r_types.h>
#include <stdlib.h>
#include <string.h>

const int PORT = 8080;
static RCore *core_link = 0;
static char *radeco_path = "./radeco";
static FILE *radeco_p = NULL;

void spawn_http_srv(RCore *core) {
    static bool is_called = false;
    if (core == NULL) {
        return;
    }
    if (!is_called) {
        char port_str[10];
        snprintf(port_str, 9, " %d", PORT);
        r_core_rtr_http(core, '&', '\0', port_str);
    }
    is_called = true;
}

void usage() {
    eprintf("Usage: pdd[ ?ac] <func> plugin for radeco\n");
    eprintf("| pdd <func>   decompile current function\n");
    eprintf("| pdd?         show this screen\n");
    eprintf("| pdda <func>  analyze current function with radeco\n");
    eprintf("| pddc         send information to radeco\n");
}

int cmd_pdd(const char *input) {
    if (input == NULL) {
        return true;
    }
    const char *func = input + 1;
    switch (input[0]) {
        case ' ':
            fprintf(radeco_p, "decompile %s\n", func);
            break;
        case 'a':
            fprintf(radeco_p, "analyze %s\n", func);
            break;
        case 'c':
            fprintf(radeco_p, "connect %d\n", PORT);
            break;
        case '\0':
        case '?':
        default:
            usage();
    }
    return true;
}

int cmd(void *user, const char *input) {
    if (strncmp("pdd", input, 3)) {
        return false;
    }
    spawn_http_srv((RCore *)user);
    cmd_pdd(input + 3);
    return true;
}

int init(void *user, const char *_input) {
    radeco_p = popen(radeco_path, "w");
    setbuf(radeco_p, NULL);
    return true;
};

RCorePlugin r_core_plugin_test = {.name = "radeco",
                                  .desc = "r2 interface for radeco",
                                  .license = "",
                                  .call = cmd,
                                  .init = init};

#ifndef CORELIB
RLibStruct radare_plugin = {.type = R_LIB_TYPE_CORE,
                            .data = &r_core_plugin_test,
                            .version = R2_VERSION};
#endif
