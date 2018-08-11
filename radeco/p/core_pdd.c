#include <r_core.h>
#include <stdlib.h>
#include <string.h>

#define SETDESC(x, y) r_config_node_desc(x, y)
#define SETPREF(x, y, z) SETDESC(r_config_set(core->config, x, y), z)

const int PORT = 11111;
static char *radeco_path = "radeco";
static FILE *radeco_p = NULL;

void spawn_radeco() {
    if (radeco_p) {
        pclose(radeco_p);
    }
    radeco_p = popen(radeco_path, "w");
    setbuf(radeco_p, NULL);
}

void spawn_http_srv(RCore *core) {
    static bool is_called = false;
    if (core == NULL) {
        return;
    }
    if (!is_called) {
        char port_str[10];
        SETPREF("http.log", "false", "Show HTTP requests processed");
        SETPREF("http.sandbox", "false", "Show HTTP requests processed");
        snprintf(port_str, 9, " %d", PORT);
        r_core_rtr_http(core, '&', '\0', port_str);
    }
    is_called = true;
}

void usage() {
    eprintf("Usage: pdd[ ?ac] <func> plugin for radeco\n");
    eprintf("| pdd <func>   decompile current function\n");
    eprintf("| pdd?         show this help\n");
    eprintf("| pdda <func>  analyze current function with radeco\n");
    eprintf("| pddc         send information to radeco\n");
    eprintf("| pddr <cmd>   send <cmd> to radeco directly\n");
    eprintf("| pdds <cmd>   respawn radeco subprocess\n");
}

int cmd_pdd(const char *input) {
    if (input == NULL) {
        return true;
    }
    const char *query = input + 1;
    switch (input[0]) {
        case ' ':
            fprintf(radeco_p, "decompile %s\n", query);
            break;
        case 'a':
            fprintf(radeco_p, "analyze %s\n", query);
            break;
        case 'c':
            fprintf(radeco_p, "connect %d\n", PORT);
            break;
        case 'r':
            fprintf(radeco_p, "%s\n", query);
            break;
        case 's':
            spawn_radeco();
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
    spawn_radeco();
    return true;
};

RCorePlugin r_core_plugin_test = {.name = "radeco",
                                  .desc = "r2 interface for radeco",
                                  .license = "BSD 3-Clause",
                                  .call = cmd,
                                  .init = init};

#ifndef CORELIB
RLibStruct radare_plugin = {.type = R_LIB_TYPE_CORE,
                            .data = &r_core_plugin_test,
                            .version = R2_VERSION};
#endif
