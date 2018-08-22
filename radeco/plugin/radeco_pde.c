#include <r_core.h>
#include <stdlib.h>
#include <string.h>

#define SETDESC(x, y) r_config_node_desc(x, y)
#define SETPREF(x, y, z) SETDESC(r_config_set(core->config, x, y), z)

const int PORT = 11111;
static char *radeco_path = "radeco";
static FILE *radeco_w = NULL;
static FILE *radeco_r = NULL;

void read_radeco_output() {
    char c, s[3];
    s[0] = s[1] = s[2] = '\0';
    while (c = fgetc(radeco_r)) {
        printf("%c", c);
        s[0] = s[1];
        s[1] = s[2];
        s[2] = c;
        if (!strncmp(s, "END", 3)) {
            break;
        }
    }
    fgetc(radeco_r);
}

void spawn_radeco() {
#ifdef __unix__
    static int user_pipe[2];
    static int radeco_pipe[2];
    pipe(user_pipe);
    pipe(radeco_pipe);
    if (fork() > 0) {
        close(user_pipe[1]);
        close(radeco_pipe[0]);
        radeco_w = fdopen(radeco_pipe[1], "w");
        radeco_r = fdopen(user_pipe[0], "r");
        setbuf(radeco_w, NULL);
        return;
    } else {
        close(user_pipe[0]);
        close(radeco_pipe[1]);
        dup2(radeco_pipe[0], 0);
        dup2(user_pipe[1], 1);
        execvp(radeco_path, NULL);
        exit(0);
    }
#else
    if (radeco_w) {
        pclose(radeco_w);
    }
    radeco_w = popen(radeco_path, "w");
    setbuf(radeco_w, NULL);
#endif
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
    eprintf("Usage: pde[ ?ac] <func> plugin for radeco\n");
    eprintf("| pde <func>   decompile current function\n");
    eprintf("| pde?         show this help\n");
    eprintf("| pdea <func>  analyze current function with radeco\n");
    eprintf("| pdec         send information to radeco\n");
    eprintf("| pder <cmd>   send <cmd> to radeco directly\n");
    eprintf("| pdes         respawn radeco subprocess\n");
}

int cmd_pdd(const char *input) {
    if (input == NULL) {
        return true;
    }
    const char *query = input + 1;
    switch (input[0]) {
        case ' ':
            fprintf(radeco_w, "decompile %s\n", query);
            break;
        case 'a':
            fprintf(radeco_w, "analyze %s\n", query);
            break;
        case 'c':
            fprintf(radeco_w, "connect http://localhost:%d\n", PORT);
            break;
        case 'r':
            fprintf(radeco_w, "%s\n", query);
            break;
        case 's':
            spawn_radeco();
            break;
        case '\0':
        case '?':
        default:
            usage();
    }
#ifdef __unix__
    read_radeco_output();
#endif
    return true;
}

int cmd(void *user, const char *input) {
    if (strncmp("pde", input, 3)) {
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
