#include <r_core.h>
#include <r_socket.h>
#include <stdlib.h>
#include <string.h>

#define SETDESC(x, y) r_config_node_desc (x, y)
#define SETPREF(x, y, z) SETDESC (r_config_set (core->config, x, y), z)

const uint32_t PORT = 11111;
static char *radeco_path = "radeco";

void read_radeco_output(RSocketProc *proc) {
    char c, s[3];
    memset (s, '\0', sizeof (s));
    while (read (proc->fd1[0], &c, 1) != -1) {
        printf ("%c", c);
        s[0] = s[1];
        s[1] = s[2];
        s[2] = c;
        if (!strncmp (s, "END", 3)) {
            break;
        }
    }
}

void spawn_http_srv(RCore *core) {
    static bool is_called = false;
    if (core == NULL) {
        return;
    }
    if (!is_called) {
        char port_str[10];
        SETPREF ("http.log", "false", "Show HTTP requests processed");
        SETPREF ("http.sandbox", "false", "Show HTTP requests processed");
        snprintf (port_str, 9, " %u", PORT);
        r_core_rtr_http (core, '&', '\0', port_str);
    }
    is_called = true;
}

void usage() {
    eprintf ("Usage: pde[ ?ac] <func> plugin for radeco\n");
    eprintf ("| pde <func>   decompile current function\n");
    eprintf ("| pde?         show this help\n");
    eprintf ("| pdea <func>  analyze current function with radeco\n");
    eprintf ("| pdec         send information to radeco\n");
    eprintf ("| pder <cmd>   send <cmd> to radeco directly\n");
    eprintf ("| pdes         respawn radeco subprocess\n");
}

// TODO Replace with `r_socket_proc_open`
RSocketProc *open_proc(char *const argv[]) {
    RSocketProc *proc = R_NEW (RSocketProc);
    pipe (proc->fd0);
    pipe (proc->fd1);
    if ((proc->pid = r_sys_fork()) > 0) {
        close (proc->fd0[0]);
        close (proc->fd1[1]);
        return proc;
    } else {
        close (proc->fd0[1]);
        close (proc->fd1[0]);
        dup2 (proc->fd0[0], 0);
        dup2 (proc->fd1[1], 1);
        execvp (argv[0], argv);
        exit (0);
    }
}

RSocketProc *spawn_radeco() {
    char *const argv[] = {radeco_path, "--append", NULL};
    return open_proc (argv);
}

// TODO Replace with `r_socket_proc_printf`
void proc_sendf(RSocketProc *sp, const char *fmt, ...) {
    const uint32_t BUFFER_SIZE = 0x100;
    char buf[BUFFER_SIZE + 1];
    va_list ap;
    va_start (ap, fmt);
    const int n = vsnprintf (buf, BUFFER_SIZE, fmt, ap);
    write (sp->fd0[1], buf, n);
    va_end (ap);
}

int cmd_pde(const char *input) {
    static RSocketProc *radeco_proc = NULL;
    if (!radeco_proc) {
        radeco_proc = spawn_radeco();
        if (!radeco_proc) {
            eprintf("Spawning radeco process failed\n");
            return true;
        }
    }

    if (!input) {
        return true;
    }
    const char *query = input + 1;
    switch (input[0]) {
    case ' ':
        proc_sendf (radeco_proc, "decompile %s\n", query);
        read_radeco_output (radeco_proc);
        break;
    case 'a':
        proc_sendf (radeco_proc, "analyze %s\n", query);
        read_radeco_output (radeco_proc);
        break;
    case 'c':
        proc_sendf (radeco_proc, "connect http://localhost:%u\n", PORT);
        read_radeco_output (radeco_proc);
        break;
    case 'r':
        proc_sendf (radeco_proc, "%s\n", query);
        read_radeco_output (radeco_proc);
        break;
    case 's':
        radeco_proc = spawn_radeco ();
        if (!radeco_proc) {
            eprintf ("Spawning radeco process failed\n");
            return true;
        }
        break;
    case '\0':
    case '?':
    default:
        usage ();
    }
    return true;
}

int cmd(void *user, const char *input) {
    if (strncmp ("pde", input, 3)) {
        return false;
    }
    spawn_http_srv ((RCore *)user);
    cmd_pde (input + 3);
    return true;
}

int init(void *user, const char *_input) { return true; };

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
