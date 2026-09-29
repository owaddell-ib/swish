#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "uv.h"

#define MAX_CHILDREN 2

static uv_process_t children[MAX_CHILDREN];
static int child_count;

static sigset_t sigchld_set;
static sigset_t old_sigmask;

static void on_child_exit(uv_process_t *process,
                          int64_t exit_status,
                          int term_signal)
{
    fprintf(stderr,
            "requested child exited: pid=%d status=%lld signal=%d\n",
            process->pid,
            (long long) exit_status,
            term_signal);

    uv_close((uv_handle_t *) process, NULL);
}

static const char *cld_code_name(int code)
{
    switch (code) {
    case CLD_EXITED:    return "CLD_EXITED";
    case CLD_KILLED:    return "CLD_KILLED";
    case CLD_DUMPED:    return "CLD_DUMPED";
    case CLD_STOPPED:   return "CLD_STOPPED";
    case CLD_TRAPPED:   return "CLD_TRAPPED";
    case CLD_CONTINUED: return "CLD_CONTINUED";
    default:            return "unknown";
    }
}

static int is_requested_child(pid_t pid)
{
    int i;

    for (i = 0; i < child_count; i++) {
        if (children[i].pid == pid)
            return 1;
    }

    return 0;
}

static void check_for_sigchld(int spawn_number)
{
    struct timespec timeout;
    siginfo_t info;
    int sig;

    /*
     * Give a pending SIGCHLD a little time to become visible.
     *
     * SIGCHLD remains blocked, so if one occurred during uv_spawn()
     * it cannot be consumed by libuv's signal handler before we inspect it.
     */
    timeout.tv_sec = 0;
    timeout.tv_nsec = 500 * 1000 * 1000; /* 500 ms */

    memset(&info, 0, sizeof(info));

    errno = 0;
    sig = sigtimedwait(&sigchld_set, &info, &timeout);

    if (sig == -1) {
        if (errno == EAGAIN) {
            fprintf(stderr,
                    "after spawn %d: no pending SIGCHLD\n",
                    spawn_number);
            return;
        }

        perror("sigtimedwait");
        exit(1);
    }

    fprintf(stderr,
            "after spawn %d: SIGCHLD pid=%d code=%s(%d) status=%d%s\n",
            spawn_number,
            (int) info.si_pid,
            cld_code_name(info.si_code),
            info.si_code,
            info.si_status,
            is_requested_child(info.si_pid)
                ? " [REQUESTED CHILD]"
                : " [OTHER CHILD]");
}

static void spawn_one(uv_loop_t *loop, int index)
{
    uv_process_options_t options;
    char *args[] = {
        "/bin/sleep",
        "30",
        NULL
    };
    int rc;

    memset(&children[index], 0, sizeof(children[index]));
    memset(&options, 0, sizeof(options));

    options.file = args[0];
    options.args = args;
    options.exit_cb = on_child_exit;

    fprintf(stderr, "\ncalling uv_spawn() #%d\n", index + 1);

    rc = uv_spawn(loop, &children[index], &options);
    if (rc != 0) {
        fprintf(stderr,
                "uv_spawn #%d: %s\n",
                index + 1,
                uv_strerror(rc));
        exit(1);
    }

    child_count = index + 1;

    fprintf(stderr,
            "uv_spawn #%d returned; requested child pid=%d\n",
            index + 1,
            children[index].pid);

    check_for_sigchld(index + 1);
}

int main(int argc, char **argv)
{
    uv_loop_t *loop;
    int spawn_count;
    int i;

    /*
     * Deliberately simple CLI:
     *
     *     ./repro 1
     *     ./repro 2
     */
    if (argc != 2 || (argv[1][0] != '1' && argv[1][0] != '2') ||
        argv[1][1] != '\0') {
        fprintf(stderr, "usage: %s 1|2\n", argv[0]);
        return 2;
    }

    spawn_count = argv[1][0] - '0';

    fprintf(stderr, "libuv version: %s\n", uv_version_string());
    fprintf(stderr, "test process pid: %d\n", (int) getpid());
    fprintf(stderr, "number of uv_spawn calls: %d\n", spawn_count);

    /*
     * Block SIGCHLD before libuv has an opportunity to spawn anything.
     *
     * The signal can still become pending. sigtimedwait() lets us consume
     * it synchronously and, crucially, obtain siginfo_t.si_pid.
     */
    sigemptyset(&sigchld_set);
    sigaddset(&sigchld_set, SIGCHLD);

    if (sigprocmask(SIG_BLOCK, &sigchld_set, &old_sigmask) != 0) {
        perror("sigprocmask(SIG_BLOCK)");
        return 1;
    }

    loop = uv_default_loop();

    for (i = 0; i < spawn_count; i++)
        spawn_one(loop, i);

    /*
     * Restore the normal signal mask before terminating our requested
     * children. From this point on libuv can receive their SIGCHLDs and
     * reap them normally.
     */
    if (sigprocmask(SIG_SETMASK, &old_sigmask, NULL) != 0) {
        perror("sigprocmask(SIG_SETMASK)");
        return 1;
    }

    fprintf(stderr, "\nterminating requested children\n");

    for (i = 0; i < child_count; i++) {
        int rc = uv_process_kill(&children[i], SIGTERM);

        if (rc != 0)
            fprintf(stderr,
                    "uv_process_kill pid=%d: %s\n",
                    children[i].pid,
                    uv_strerror(rc));
    }

    uv_run(loop, UV_RUN_DEFAULT);

    fprintf(stderr, "done\n");
    return 0;
}
