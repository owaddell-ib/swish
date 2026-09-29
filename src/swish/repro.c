#include <signal.h>
#include <stdio.h>
#include <string.h>

#include "uv.h"

static uv_process_t child;
static uv_signal_t sigchld_watcher;
static uv_timer_t timer;

static int kill_requested = 0;
static int unexpected_sigchld = 0;

static void on_sigchld(uv_signal_t *handle, int signum)
{
    (void) signum;

    fprintf(stderr, "SIGCHLD callback invoked\n");

    if (!kill_requested) {
        fprintf(stderr,
                "UNEXPECTED: SIGCHLD arrived while requested child "
                "should still be sleeping\n");

        unexpected_sigchld = 1;

        /*
         * We've demonstrated the behavior. Don't observe the SIGCHLD
         * that will legitimately result from killing /bin/sleep later.
         */
        uv_signal_stop(handle);
        uv_close((uv_handle_t *) handle, NULL);
    }
}

static void on_timer(uv_timer_t *handle)
{
    int rc;

    fprintf(stderr, "timer fired; terminating requested child\n");

    kill_requested = 1;

    rc = uv_process_kill(&child, SIGTERM);
    if (rc != 0)
        fprintf(stderr, "uv_process_kill: %s\n", uv_strerror(rc));

    uv_timer_stop(handle);
    uv_close((uv_handle_t *) handle, NULL);
}

int main(void)
{
    uv_loop_t *loop;
    uv_process_options_t options;
    char *args[] = {
        "/bin/sleep",
        "30",
        NULL
    };
    int rc;

    fprintf(stderr, "libuv version: %s\n", uv_version_string());

    loop = uv_default_loop();

    rc = uv_signal_init(loop, &sigchld_watcher);
    if (rc != 0) {
        fprintf(stderr, "uv_signal_init: %s\n", uv_strerror(rc));
        return 1;
    }

    rc = uv_signal_start(&sigchld_watcher, on_sigchld, SIGCHLD);
    if (rc != 0) {
        fprintf(stderr, "uv_signal_start: %s\n", uv_strerror(rc));
        return 1;
    }

    memset(&child, 0, sizeof(child));
    memset(&options, 0, sizeof(options));

    options.file = args[0];
    options.args = args;
    options.exit_cb = on_child_exit;

    fprintf(stderr, "calling uv_spawn()\n");

    rc = uv_spawn(loop, &child, &options);
    if (rc != 0) {
        fprintf(stderr, "uv_spawn: %s\n", uv_strerror(rc));
        return 1;
    }

    fprintf(stderr,
            "uv_spawn returned; requested child pid=%d\n",
            child.pid);

    /*
     * Give the event loop time to deliver any SIGCHLD generated during
     * uv_spawn(), while the requested /bin/sleep should still be running.
     */
    rc = uv_timer_init(loop, &timer);
    if (rc != 0) {
        fprintf(stderr, "uv_timer_init: %s\n", uv_strerror(rc));
        return 1;
    }

    rc = uv_timer_start(&timer, on_timer, 500, 0);
    if (rc != 0) {
        fprintf(stderr, "uv_timer_start: %s\n", uv_strerror(rc));
        return 1;
    }

    uv_run(loop, UV_RUN_DEFAULT);

    uv_signal_stop(&sigchld_watcher);
    uv_close((uv_handle_t *) &sigchld_watcher, NULL);
    uv_run(loop, UV_RUN_DEFAULT);

    fprintf(stderr,
            "RESULT: %s\n",
            unexpected_sigchld
                ? "unexpected SIGCHLD observed"
                : "unexpected SIGCHLD not observed");

    return unexpected_sigchld ? 0 : 2;
}
