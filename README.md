# Crocker: cron docker

`crocker` is an entrypoint binary for a Docker image for periodic tasks.  It has two main purposes:

 1. Execution of another process on a schedule.

    Docker itself has no facility for periodic execution.  The usual solutions are to use host services to schedule
    container execution, or to embed a process for scheduled execution.  This solution is an example of the latter.

 2. Docker dissociates processes from `init`, and they must be reaped.

    It is possible to create zombie processes using Docker, when a program spawns another and doesn't reap it on exit.
    Usually, the `init` process will inherit all orphaned children and reap them, but the Docker process management
    approach separates processes inside a container from the host such that this no longer happens.  If you only run
    one process, this is not a concern, but if you spawn a child and don't successfully reap it, you will wind up
    with zombies.  For more details, see [this blog post][zombie].

Running `cron` or its variants inside the container works, but has drawbacks.  These usually aren't static binaries,
and usually depend on a system mailer and possibly a logging facility to work.  `crocker` is a static binary which
logs to standard out, including reporting error exit codes for the tasks it executes.

## Usage

    crocker [--at-start|-A] <cron exrepssion> <command> [<arg> ...]

CRON expressions are the usual. If the `--at-start` or `-A` option is given, the command is immediately executed on
launch.

A command will not be re-executed until it has completed its previous execution.  If a scheduled execution time elapses
before the command has completed, the command will be immediately re-executed on termination.  If more than one
scheduled execution time elapses before a command has completed, only one subsequent execution will be queued - a
process running very slowly will lose executions rather than overrun an execution queue.

Here's an example Dockerfile:

```Dockerfile
FROM busybox

ADD https://github.com/APNIC-net/crocker/releases/download/v0.2.0/crocker /crocker
RUN chmod a+x /crocker

ENTRYPOINT ["/crocker", "-A", "@daily", "/bin/echo", "it is a brand new day"]
```

The binary is static, so you may use it in a container from `scratch`, but you will need to mark it executable. This
means you cannot `ADD` it by URL.

## Credits

Many of the functions for handling sub-process reaping are based on the code in [pid1][pid1], though regrettably
nothing in that code base was usable directly, as it serves a different purpose.

[pid1]: https://github.com/fpco/pid1
[zombie]: https://blog.phusion.nl/2015/01/20/docker-and-the-pid-1-zombie-reaping-problem/
