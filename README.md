# Crocker: cron docker

`crocker` launches a sub-process on a schedule, with no further dependencies.  All logging is to stdout.

## Usage

    crocker [--at-start|-A] <cron exrepssion> <command> [<arg> ...]

CRON expressions are the usual. If the `--at-start` or `-A` option is given, the command is immediately executed on
launch.

A command will not be re-executed until it has completed its previous execution.  If a scheduled execution time elapses
before the command has completed, the command will be immediately re-executed on termination.
