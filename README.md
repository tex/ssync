# Stay in Sync

## What is ssync?

Ssync is a developer utility. It recompiles and reloads your rebarized Erlang
code on the fly. With Ssync, you can code without friction.

What does "code without friction" mean? It means that with Sync
running, you no longer need to worry about running `rebar compile`, or
`c:l(Module)` again. Just write code, save the file, and watch as
Erlang automatically detects your changes, recompiles the code, and
reloads the module.

## How can I use Ssync?

The recommended approach is to put ssync in your rebar.config deps.

Then, go in the Erlang console of an application you are developing,
run `ssync:start().`.

## Desktop notifications

If you are running on Linux install notify-send (Fedora: `libnotify` package,
Ubuntu: `libnotify-bin` package)

