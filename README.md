# Stay in Sync

## What is ssync?

Ssync is a developer utility. It recompiles and reloads your rebarized Erlang
code on the fly. With Ssync, you can code without friction.

What does "code without friction" mean? It means that with Sync
running, you no longer need to worry about running `rebar compile`, or
`c:l(Module)` again. Just write code, save the file, and watch as
Erlang automatically detects your changes, fetches dependencies,
recompiles the code, and reloads the module.

## How can I use Ssync?

The recommended approach is to put ssync in your rebar.config deps:

{deps, [
    {ssync, ".*", {git, "git://github.com/tex/ssync.git", "HEAD"}}
]}.

Then, run `rebar get-deps` for this last time manually, go in the
Erlang console of an application you are developing and run `ssync:start().`.

## Desktop notifications

If you are running on Linux install notify-send (Fedora: `libnotify` package,
Ubuntu: `libnotify-bin` package)

## Last notes

I am not responsible for any bad that might happen to you or to your computer
when using this application.

