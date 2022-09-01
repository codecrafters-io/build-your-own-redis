This is a starting point for {{language_name}} solutions to the
["Build Your Own Redis" Challenge](https://codecrafters.io/challenges/redis).

In this challenge, you'll build a toy Redis clone that's capable of handling
basic commands like `PING`, `SET` and `GET`. Along the way we'll learn about
event loops, the Redis protocol and more.

**Note**: If you're viewing this repo on GitHub, head over to
[codecrafters.io](https://codecrafters.io) to try the challenge.

# Passing the first stage

The entry point for your Redis implementation is in `{{ user_editable_file }}`. Study and uncomment the relevant code, and
push your changes to pass the first stage:

``` sh
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```

That's all!

# Stage 2 & beyond

Note: This section is for stages 2 and beyond.

1. Ensure you have `{{ required_executable }}` installed locally
1. Run `./spawn_redis_server.sh` to run your Redis server, which is implemented in
   `{{ user_editable_file }}`.{{# language_is_rust }} This command compiles your
   Rust project, so it might be slow the first time you run it. Subsequent runs
   will be fast.{{/ language_is_rust}}
1. Commit your changes and run `git push origin master` to submit your solution
   to CodeCrafters. Test output will be streamed to your terminal.


{{#language_is_python}}
# Troubleshooting

## module `socket` has no attribute `create_server`

When running your server locally, you might see an error like this:

```
Traceback (most recent call last):
  File "/.../python3.7/runpy.py", line 193, in _run_module_as_main
    "__main__", mod_spec)
  File "/.../python3.7/runpy.py", line 85, in _run_code
    exec(code, run_globals)
  File "/app/app/main.py", line 11, in <module>
    main()
  File "/app/app/main.py", line 6, in main
    s = socket.create_server(("localhost", 6379), reuse_port=True)
AttributeError: module 'socket' has no attribute 'create_server'
```

This is because `socket.create_server` was introduced in Python 3.8, and you
might be running an older version.

You can fix this by installing Python 3.8 locally and using that.

If you'd like to use a different version of Python, change the `language_pack`
value in `codecrafters.yml`.
{{/language_is_python}}
