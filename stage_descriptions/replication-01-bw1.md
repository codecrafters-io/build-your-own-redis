Welcome to the Replication extension!

In this extension, you'll extend your Redis server to support [leader-follower replication](https://redis.io/docs/management/replication/). You'll be able to run
multiple Redis servers with one acting as the "master" and the others as "replicas". Changes made to the master will be automatically replicated to replicas.

Since we'll need to run multiple instances of your Redis server at once, we can't run all of them on port 6379.

In this stage, you'll add support for starting the Redis server on a custom port. The port number will be passed to your program via the `--port` flag.

### Tests

The tester will execute your program like this:

```
./your_program.sh --port 6380
```

It'll then try to connect to your TCP server on the specified port number (`6380` in the example above). If the connection succeeds, you'll pass this stage.

### Notes

- Your program still needs to pass the previous stages, so if `--port` isn't specified, you should default to port 6379.
- The tester will pass a random port number to your program, so you can't hardcode the port number from the example above.
- If your repository was created before 5th Oct 2023, it's possible that your `./your_program.sh` script
might not be passing arguments on to your program. You'll need to edit `./your_program.sh` to fix this, check
[this PR](https://github.com/codecrafters-io/build-your-own-redis/pull/89/files) for details.
