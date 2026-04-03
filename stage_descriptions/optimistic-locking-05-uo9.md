In this stage, you'll add support for watching keys that don't exist yet.

### Watching Non-existent Keys

So far, you've only watched keys that already have values. But `WATCH` also works on keys that don't exist at the time of the call. If a watched key is created by another client before `EXEC`, the transaction should still abort.

```bash
# Client A
> WATCH foo
OK

# Client B
> SET foo 300
OK

# Client A
> MULTI
OK
> SET bar 500
QUEUED
> EXEC
*-1\r\n                # transaction aborted, foo was created after WATCH
```

The key didn't exist when Client A watched it, but Client B created it before `EXEC`. That counts as a modification.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

The tester will then spawn two clients. Using the first client, it will watch a key that doesn't exist:

```bash
# Client 1
> WATCH foo
OK
```

Using the second client, it will create the watched key:

```bash
# Client 2
> SET foo 200
OK
```

Back on the first client, the tester will start a transaction and attempt to execute it:

```bash
# Client 1
> MULTI
OK
> SET foo 300
QUEUED
> EXEC
*-1\r\n                # transaction aborted
> GET foo
"200"                  # unchanged, transaction had no effect
```

The tester will verify that:

- `EXEC` returns a RESP null array (`*-1\r\n`) when a watched key was created by another client
- The aborted transaction's queued commands had no effect

### Notes

- If your implementation already tracks modifications by flagging watched keys on any write command, this stage may already pass without changes.
- The key insight is that "modified" includes going from non-existent to existing. A key doesn't need to have a prior value to be considered touched.
