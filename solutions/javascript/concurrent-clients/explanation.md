Just like the previous stage, this one too requires zero code changes to pass!

In most languages, passing this stage would involve spawning a thread/process to handle each client that the server
accepts. When using JavaScript we don't need to do this because the event handler we passed in to `createServer` event
will automatically fire again when the server accepts more clients.
