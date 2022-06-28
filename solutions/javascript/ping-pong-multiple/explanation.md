In most languages, passing this stage would involve adding a loop to continuously read data and respond back with `PONG`. 

Since JavaScript's programming model is event-driven, we're able to pass the stage with zero changes! The event handler we
attached to the [`data`](https://nodejs.org/api/net.html#net_event_data) event will automatically fire again when the
client sends more commands.
