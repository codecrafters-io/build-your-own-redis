In the final stage we're going to add expiring keys.

This is a Redis feature that allows the server to take responsibility for removing keys after a specified 
amount of time. The `set` command with expiry takes the form `set <key> <value> px <time>`.

We need to update the `Store` from part 6 so that it can store expiry alongside each value.

```rust
pub struct Store {
    data: HashMap<String, Entry>,
}

struct Entry {
    t: Option<Instant>,
    value: String,
}
```

A new function is needed in the `Store` implementation to store a key with an expiry time

```rust
pub fn set_px(&mut self, key: String, value: String, px: u64) {
    let entry = Entry {
        t: Some(Instant::now() + Duration::from_millis(px)),
        value,
    };
    self.data.insert(key, entry);
}
```

Notice that the expiry time is optional so that we don't break the previous use case for keys that don't expire.

The next job is to figure out how to expire keys. In a real system we would want to actively remove keys that have 
expired to avoid wasting memory. We could possibly implement this with a worker thread and a priority queue that keeps
track of upcoming expiry times. Let's keep things simple though and implement lazy expiry.

```rust
pub fn get(&mut self, key: String) -> Option<String> {
    match self.data.get(key.as_str()) {
        Some(entry) => {
            // Lazily expire keys as they are requested
            if let Some(t) = &entry.t {
                if Instant::now() > t.clone() {
                    self.data.remove(key.as_str());
                    return None;
                }
            }

            Some(entry.value.clone())
        }
        None => None,
    }
}
```

Now when a client tries to access a key that has passed its expiry time, the key is removed from 
storage and the client is told that the value for the key is null.

Finally, we can update the command handling to include set with expiry. Check `main.rs` these changes.
