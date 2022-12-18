In Stage 6, we add in-memory storage.

Now that we can handle arguments, we can implement the `get <key>` and `set <key> <value>` commands. 

There is a new module called `store` which contains the in-memory store. Start by defining a `Store` struct, which is
just a wrapper around a `HashMap` for now.

```rust
use std::collections::HashMap;

pub struct Store {
    data: HashMap<String, String>
}
```

Then we can add an implementation for the `Store`

```rust
impl Store {
    pub fn new() -> Self {
        Store {
            data: HashMap::new(),
        }
    }

    pub fn set(&mut self, key: String, value: String) {
        self.data.insert(key, value);
    }

    pub fn get(&mut self, key: String) -> Option<String> {
        self.data.get(key.as_str()).cloned()
    }
}
```

Again, we're hiding the implementation of the underlying `HashMap` but defining an abstraction now will let us extend
the behaviour later, when we need to add expiring keys in the next section.

Now let's add the `Store` to the server. Because we want to have a single instance of the store the all clients can use, 
we need a way to safely share it between Tokio tasks we will use an `Arc` which is a reference counted pointer. You can
find the documentation here. To make sure only one task at a time accesses the `Store` we also need to wrap it in a 
Mutex. This comes together as 

```rust
let main_store = Arc::new(Mutex::new(Store::new()));
```

Then the store can be passed to each task by increasing the reference count, which is inherent to the `clone`, and 
moving the new reference.

```rust
let client_store = main_store.clone();
```

Finally, we have all the pieces and can update the command processing. Check `main.rs` to see these changes.
