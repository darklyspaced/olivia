use std::{collections::HashMap, mem};

/// An id that points to a interned `String`. Ideally within the compiler itself there should be
/// very few free floating `Strings` and should be `Symbol`s instead for efficiency.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

/// This interner primarily employs two strategies to be efficient. The first is that all strings
/// are concatenated and then adding strings just increments the pointer to the buffer. However, we need
/// address stability so when we need a new buffer, we allocate a new one but DON'T copy the old
/// one.
///
/// The interner internally then holds references to these bufs and hands them out as needed.
pub struct Interner {
    map: HashMap<&'static str, Symbol>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl Interner {
    pub fn with_capacity(cap: usize) -> Interner {
        let cap = cap.next_power_of_two();
        Interner {
            map: HashMap::default(),
            vec: Vec::new(),
            buf: String::with_capacity(cap),
            full: Vec::new(),
        }
    }

    /// Interns the given `name` and copies it and then returns it's new identity in the form of
    /// `Idx`
    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&id) = self.map.get(name) {
            return id;
        }
        let name = unsafe { self.alloc(name) };
        let id = Symbol(self.map.len() as u32);
        self.map.insert(name, id);
        self.vec.push(name);

        id
    }

    pub fn lookup(&self, id: Symbol) -> &str {
        self.vec[id.0 as usize]
    }

    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };

        // SAFETY: technically the lifetime of this isn't really 'static since once the Interner is
        // dropped, the references are no longer valid but the lifetimes are shortened when
        // returned via `lookup`
        unsafe { &*(interned as *const str) }
    }
}
