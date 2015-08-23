/// We re-export the standard TypedArena API from here, calling it
/// `ArenaMut` since it hands back `&mut`-refs.
pub use typed_arena::Arena as ArenaMut;

use std::cell::RefCell;
use std::mem;
use std::slice;

/// In addition, we offer the alternative `ArenaVex` API.
///
/// `ArenaVex` provides `&`-refs, not `&mut`-refs like `ArenaMut`
/// above.
///
/// In exchange for accepting this limitation, `ArenaVex` allows one
/// to traverse it (like `Vec`, unlike `TypedArena`) and also to
/// extend it via a `&`-ref (like `TypedArena`, unlike `Vec`)
///
/// "Vex" is a pun with the plural of `Vec`.
///
/// The word "Vex" is also appropriate because it is vexing that the
/// current version makes no attempt to live up to the guarantees of
/// its types; it uses RefCell::borrow_mut in its `fn push` method, so
/// different threads might *think* they can independently push onto
/// the `ArenaVex`, but this will actually be a race to cause a
/// panic. (There's not really a way to fix this cleanly apart from
/// stop using `Vec` as the underlying representation here. Maybe
/// building on top of Gankro's relatively new RawVec abstraction
/// would be a good idea here.)
pub struct ArenaVex<T> {
    chunks: RefCell<Box<Chunk<T>>>,
}

struct Chunk<T> {
    elems: Vec<T>,
    next: Option<Box<Chunk<T>>>,
}

pub struct ArenaVexIter<'a, T:'a> {
    iter: slice::Iter<'a, T>,
    next: Option<&'a Chunk<T>>,
}

impl<'a, T:'a> Iterator for ArenaVexIter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        loop {
            let r = self.iter.next();
            if r.is_some() { return r; }
            self.next = match self.next {
                None => return None,
                Some(chunk) => {
                    self.iter = chunk.elems.iter();
                    chunk.next.as_ref().map(|b| &**b)
                }
            };
        }
    }
}

impl<T> ArenaVex<T> {
    pub fn new() -> ArenaVex<T> { ArenaVex::with_capacity(8) }
    pub fn with_capacity(n: usize) -> ArenaVex<T> {
        ArenaVex {
            chunks: RefCell::new(Box::new(Chunk {
                elems: Vec::with_capacity(n),
                next: None }))
        }
    }

    pub fn iter<'a>(&'a self) -> ArenaVexIter<'a, T> {
        let r = &**self.chunks.borrow();
        ArenaVexIter {
            iter: [].iter(),
            next: unsafe { mem::transmute(Some(r)) },
        }
    }

    pub fn push(&self, value: T) -> &T {
        let mut chunks = self.chunks.borrow_mut();

        if chunks.elems.len() == chunks.elems.capacity() {
            let new_capacity = chunks.elems.capacity().checked_mul(2).unwrap();
            let new_elems = Vec::with_capacity(new_capacity);
            let mut chunk = Box::new(Chunk {
                elems: new_elems,
                next: None,
            });
            mem::swap(&mut *chunks, &mut chunk);
            chunks.next = Some(chunk);
        }
        assert!(chunks.elems.len() < chunks.elems.capacity());

        chunks.elems.push(value);
        return unsafe { mem::transmute(chunks.elems.last().unwrap()) };
    }
}
