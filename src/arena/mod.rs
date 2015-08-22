/// We re-export the standard TypedArena API from here, calling it
/// `ArenaMut` since it hands back `&mut`-refs.
pub use typed_arena::Arena as ArenaMut;

use std::cell::RefCell;
use std::iter;
use std::mem;
use std::slice;
use std::vec;

/// In addition, we offer the alternative `ArenaVex` API.
///
/// `ArenaVex` provides `&`-refs, not `&mut`-refs like `ArenaMut`
/// above.
///
/// In exchange for accepting this limitation, `ArenaVex` allows one
/// to traverse it (like `Vec`, unlike `TypedArena`) and also to
/// extend it via a `&`-ref (like `TypedArena`, unlike `Vec`)
///
///  ("Vex" is a pun with the plural of `Vec`.)
pub struct ArenaVex<T> {
    chunks: RefCell<Vec<Vec<T>>>,
}

pub type ArenaVexIter<'a, T> = iter::FlatMap<vec::IntoIter<slice::Iter<'a, T>>, slice::Iter<'a, T>, s_iter_fn<'a, T>>;

#[allow(non_camel_case_types)]
type s_iter_fn<'a, T> = fn(s: slice::Iter<'a, T>) -> slice::Iter<'a, T>;

fn s_iter<'a, T>(s: slice::Iter<'a, T>) -> slice::Iter<'a, T> { s }

impl<T> ArenaVex<T> {
    pub fn new() -> ArenaVex<T> { ArenaVex::with_capacity(8) }
    pub fn with_capacity(n: usize) -> ArenaVex<T> {
        ArenaVex { chunks: RefCell::new(vec![Vec::with_capacity(n)]), }
    }

    // https://github.com/rust-lang/rust/issues/27692
    //
    // Maybe the another orthogonal (and in some ways better) answer here would be to extend our type expression syntax to allow me to actually write the type `fn even` (which currently prints as `fn(&&i32) -> bool {even}` in a explicit type instantiation like `Map<Filter<Iter<'a, i32>, fn even>, fn add1>`

    // My original idea for `ArenaVex` was that creating the iterator
    // over it would be cheap (O(1)) since the elements never
    // move. However, the piece that I missed (which may be an
    // artifact of SimonSapin's simplified version of TypedArena) is
    // that while the elements within the inner vecs never move,
    // i.e. the backing storage for the inner vecs are never
    // reallocated, the *outer* vec is certainly reallocated as the
    // arena is pushed.
    //
    // There are various ways to deal with this. For example, I could
    // switch to a linked-list representation for the outer vec.
    // (That is in fact probably the best thing to do in the long
    // term.)
    //
    // However, now I want to put in the least effort possible, so
    // continue with vec-of-vec and just clone outer vec on each iter
    // (which means each iter creation costs something like O(lg n) --
    // which in reality is probably entirely acceptable), considering
    // it is also the worst-case cost of push in this scheme.
    pub fn iter<'a>(&'a self) -> ArenaVexIter<'a, T> {
        let b = self.chunks.borrow();
        let c: Vec<_> = b.iter()
            .map(|v|v.iter())
            .collect();
        let iter = c.into_iter().flat_map(s_iter::<T> as s_iter_fn<T>);
        // these references are guaranteed to survive as long as 'a;
        // the borrow checker does not know this, because it thinks
        // the individual chunks might be dropped from the outer vec
        // at any time outside of this scope.
        //
        // worse still, mem::transmute cannot tranmute the use of type
        // param T here, so I am resorting to transmute_copy (that
        // linked list thing from above is looking better and
        // better...)
        unsafe { mem::transmute_copy::<ArenaVexIter<T>, ArenaVexIter<T>>(&iter) }
    }
    pub fn push(&self, value: T) -> &T {
        let mut chunks_borrow = self.chunks.borrow_mut();
        let next_chunk_index = chunks_borrow.len();

        let (last_child_length, last_chunk_capacity) = {
            let last_chunk = &chunks_borrow[next_chunk_index - 1];
            (last_chunk.len(), last_chunk.capacity())
        };

        let (chunk, next_item_index) = if last_child_length < last_chunk_capacity {
            (&mut chunks_borrow[next_chunk_index - 1], last_child_length)
        } else {
            let new_capacity = last_chunk_capacity.checked_mul(2).unwrap();
            chunks_borrow.push(Vec::with_capacity(new_capacity));
            (&mut chunks_borrow[next_chunk_index], 0)
        };
        chunk.push(value);
        let new_item_ref = &chunk[next_item_index];

        // Extend the lifetime from that of `chunks_borrow` to that of `self`.
        // This is OK because we’re careful to never move items
        // by never pushing to inner `Vec`s beyond their initial capacity.
        unsafe { mem::transmute::<&T, &T>(new_item_ref) }
    }
}
