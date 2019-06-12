use std::alloc::{Alloc, Global};
use std::cell::UnsafeCell;
use std::mem;
use std::ptr::{NonNull, Unique};

pub struct RawVec<T, A: Alloc = Global> {
    ptr: Unique<T>,
    cap: usize,
    a: A,
}

impl<T> RawVec<T> {
    pub fn new() -> Self {
        Self::new_in(Global)
    }
}

impl<T, A: Alloc> RawVec<T, A> {
    pub fn new_in(a: A) -> Self {
        assert!(mem::size_of::<T>() != 0, "TODO: implement ZST support");
        RawVec {
            ptr: Unique::empty(),
            cap: 0,
            a: a,
        }
    }

    pub fn grow(&mut self) {
        unsafe {
            let (new_cap, ptr) = if self.cap == 0 {
                (1, self.a.alloc_array(1))
            } else {
                let new_cap = 2 * self.cap;
                (
                    new_cap * self.cap,
                    self.a.realloc_array(self.ptr.into(), self.cap, new_cap),
                )
            };

            let ptr = ptr.unwrap_or_else(|_| std::process::exit(24));

            self.ptr = ptr.into();
            self.cap = new_cap;
        }
    }

    pub fn ptr(&self) -> NonNull<T> {
        self.ptr.into()
    }

    pub fn cap(&self) -> usize {
        self.cap
    }
}

impl<T, A: Alloc> Drop for RawVec<T, A> {
    fn drop(&mut self) {
        if self.cap != 0 {
            unsafe {
                self.a.dealloc_array(self.ptr.into(), self.cap).unwrap();
            }
        }
    }
}

pub struct MutVec<T> {
    data: RawVec<T>,
    len: usize,
}

impl<T> MutVec<T> {
    pub fn new() -> Self {
        Self {
            data: RawVec::new(),
            len: 0,
        }
    }

    pub fn push(&self, t: T) {
        let ptr = self.data.ptr();

        if self.len == self.data.cap() {
            unsafe {
                let data: *mut RawVec<T> = &self.data as *const RawVec<T> as *mut RawVec<T>;
                (&mut *data).grow()
            }
        }

        unsafe {
            std::ptr::write(ptr.as_ptr().offset(self.len as isize), t);
        }
    }

    pub fn get(&self, i: usize) -> Option<*const T> {
        unsafe {
            if i >= self.len {
                return None;
            }

            let ptr = self.data.ptr();
            Some(ptr.as_ptr().offset(i as isize))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grow() {
        let mut raw: RawVec<u64> = RawVec::new();
        raw.grow();
        let cap = raw.cap();
        raw.grow();

        assert_eq!(cap * 2, raw.cap())
    }

    #[test]
    fn test_grow_access() {
        let mut raw = RawVec::new();
        raw.grow();

        let ptr = raw.ptr.as_ptr();

        unsafe { std::ptr::write(ptr, 10u64) }
    }

}
