pub trait GetAndIncrement<T> {
    fn get_and_increment(&self) -> T;
}

// Note: to get docs working on ID types, grabbed that from here:
// https://amanjeev.com/blog/rust-document-macro-invocations/
macro_rules! id_type {
    (
        $(#[$meta:meta])*
        $name: ident
    ) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name(u32);

        impl $name {
            pub fn next(self) -> Self {
                Self(self.0 + 1)
            }

            pub fn get_and_increment(&mut self) -> Self {
                let current = *self;
                *self = Self(current.0 + 1);

                current
            }

            pub fn value(self) -> u32 {
                self.0
            }
        }

        impl Default for $name {
            fn default() -> Self {
                Self(0)
            }
        }

        impl crate::id_type::GetAndIncrement<$name> for std::cell::Cell<$name> {
            fn get_and_increment(&self) -> $name {
                let current = self.get();
                self.set(current.next());

                current
            }
        }
    };
}
