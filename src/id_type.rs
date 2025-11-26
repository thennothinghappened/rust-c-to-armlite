pub trait GetAndIncrement<T> {
    fn get_and_increment(&self) -> T;
}

macro_rules! id_type {
    ($name: ident) => {
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
