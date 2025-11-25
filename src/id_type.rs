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
    };
}
