//! Defines IR constructs representing C's type system.

/// The set of C types.
///
/// Explicit integer types are used instead of the standard C keywords to avoid
/// confusion about the actual size of these types (e.g., `int32_t` instead of
/// `int` because `int` may or may not be 32 bits on all platforms).
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
	/// Represents the absence of a type.
	///
	/// Equivalent to C's `void` keyword.
	///
	/// Note that this is not a valid type for a variable.
	Void,

	/// Boolean type for holding values which are either `1` (`true`) or `0`
	/// (`false`).
	///
	/// Equivalent to C's `_Bool` keyword.
	Bool,

	/// Character type for holding ASCII characters.
	///
	/// Equivalent to C's `char` keyword.
	Char,

	/// 8-bit signed integer type.
	///
	/// Equivalent to `int8_t` as defined in `inttypes.h`.
	Int8,

	/// 16-bit signed integer type.
	///
	/// Equivalent to `int16_t` as defined in `inttypes.h`.
	Int16,

	/// 32-bit signed integer type.
	///
	/// Equivalent to `int32_t` as defined in `inttypes.h`.
	Int32,

	/// 64-bit signed integer type.
	///
	/// Equivalent to `int64_t` as defined in `inttypes.h`.
	Int64,

	/// 8-bit unsigned integer type.
	///
	/// Equivalent to `uint8_t` as defined in `inttypes.h`.
	UInt8,

	/// 16-bit unsigned integer type.
	///
	/// Equivalent to `uint16_t` as defined in `inttypes.h`.
	UInt16,

	/// 32-bit unsigned integer type.
	///
	/// Equivalent to `uint32_t` as defined in `inttypes.h`.
	UInt32,

	/// 64-bit unsigned integer type.
	///
	/// Equivalent to `uint64_t` as defined in `inttypes.h`.
	///
	UInt64,

	/// Single precision floating point type.
	///
	/// Equivalent to C's `float` keyword.
	Float,

	/// Double precision floating point type.
	///
	/// Equivalent to C's `double` keyword.
	Double,

	/// Pointer type.
	///
	/// Equivalent to `T *` where `T` is some type (possibly another pointer).
	Pointer(Box<Type>),

	/// Qualified type (e.g., `const T`, `volatile T`).
	Qualified(Box<Type>, Qualifier),
}

impl Type {
	/// Returns `true` if `self` is `const` qualified.
	pub fn is_const(&self) -> bool {
		match self {
			Type::Qualified(_, Qualifier::Const) => true,
			Type::Qualified(t, _) => t.is_const(),
			_ => false,
		}
	}

	/// Returns `true` if `self` is `volatile` qualified.
	pub fn is_volatile(&self) -> bool {
		match self {
			Type::Qualified(_, Qualifier::Volatile) => true,
			Type::Qualified(t, _) => t.is_volatile(),
			_ => false,
		}
	}

	/// Returns `true` if `self` is a pointer type.
	pub fn is_pointer(&self) -> bool {
		match self {
			Type::Pointer(_) => true,
			Type::Qualified(t, _) => t.is_pointer(),
			_ => false,
		}
	}

	/// Wraps `self` in a pointer type.
	pub fn into_pointer(self) -> Type {
		Type::Pointer(Box::new(self))
	}

	/// Qualifies `self` with the `const` qualifier.
	///
	/// If `self` is already `const` qualified, then this method does nothing and
	/// simply returns `self` unmodified.
	pub fn into_const(self) -> Type {
		if self.is_const() {
			self
		} else {
			Type::Qualified(Box::new(self), Qualifier::Const)
		}
	}

	/// Qualifies `self` with the `volatile` qualifier.
	///
	/// If `self` is already `volatile` qualified, then this method does nothing
	/// and simply returns `self` unmodified.
	pub fn into_volatile(self) -> Type {
		if self.is_volatile() {
			self
		} else {
			Type::Qualified(Box::new(self), Qualifier::Volatile)
		}
	}

	/// Returns a reference to the underlying type of a qualified type.
	///
	/// # Examples
	///
	/// ```rust
	/// # use cir::Type;
	/// let t = Type::Int32.into_const();
	/// assert_eq!(&Type::Int32, t.without_qualifiers());
	/// ```
	///
	/// If `self` is not qualified, then `self` is simply returned.
	///
	/// ```rust
	/// # use cir::Type;
	/// let t = Type::Int32;
	/// assert_eq!(&Type::Int32, t.without_qualifiers());
	/// ```
	pub fn without_qualifiers(&self) -> &Type {
		match self {
			Type::Qualified(t, _) => t.without_qualifiers(),
			_ => self,
		}
	}

	/// A variant of `without_qualifiers` which moves `self`.
	pub fn drop_qualifiers(self) -> Type {
		match self {
			Type::Qualified(t, _) => t.drop_qualifiers(),
			_ => self,
		}
	}
}

/// The set of type qualifiers.
///
/// TODO: `restricted` might have to be added here as well
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Qualifier {
	/// Denotes a type as constant.
	Const,

	/// Denotes a type as volatile.
	Volatile,
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn is_const() {
		let t = Type::Int32; // int32_t
		assert!(!t.is_const());

		let t = t.into_const(); // const int32_t
		assert!(t.is_const());

		let t = t.into_volatile(); // const volatile int32_t
		assert!(t.is_const());

		let t = t.into_pointer(); // const volatile int32_t *
		// While the underlying pointer type is const, the pointer variable itself
		// is not, hence this should return false.
		assert!(!t.is_const());

		let t = t.into_const(); // const volatile int32_t * const
		assert!(t.is_const());
	}

	#[test]
	fn is_volatile() {
		let t = Type::Int32; // int32_t
		assert!(!t.is_volatile());

		let t = t.into_const(); // const int32_t
		assert!(!t.is_volatile());

		let t = t.into_volatile(); // const volatile int32_t
		assert!(t.is_volatile());

		let t = t.into_pointer(); // const volatile int32_t *
		// While the underlying pointer type is volatile, the pointer variable
		// itself is not, hence this should return false.
		assert!(!t.is_volatile());

		let t = t.into_volatile(); // const volatile int32_t * volatile
		assert!(t.is_volatile());
	}

	#[test]
	fn is_pointer() {
		let t = Type::Int32; // int32_t
		assert!(!t.is_pointer());

		let t = t.into_pointer(); // int32_t *
		assert!(t.is_pointer());

		let t = t.into_const(); // int32_t * const
		assert!(t.is_pointer());

		let t = t.into_pointer(); // int32_t * const *
		assert!(t.is_pointer());
	}

	#[test]
	fn into_const() {
		let t = Type::Int32.into_const();
		let expected = Type::Qualified(Box::new(Type::Int32), Qualifier::Const);
		assert_eq!(expected, t);

		// Additional calls to `into_const` shouldn't change it's value.
		let t = t.into_const();
		assert_eq!(expected, t);
	}

	#[test]
	fn into_volatile() {
		let t = Type::Int32.into_volatile();
		let expected = Type::Qualified(Box::new(Type::Int32), Qualifier::Volatile);
		assert_eq!(expected, t);

		// Additional calls to `into_volatile` shouldn't change it's value.
		let t = t.into_volatile();
		assert_eq!(expected, t);
	}
}
