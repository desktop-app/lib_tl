// This file is part of Desktop App Toolkit,
// a set of libraries for developing nice desktop applications.
//
// For license and copyright information please follow this link:
// https://github.com/desktop-app/legal/blob/master/LEGAL
//
#pragma once

#include "base/basic_types.h"

#include <QtCore/QVector>

namespace tl {

template <typename bare>
class boxed : public bare {
public:
	using bare::bare;

	boxed() = default;
	boxed(const boxed<bare> &v) = default;
	boxed<bare> &operator=(const boxed<bare> &v) = default;
	boxed(const bare &v) : bare(v) {
	}
	boxed<bare> &operator=(const bare &v) {
		*((bare*)this) = v;
		return *this;
	}

	uint32 innerLength() const {
		return sizeof(uint32) + bare::innerLength();
	}
	[[nodiscard]] bool read(const int32 *&from, const int32 *end, uint32 cons = 0) {
		if (from + 1 > end) {
			return false;
		}
		cons = static_cast<uint32>(*(from++));
		return bare::read(from, end, cons);
	}
	void write(QVector<int32> &to) const {
		to.push_back(bare::type());
		bare::write(to);
	}

	using Unboxed = bare;

};

template <typename T>
class boxed<boxed<T> > {
	using Unboxed = typename T::CantMakeBoxedBoxedType;
};

template <typename T>
struct is_boxed : std::false_type {
};

template <typename T>
struct is_boxed<boxed<T>> : std::true_type {
};

template <typename T>
inline constexpr bool is_boxed_v = is_boxed<T>::value;

} // namespace tl
