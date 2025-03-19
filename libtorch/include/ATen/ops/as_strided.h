#pragma once

// @generated by torchgen/gen.py from Function.h

#include <ATen/Context.h>
#include <ATen/DeviceGuard.h>
#include <ATen/TensorUtils.h>
#include <ATen/TracerMode.h>
#include <ATen/core/Generator.h>
#include <ATen/core/Reduction.h>
#include <ATen/core/Tensor.h>
#include <c10/core/Scalar.h>
#include <c10/core/Storage.h>
#include <c10/core/TensorOptions.h>
#include <c10/util/Deprecated.h>
#include <optional>



#include <ATen/ops/as_strided_ops.h>

namespace at {


// aten::as_strided(Tensor(a) self, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor(a)
inline at::Tensor as_strided(const at::Tensor & self, at::IntArrayRef size, at::IntArrayRef stride, ::std::optional<int64_t> storage_offset=::std::nullopt) {
    return at::_ops::as_strided::call(self, c10::fromIntArrayRefSlow(size), c10::fromIntArrayRefSlow(stride), storage_offset.has_value() ? ::std::make_optional(c10::SymInt(*storage_offset)) : ::std::nullopt);
}
namespace symint {
  template <typename T, typename = std::enable_if_t<std::is_same<T, int64_t>::value>>
  at::Tensor as_strided(const at::Tensor & self, at::IntArrayRef size, at::IntArrayRef stride, ::std::optional<int64_t> storage_offset=::std::nullopt) {
    return at::_ops::as_strided::call(self, c10::fromIntArrayRefSlow(size), c10::fromIntArrayRefSlow(stride), storage_offset.has_value() ? ::std::make_optional(c10::SymInt(*storage_offset)) : ::std::nullopt);
  }
}

// aten::as_strided(Tensor(a) self, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor(a)
inline at::Tensor as_strided_symint(const at::Tensor & self, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, ::std::optional<c10::SymInt> storage_offset=::std::nullopt) {
    return at::_ops::as_strided::call(self, size, stride, storage_offset);
}
namespace symint {
  template <typename T, typename = std::enable_if_t<std::is_same<T, c10::SymInt>::value>>
  at::Tensor as_strided(const at::Tensor & self, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, ::std::optional<c10::SymInt> storage_offset=::std::nullopt) {
    return at::_ops::as_strided::call(self, size, stride, storage_offset);
  }
}

// aten::as_strided_(Tensor(a!) self, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor(a!)
inline const at::Tensor & as_strided_(const at::Tensor & self, at::IntArrayRef size, at::IntArrayRef stride, ::std::optional<int64_t> storage_offset=::std::nullopt) {
    return at::_ops::as_strided_::call(self, c10::fromIntArrayRefSlow(size), c10::fromIntArrayRefSlow(stride), storage_offset.has_value() ? ::std::make_optional(c10::SymInt(*storage_offset)) : ::std::nullopt);
}
namespace symint {
  template <typename T, typename = std::enable_if_t<std::is_same<T, int64_t>::value>>
  const at::Tensor & as_strided_(const at::Tensor & self, at::IntArrayRef size, at::IntArrayRef stride, ::std::optional<int64_t> storage_offset=::std::nullopt) {
    return at::_ops::as_strided_::call(self, c10::fromIntArrayRefSlow(size), c10::fromIntArrayRefSlow(stride), storage_offset.has_value() ? ::std::make_optional(c10::SymInt(*storage_offset)) : ::std::nullopt);
  }
}

// aten::as_strided_(Tensor(a!) self, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor(a!)
inline const at::Tensor & as_strided__symint(const at::Tensor & self, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, ::std::optional<c10::SymInt> storage_offset=::std::nullopt) {
    return at::_ops::as_strided_::call(self, size, stride, storage_offset);
}
namespace symint {
  template <typename T, typename = std::enable_if_t<std::is_same<T, c10::SymInt>::value>>
  const at::Tensor & as_strided_(const at::Tensor & self, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, ::std::optional<c10::SymInt> storage_offset=::std::nullopt) {
    return at::_ops::as_strided_::call(self, size, stride, storage_offset);
  }
}

}
