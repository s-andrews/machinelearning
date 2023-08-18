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
#include <c10/util/Optional.h>



#include <ATen/ops/as_strided_ops.h>

namespace at {


// aten::as_strided(Tensor(a) self, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor(a)
inline at::Tensor as_strided(const at::Tensor & self, at::IntArrayRef size, at::IntArrayRef stride, c10::optional<int64_t> storage_offset=c10::nullopt) {
    return at::_ops::as_strided::call(self, c10::fromIntArrayRef(size), c10::fromIntArrayRef(stride), storage_offset.has_value() ? c10::make_optional(c10::SymInt(*storage_offset)) : c10::nullopt);
}

// aten::as_strided(Tensor(a) self, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor(a)
inline at::Tensor as_strided_symint(const at::Tensor & self, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, c10::optional<c10::SymInt> storage_offset=c10::nullopt) {
    return at::_ops::as_strided::call(self, size, stride, storage_offset);
}

// aten::as_strided_(Tensor(a!) self, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor(a!)
inline const at::Tensor & as_strided_(const at::Tensor & self, at::IntArrayRef size, at::IntArrayRef stride, c10::optional<int64_t> storage_offset=c10::nullopt) {
    return at::_ops::as_strided_::call(self, c10::fromIntArrayRef(size), c10::fromIntArrayRef(stride), storage_offset.has_value() ? c10::make_optional(c10::SymInt(*storage_offset)) : c10::nullopt);
}

// aten::as_strided_(Tensor(a!) self, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor(a!)
inline const at::Tensor & as_strided__symint(const at::Tensor & self, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, c10::optional<c10::SymInt> storage_offset=c10::nullopt) {
    return at::_ops::as_strided_::call(self, size, stride, storage_offset);
}

}
