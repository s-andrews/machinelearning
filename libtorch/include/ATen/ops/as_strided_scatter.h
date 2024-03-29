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



#include <ATen/ops/as_strided_scatter_ops.h>

namespace at {


// aten::as_strided_scatter(Tensor self, Tensor src, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor
inline at::Tensor as_strided_scatter(const at::Tensor & self, const at::Tensor & src, at::IntArrayRef size, at::IntArrayRef stride, c10::optional<int64_t> storage_offset=c10::nullopt) {
    return at::_ops::as_strided_scatter::call(self, src, c10::fromIntArrayRef(size), c10::fromIntArrayRef(stride), storage_offset.has_value() ? c10::make_optional(c10::SymInt(*storage_offset)) : c10::nullopt);
}

// aten::as_strided_scatter(Tensor self, Tensor src, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor
inline at::Tensor as_strided_scatter_symint(const at::Tensor & self, const at::Tensor & src, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, c10::optional<c10::SymInt> storage_offset=c10::nullopt) {
    return at::_ops::as_strided_scatter::call(self, src, size, stride, storage_offset);
}

// aten::as_strided_scatter.out(Tensor self, Tensor src, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None, *, Tensor(a!) out) -> Tensor(a!)
inline at::Tensor & as_strided_scatter_out(at::Tensor & out, const at::Tensor & self, const at::Tensor & src, at::IntArrayRef size, at::IntArrayRef stride, c10::optional<int64_t> storage_offset=c10::nullopt) {
    return at::_ops::as_strided_scatter_out::call(self, src, c10::fromIntArrayRef(size), c10::fromIntArrayRef(stride), storage_offset.has_value() ? c10::make_optional(c10::SymInt(*storage_offset)) : c10::nullopt, out);
}

// aten::as_strided_scatter.out(Tensor self, Tensor src, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None, *, Tensor(a!) out) -> Tensor(a!)
inline at::Tensor & as_strided_scatter_outf(const at::Tensor & self, const at::Tensor & src, at::IntArrayRef size, at::IntArrayRef stride, c10::optional<int64_t> storage_offset, at::Tensor & out) {
    return at::_ops::as_strided_scatter_out::call(self, src, c10::fromIntArrayRef(size), c10::fromIntArrayRef(stride), storage_offset.has_value() ? c10::make_optional(c10::SymInt(*storage_offset)) : c10::nullopt, out);
}

// aten::as_strided_scatter.out(Tensor self, Tensor src, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None, *, Tensor(a!) out) -> Tensor(a!)
inline at::Tensor & as_strided_scatter_symint_out(at::Tensor & out, const at::Tensor & self, const at::Tensor & src, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, c10::optional<c10::SymInt> storage_offset=c10::nullopt) {
    return at::_ops::as_strided_scatter_out::call(self, src, size, stride, storage_offset, out);
}

// aten::as_strided_scatter.out(Tensor self, Tensor src, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None, *, Tensor(a!) out) -> Tensor(a!)
inline at::Tensor & as_strided_scatter_symint_outf(const at::Tensor & self, const at::Tensor & src, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, c10::optional<c10::SymInt> storage_offset, at::Tensor & out) {
    return at::_ops::as_strided_scatter_out::call(self, src, size, stride, storage_offset, out);
}

}
