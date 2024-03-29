#pragma once

// @generated by torchgen/gen.py from NativeFunction.h

#include <c10/core/Scalar.h>
#include <c10/core/Storage.h>
#include <c10/core/TensorOptions.h>
#include <c10/util/Deprecated.h>
#include <c10/util/Optional.h>
#include <c10/core/QScheme.h>
#include <ATen/core/Reduction.h>
#include <ATen/core/Tensor.h>
#include <tuple>
#include <vector>


namespace at {
namespace native {
TORCH_API at::Tensor as_strided_scatter_symint(const at::Tensor & self, const at::Tensor & src, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, c10::optional<c10::SymInt> storage_offset=c10::nullopt);
TORCH_API at::Tensor & as_strided_scatter_out_symint(const at::Tensor & self, const at::Tensor & src, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, c10::optional<c10::SymInt> storage_offset, at::Tensor & out);
} // namespace native
} // namespace at
