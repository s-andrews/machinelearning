#pragma once
// @generated by torchgen/gen.py from DispatchKeyFunction.h

// NB: The implementing C++ file is RegisterDispatchKey.cpp

// The only #includes we need are for custom classes that have defaults in the C++ API
#include <c10/core/MemoryFormat.h>
#include <c10/core/Scalar.h>
#include <ATen/core/Reduction.h>

// Forward declarations of any types needed in the operator signatures.
// We can't directly include these classes because it will cause circular include dependencies.
// This file is included by TensorBody.h, which defines the Tensor class.
#include <ATen/core/ATen_fwd.h>

namespace at {

namespace cpu {

TORCH_API at::Tensor avg_pool2d(const at::Tensor & self, at::IntArrayRef kernel_size, at::IntArrayRef stride={}, at::IntArrayRef padding=0, bool ceil_mode=false, bool count_include_pad=true, ::std::optional<int64_t> divisor_override=::std::nullopt);
TORCH_API at::Tensor & avg_pool2d_out(at::Tensor & out, const at::Tensor & self, at::IntArrayRef kernel_size, at::IntArrayRef stride={}, at::IntArrayRef padding=0, bool ceil_mode=false, bool count_include_pad=true, ::std::optional<int64_t> divisor_override=::std::nullopt);
TORCH_API at::Tensor & avg_pool2d_outf(const at::Tensor & self, at::IntArrayRef kernel_size, at::IntArrayRef stride, at::IntArrayRef padding, bool ceil_mode, bool count_include_pad, ::std::optional<int64_t> divisor_override, at::Tensor & out);

} // namespace cpu
} // namespace at
