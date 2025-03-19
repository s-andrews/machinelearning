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

TORCH_API ::std::tuple<at::Tensor,at::Tensor> aminmax(const at::Tensor & self, ::std::optional<int64_t> dim=::std::nullopt, bool keepdim=false);
TORCH_API ::std::tuple<at::Tensor &,at::Tensor &> aminmax_out(at::Tensor & min, at::Tensor & max, const at::Tensor & self, ::std::optional<int64_t> dim=::std::nullopt, bool keepdim=false);
TORCH_API ::std::tuple<at::Tensor &,at::Tensor &> aminmax_outf(const at::Tensor & self, ::std::optional<int64_t> dim, bool keepdim, at::Tensor & min, at::Tensor & max);

} // namespace cpu
} // namespace at
