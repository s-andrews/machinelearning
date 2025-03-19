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

namespace cuda {

TORCH_API at::Tensor as_strided(const at::Tensor & self, at::IntArrayRef size, at::IntArrayRef stride, ::std::optional<int64_t> storage_offset=::std::nullopt);
TORCH_API at::Tensor as_strided_symint(const at::Tensor & self, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, ::std::optional<c10::SymInt> storage_offset=::std::nullopt);

} // namespace cuda
} // namespace at
