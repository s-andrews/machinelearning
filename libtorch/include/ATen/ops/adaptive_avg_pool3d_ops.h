#pragma once

// @generated by torchgen/gen.py from Operator.h

#include <tuple>
#include <vector>

// Forward declarations of any types needed in the operator signatures.
// We can't directly include these classes because it will cause circular include dependencies.
// This file is included by TensorBody.h, which defines the Tensor class.
#include <ATen/core/ATen_fwd.h>

namespace at {
namespace _ops {


struct TORCH_API adaptive_avg_pool3d_out {
  using schema = at::Tensor & (const at::Tensor &, c10::SymIntArrayRef, at::Tensor &);
  using ptr_schema = schema*;
  // See Note [static constexpr char* members for windows NVCC]
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(name, "aten::adaptive_avg_pool3d")
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(overload_name, "out")
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(schema_str, "adaptive_avg_pool3d.out(Tensor self, SymInt[3] output_size, *, Tensor(a!) out) -> Tensor(a!)")
  static at::Tensor & call(const at::Tensor & self, c10::SymIntArrayRef output_size, at::Tensor & out);
  static at::Tensor & redispatch(c10::DispatchKeySet dispatchKeySet, const at::Tensor & self, c10::SymIntArrayRef output_size, at::Tensor & out);
};

struct TORCH_API adaptive_avg_pool3d {
  using schema = at::Tensor (const at::Tensor &, c10::SymIntArrayRef);
  using ptr_schema = schema*;
  // See Note [static constexpr char* members for windows NVCC]
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(name, "aten::adaptive_avg_pool3d")
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(overload_name, "")
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(schema_str, "adaptive_avg_pool3d(Tensor self, SymInt[3] output_size) -> Tensor")
  static at::Tensor call(const at::Tensor & self, c10::SymIntArrayRef output_size);
  static at::Tensor redispatch(c10::DispatchKeySet dispatchKeySet, const at::Tensor & self, c10::SymIntArrayRef output_size);
};

}} // namespace at::_ops
