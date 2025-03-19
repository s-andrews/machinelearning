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


struct TORCH_API as_strided_scatter {
  using schema = at::Tensor (const at::Tensor &, const at::Tensor &, c10::SymIntArrayRef, c10::SymIntArrayRef, ::std::optional<c10::SymInt>);
  using ptr_schema = schema*;
  // See Note [static constexpr char* members for windows NVCC]
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(name, "aten::as_strided_scatter")
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(overload_name, "")
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(schema_str, "as_strided_scatter(Tensor self, Tensor src, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None) -> Tensor")
  static at::Tensor call(const at::Tensor & self, const at::Tensor & src, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, ::std::optional<c10::SymInt> storage_offset);
  static at::Tensor redispatch(c10::DispatchKeySet dispatchKeySet, const at::Tensor & self, const at::Tensor & src, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, ::std::optional<c10::SymInt> storage_offset);
};

struct TORCH_API as_strided_scatter_out {
  using schema = at::Tensor & (const at::Tensor &, const at::Tensor &, c10::SymIntArrayRef, c10::SymIntArrayRef, ::std::optional<c10::SymInt>, at::Tensor &);
  using ptr_schema = schema*;
  // See Note [static constexpr char* members for windows NVCC]
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(name, "aten::as_strided_scatter")
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(overload_name, "out")
  STATIC_CONSTEXPR_STR_INL_EXCEPT_WIN_CUDA(schema_str, "as_strided_scatter.out(Tensor self, Tensor src, SymInt[] size, SymInt[] stride, SymInt? storage_offset=None, *, Tensor(a!) out) -> Tensor(a!)")
  static at::Tensor & call(const at::Tensor & self, const at::Tensor & src, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, ::std::optional<c10::SymInt> storage_offset, at::Tensor & out);
  static at::Tensor & redispatch(c10::DispatchKeySet dispatchKeySet, const at::Tensor & self, const at::Tensor & src, c10::SymIntArrayRef size, c10::SymIntArrayRef stride, ::std::optional<c10::SymInt> storage_offset, at::Tensor & out);
};

}} // namespace at::_ops
