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
TORCH_API ::std::tuple<at::Tensor &,at::Tensor &> batch_norm_gather_stats_with_counts_out(const at::Tensor & input, const at::Tensor & mean, const at::Tensor & invstd, const c10::optional<at::Tensor> & running_mean, const c10::optional<at::Tensor> & running_var, double momentum, double eps, const at::Tensor & counts, at::Tensor & out0, at::Tensor & out1);
TORCH_API ::std::tuple<at::Tensor,at::Tensor> batch_norm_gather_stats_with_counts_cuda(const at::Tensor & input, const at::Tensor & mean, const at::Tensor & invstd, const c10::optional<at::Tensor> & running_mean, const c10::optional<at::Tensor> & running_var, double momentum, double eps, const at::Tensor & counts);
} // namespace native
} // namespace at
