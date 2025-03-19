#pragma once

// @generated by torchgen/gen.py from NativeFunction.h

#include <c10/core/Scalar.h>
#include <c10/core/Storage.h>
#include <c10/core/TensorOptions.h>
#include <c10/util/Deprecated.h>
#include <optional>
#include <c10/core/QScheme.h>
#include <ATen/core/Reduction.h>
#include <ATen/core/Tensor.h>
#include <tuple>
#include <vector>


namespace at {
namespace native {
TORCH_API at::Tensor adaptive_avg_pool2d_symint(const at::Tensor & self, c10::SymIntArrayRef output_size);
TORCH_API at::Tensor & adaptive_avg_pool2d_out_cpu(const at::Tensor & self, at::IntArrayRef output_size, at::Tensor & out);
TORCH_API at::Tensor & adaptive_avg_pool2d_out_cuda(const at::Tensor & self, at::IntArrayRef output_size, at::Tensor & out);
TORCH_API at::Tensor & mkldnn_adaptive_avg_pool2d_out_stub(const at::Tensor & self, at::IntArrayRef output_size, at::Tensor & out);
} // namespace native
} // namespace at
