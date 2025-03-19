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
#include <optional>



#include <ATen/ops/amin_ops.h>

namespace at {


// aten::amin(Tensor self, int[1] dim=[], bool keepdim=False) -> Tensor
inline at::Tensor amin(const at::Tensor & self, at::IntArrayRef dim={}, bool keepdim=false) {
    return at::_ops::amin::call(self, dim, keepdim);
}

// aten::amin.out(Tensor self, int[1] dim=[], bool keepdim=False, *, Tensor(a!) out) -> Tensor(a!)
inline at::Tensor & amin_out(at::Tensor & out, const at::Tensor & self, at::IntArrayRef dim={}, bool keepdim=false) {
    return at::_ops::amin_out::call(self, dim, keepdim, out);
}
// aten::amin.out(Tensor self, int[1] dim=[], bool keepdim=False, *, Tensor(a!) out) -> Tensor(a!)
inline at::Tensor & amin_outf(const at::Tensor & self, at::IntArrayRef dim, bool keepdim, at::Tensor & out) {
    return at::_ops::amin_out::call(self, dim, keepdim, out);
}

}
