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



#include <ATen/ops/argmin_ops.h>

namespace at {


// aten::argmin(Tensor self, int? dim=None, bool keepdim=False) -> Tensor
inline at::Tensor argmin(const at::Tensor & self, c10::optional<int64_t> dim=c10::nullopt, bool keepdim=false) {
    return at::_ops::argmin::call(self, dim, keepdim);
}

// aten::argmin.out(Tensor self, int? dim=None, bool keepdim=False, *, Tensor(a!) out) -> Tensor(a!)
inline at::Tensor & argmin_out(at::Tensor & out, const at::Tensor & self, c10::optional<int64_t> dim=c10::nullopt, bool keepdim=false) {
    return at::_ops::argmin_out::call(self, dim, keepdim, out);
}

// aten::argmin.out(Tensor self, int? dim=None, bool keepdim=False, *, Tensor(a!) out) -> Tensor(a!)
inline at::Tensor & argmin_outf(const at::Tensor & self, c10::optional<int64_t> dim, bool keepdim, at::Tensor & out) {
    return at::_ops::argmin_out::call(self, dim, keepdim, out);
}

}
