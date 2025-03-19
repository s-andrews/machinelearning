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



#include <ATen/ops/argsort_ops.h>

namespace at {


// aten::argsort(Tensor self, int dim=-1, bool descending=False) -> Tensor
inline at::Tensor argsort(const at::Tensor & self, int64_t dim=-1, bool descending=false) {
    return at::_ops::argsort::call(self, dim, descending);
}

// aten::argsort.stable(Tensor self, *, bool stable, int dim=-1, bool descending=False) -> Tensor
inline at::Tensor argsort(const at::Tensor & self, bool stable, int64_t dim=-1, bool descending=false) {
    return at::_ops::argsort_stable::call(self, stable, dim, descending);
}

// aten::argsort.stable_out(Tensor self, *, bool stable, int dim=-1, bool descending=False, Tensor(a!) out) -> Tensor(a!)
inline at::Tensor & argsort_out(at::Tensor & out, const at::Tensor & self, bool stable, int64_t dim=-1, bool descending=false) {
    return at::_ops::argsort_stable_out::call(self, stable, dim, descending, out);
}
// aten::argsort.stable_out(Tensor self, *, bool stable, int dim=-1, bool descending=False, Tensor(a!) out) -> Tensor(a!)
inline at::Tensor & argsort_outf(const at::Tensor & self, bool stable, int64_t dim, bool descending, at::Tensor & out) {
    return at::_ops::argsort_stable_out::call(self, stable, dim, descending, out);
}

// aten::argsort.dimname(Tensor self, Dimname dim, bool descending=False) -> Tensor
inline at::Tensor argsort(const at::Tensor & self, at::Dimname dim, bool descending=false) {
    return at::_ops::argsort_dimname::call(self, dim, descending);
}

}
