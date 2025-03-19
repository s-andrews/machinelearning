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



#include <ATen/ops/atleast_3d_ops.h>

namespace at {


// aten::atleast_3d(Tensor self) -> Tensor
inline at::Tensor atleast_3d(const at::Tensor & self) {
    return at::_ops::atleast_3d::call(self);
}

// aten::atleast_3d.Sequence(Tensor[] tensors) -> Tensor[]
inline ::std::vector<at::Tensor> atleast_3d(at::TensorList tensors) {
    return at::_ops::atleast_3d_Sequence::call(tensors);
}

}
