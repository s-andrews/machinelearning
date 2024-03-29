#pragma once

// @generated by torchgen/gen.py from enum_tag.h

namespace at {
    // Enum of valid tags obtained from the entries in tags.yaml
    enum class Tag {
        data_dependent_output,
        dynamic_output_shape,
        generated,
        inplace_view,
        nondeterministic_bitwise,
        nondeterministic_seeded,
        view_copy
    };
}
