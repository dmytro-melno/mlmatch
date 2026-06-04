
#include "../mlmatch.h"

enum class axis {
    x,
    y
};

enum class polarity {
    pos,
    neg
};

template <axis axis_v, polarity polarity_v>
struct tagged_gadt(indexed) (
    (x_pos_a, (int payload), (indexed<axis::x, polarity::pos>)),
    (x_pos_b, (int payload), (indexed<axis::x, polarity::pos>)),
    (x_neg,   (int payload), (indexed<axis::x, polarity::neg>)),
    (y_pos,   (int payload), (indexed<axis::y, polarity::pos>)),
    (y_neg,   (int payload), (indexed<axis::y, polarity::neg>))
);


struct tagged(other) (
    (other_ctor, ())
);

int invalid_pattern(indexed<axis::x, polarity::pos> x) {
    // Expected: mlmatch_invalid_pattern<..., indexed<...>, other::other_ctor>.
    return match(x).with(
        [](other::other_ctor) {
            return 0;
        }
    );
}
