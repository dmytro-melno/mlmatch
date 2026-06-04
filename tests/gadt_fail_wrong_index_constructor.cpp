
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


int wrong_index_constructor() {
    // Expected: constraints-not-satisfied error. x_pos_a constructs
    // indexed<axis::x, polarity::pos>, not indexed<axis::x, polarity::neg>.
    auto bad = indexed<axis::x, polarity::neg>::x_pos_a::make(1);
    return bad.tag == indexed<axis::x, polarity::neg>::tag_t::x_pos_a ? 0 : 1;
}
