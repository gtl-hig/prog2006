
/**
 * All possible errors as a bit flag so we can combine them easily
 */
enum ERROR_FLAGS {
    NAME_SHORT = 1U << 1U,
    NAME_NON_CAPITALIZED = 1U << 2U,
    NAME_INVALID_LETTER = 1U << 3U,
    SURNAME_SHORT = 1U << 4U,
    SURNAME_NON_CAPITALIZED = 1U << 5U,
    SURNAME_INVALID_LETTER = 1U << 6U,
    AGE_LOW = 1U << 7U,
    AGE_HIGH = 1U << 8U,
};

int main() {
    return 0;
}
