package no.kh498.util;

/**
 * A three valued logic type
 *
 * @author karl henrik
 */
@SuppressWarnings("unused")
public enum Ternary {
    TRUE,
    UNKNOWN,
    FALSE;

    /**
     * @return whenever the state is {@code TRUE} or {@code UNKNOWN}
     */
    public boolean isNotFalse() {
        return this != FALSE;
    }

    /**
     * @return Return whenever the state is {@code FALSE} or {@code UNKNOWN}
     */
    public boolean isNotTrue() {
        return this != TRUE;
    }

    /**
     * @return Return whenever the value if known, {@code FALSE} or {@code TRUE}
     */
    public boolean isKnown() {
        return this != UNKNOWN;
    }

    /**
     * Get the corresponding {@code Ternary} state from an integer
     *
     * @param i The integer to convert
     *
     * @return {@code (i == 1) ? TRUE : ((i == 0) ? FALSE : UNKNOWN)}
     */
    public static Ternary get(final int i) {
        return (i == 1) ? TRUE : ((i == 0) ? FALSE : UNKNOWN);
    }

    /**
     * Get the corresponding {@code Ternary} state from a float
     *
     * @param f The float to convert to {@code Ternary}
     *
     * @return {@code (i == 1) ? TRUE : ((i == 0) ? FALSE : UNKNOWN)}
     */
    public static Ternary get(final float f) {
        return (f == 1f) ? TRUE : ((f == 0f) ? FALSE : UNKNOWN);
    }

    /**
     * Get the corresponding {@code Ternary} state from a double
     *
     * @param d The double to convert to {@code Ternary}
     *
     * @return {@code (i == 1) ? TRUE : ((i == 0) ? FALSE : UNKNOWN)}
     */
    public static Ternary get(final double d) {
        return (d == 1d) ? TRUE : ((d == 0d) ? FALSE : UNKNOWN);
    }

    /**
     * Get the corresponding {@code Ternary} state from an integer
     *
     * @param b The boolean to convert to {@code Ternary}
     *
     * @return {@code b ? TRUE : FALSE}
     */
    public static Ternary get(final boolean b) {
        return b ? TRUE : FALSE;
    }

    /**
     * Get the corresponding {@code Ternary} state from an integer
     *
     * @param s The boolean to convert to {@code Ternary}, case insensitive
     *
     * @return {@code "TRUE".equalsIgnoreCase(s) ? TRUE : ("FALSE".equalsIgnoreCase(s) ? FALSE : UNKNOWN)}
     */
    public static Ternary get(final String s) {
        return "TRUE".equalsIgnoreCase(s) ? TRUE : ("FALSE".equalsIgnoreCase(s) ? FALSE : UNKNOWN);
    }
}
