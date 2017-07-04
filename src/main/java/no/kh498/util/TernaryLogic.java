package no.kh498.util;

/**
 * A three valued logic type
 *
 * @author karl henrik
 */
@SuppressWarnings("unused")
public enum TernaryLogic {
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

    public boolean isTrue() {
        return this == TRUE;
    }
    public boolean isFalse() {
        return this == FALSE;
    }
    public boolean isUnknown() {
        return this == UNKNOWN;
    }

    /* Bitwise operators
     * Resource: https://en.wikipedia.org/wiki/Three-valued_logic#Kleene_and_Priest_logics
     */

    /**
     * Invert the
     *
     * @return {@code isKnown() ? (this == TRUE ? FALSE : TRUE) : UNKNOWN}.
     */
    public TernaryLogic not() {
        return isKnown() ? (this == TRUE ? FALSE : TRUE) : UNKNOWN;
    }

    public TernaryLogic and(final TernaryLogic other) {
        if (this == FALSE || other == FALSE) {
            return FALSE;
        }
        else if (this == TRUE && other == TRUE) {
            return TRUE;
        }
        return UNKNOWN;
    }

    public TernaryLogic or(final TernaryLogic other) {
        if (this == TRUE || other == TRUE) {
            return TRUE;
        }
        else if (this == FALSE && other == FALSE) {
            return FALSE;
        }
        return UNKNOWN;
    }
    /* END Bitwise operators */

    /**
     * Get the corresponding {@code Ternary} state from an integer
     *
     * @param i The integer to convert
     *
     * @return {@code (i == 1) ? TRUE : ((i == 0) ? FALSE : UNKNOWN)}
     */
    public static TernaryLogic get(final int i) {
        return (i == 1) ? TRUE : ((i == 0) ? FALSE : UNKNOWN);
    }

    /**
     * Get the corresponding {@code Ternary} state from a float
     *
     * @param f The float to convert to {@code Ternary}
     *
     * @return {@code (i == 1) ? TRUE : ((i == 0) ? FALSE : UNKNOWN)}
     */
    public static TernaryLogic get(final float f) {
        return (f == 1f) ? TRUE : ((f == 0f) ? FALSE : UNKNOWN);
    }

    /**
     * Get the corresponding {@code Ternary} state from a double
     *
     * @param d The double to convert to {@code Ternary}
     *
     * @return {@code (i == 1) ? TRUE : ((i == 0) ? FALSE : UNKNOWN)}
     */
    public static TernaryLogic get(final double d) {
        return (d == 1d) ? TRUE : ((d == 0d) ? FALSE : UNKNOWN);
    }

    /**
     * Get the corresponding {@code Ternary} state from an integer
     *
     * @param b The boolean to convert to {@code Ternary}
     *
     * @return {@code b ? TRUE : FALSE}
     */
    public static TernaryLogic get(final boolean b) {
        return b ? TRUE : FALSE;
    }

    /**
     * Get the corresponding {@code Ternary} state from an integer
     *
     * @param s The boolean to convert to {@code Ternary}, case insensitive
     *
     * @return {@code "TRUE".equalsIgnoreCase(s) ? TRUE : ("FALSE".equalsIgnoreCase(s) ? FALSE : UNKNOWN)}
     */
    public static TernaryLogic get(final String s) {
        return "TRUE".equalsIgnoreCase(s) ? TRUE : ("FALSE".equalsIgnoreCase(s) ? FALSE : UNKNOWN);
    }
}
