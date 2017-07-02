package no.kh498.util;

/**
 * A three way logic operator.
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
}
